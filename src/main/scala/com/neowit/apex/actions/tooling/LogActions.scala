package com.neowit.apex.actions.tooling

import java.io.File

import com.neowit.apex.{LogUtils, Session}
import com.neowit.apex.actions.SoqlQuery.ResultRecord
import com.neowit.apex.actions._
import com.neowit.response.ResponseWriter.{InfoMessage, KeyValueMessage, SUCCESS}
import com.neowit.utils.{ConfigValueException, FileUtils}
import com.sforce.soap.tooling._

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

object LogActions {

    def getLog(session:Session, logId: String): String = {
        session.getRestContentTooling("/sobjects/ApexLog/"+logId+"/Body/", "") match {
            case Some(log) => log
            case None => ""
        }

    }

    /**
     * retrieve Last log Id for current User
     */
    def getLastLogId(session:Session): Option[String] = {
        val userId = session.getUserId
        val queryResult = session.queryTooling("select Id from ApexLog where LogUserId = '" + userId + "' and Request = 'API' order by StartTime Desc limit 1")
        val records = queryResult.getRecords
        if (records.nonEmpty) {
            Some(records.head.getId)
        } else {
            None
        }
    }
}

object ChangeLogLevels {
    /**
     * - To set up a log for a specific user, set TracedEntityId to the ID of the user.
     *   This option can only be configured for a user, not an Apex class or Apex trigger.
     *
     * - To configure logging levels for system logs (visible only to you), set TracedEntityId
     *   to the ID of the logged-in user.
     *
     * - To set up a system log (visible only to you) for a specific Apex class or trigger, set
      *  TracedEntityId to the ID of the Apex class or trigger.
     *
     * @param traceFlagConfigPath - Option[path to temp file with trace flag json]
     * @return trace flag Id
     */
    def setupTraceFlag(traceFlagConfigPath: Option[String], session: Session, logger: com.neowit.utils.Logging,
                       logType: Option[String], tracedEntityId: String): Try[String] = {
        val action = new ChangeLogLevels().load[ChangeLogLevels](session)
        action.setupTrace(loadTraceFlagConfig(traceFlagConfigPath, session), session, logger, logType, tracedEntityId)
    }
    def deleteTraceFlag(traceFlagId: String, session: Session, logger: com.neowit.utils.Logging): Unit = {
        session.deleteTooling(traceFlagId)
    }

    private def loadTraceFlagConfig(traceFlagConfigPath: Option[String], session: Session): Map[String, String] = {
        val defaultMap = Map("ApexCode" -> "Debug", "ApexProfiling"-> "Error", "Callout"-> "Error", "Database"-> "Error", "System"-> "Error", "Validation"-> "Error", "Visualforce"-> "Error", "Workflow"-> "Error")
        LogUtils.loadDebugConfig(traceFlagConfigPath, session, "traceFlagConfig", defaultMap)
    }


}
class ChangeLogLevels extends ApexActionWithReadOnlySession {
    override def getHelp: ActionHelp = new ActionHelp {
        override def getExample: String = ""
        override def getParamDescription(paramName: String): String = paramName match {
            case "traceFlagConfig" =>
                """--traceFlagConfig=/full/path/to/traceflag.conf
                  |  traceflag.conf must contain a JSON object with "type":"value" pairs.
                  |  All log types [ApexCode, ApexProfiling, ... Workflow] must be specified.
                  |  File content format is as follows:
                  |  "{"ApexCode": "Debug", "ApexProfiling": "Error", "Callout": "Error", "Database": "Error", "System": "Error", "Validation": "Error", "Visualforce": "Error", "Workflow": "Error"}"
                """.stripMargin
            case "logType" =>
                """--logType=CLASS_TRACING|DEVELOPER_LOG|USER_DEBUG
                  |  This parameter is Optional, if not provided then 'USER_DEBUG' is used
                  |  see TraceFlag.LogType in Tooling API documentation
                  |  e.g. --logType=DEVELOPER_LOG
                """.stripMargin
            case "tracedEntity" =>
                """--tracedEntity=username|classname|triggername|<empty>
                  |  This parameter is Optional, defaults to current user
                  |
                  |  e.g. --tracedEntity=john@acme.com
                  |      will set log levels for specific existing user
                  |  e.g. --tracedEntity=MyClass.cls
                  |      will set log levels for specific class
                  |  e.g. --tracedEntity=MyTrigger.trigger
                  |      will set log levels for specific trigger
                """.stripMargin
        }

        override def getParamNames: List[String] = List("traceFlagConfig", "logType", "tracedEntity")

        override def getSummary: String = "Setup TraceFlag that triggers an Apex debug log at the specified logging level"

        override def getName: String = "changeLogLevels"
    }

    //this method should implement main logic of the action
    override protected def act()(implicit ec: ExecutionContext): Future[ActionResult] = {
        val traceFlagMap = loadTraceFlagConfig

        val tracedEntityId = getTracedEntityId match {
            case Success(entityId) =>
                entityId
            case Failure(e) =>
                //messages += new ResponseWriter.Message(ResponseWriter.ERROR, e.getMessage)
                throw e
        }
        setupTrace(traceFlagMap, session, logger, config.getProperty("logType"), tracedEntityId) match {
            case Success(traceFlagId) =>
                //responseWriter.println(SUCCESS)
                Future.successful(ActionSuccess())
            case Failure(e) =>
                //responseWriter.println(FAILURE)
                //responseWriter.println(ErrorMessage(e.getMessage))
                Future.successful(ActionFailure(e.getMessage))

        }
    }

    private def loadTraceFlagConfig: Map[String, String] = {
        val traceFlagMap: Map[String, String] = ChangeLogLevels.loadTraceFlagConfig(config.getRequiredProperty("traceFlagConfig"), session)
        traceFlagMap
    }

    /**
     * traceFlagConfig={"ApexProfiling" : "Error", "Validation" : "Error", "Database" : "Error", "Workflow" : "Error",
     *                  "ApexCode" : "Debug", "System" : "Error", "Visualforce" : "Error", "Callout" : "Debug"}

     * @param traceFlag flag to save - not currently used
     * @param traceFlagMap flag config
     */
    private def saveTraceFlagConfig(traceFlag: TraceFlag, traceFlagMap: Map[String, String]) = {
        //TODO consider saving with TracedEntity taken into account
        session.setData("traceFlagConfig", traceFlagMap)
    }
    private val DEBUG_LEVEL_DEVELOPER_NAME = "TOOLING_FORCE_DOT_COM"

    def createDebugLevel(traceFlagMap: Map[String, String]): Try[String] = {
        logger.debug("Setup DebugLevel")
        val debugLevel = new DebugLevel with DebugLevelLike
        debugLevel.setDeveloperName(DEBUG_LEVEL_DEVELOPER_NAME)
        debugLevel.setMasterLabel(DEBUG_LEVEL_DEVELOPER_NAME)
        traceFlagMap.foreach{
            case (logLevelType, logLevel) => setLogLevelByType(logLevelType, logLevel, debugLevel)
        }
        val saveResults: Array[SaveResult] = session.createTooling(Array(debugLevel))
        if (saveResults.nonEmpty && saveResults.head.isSuccess) {
            //saveTraceFlagConfig(traceFlag, traceFlagMap)
            Success(saveResults.head.getId)
        } else {
            val errors = saveResults.head.getErrors
            if (null != errors && errors.nonEmpty) {
                    logger.error("Failed to setup TraceFlag: " + errors.head.getMessage)
                    Failure( new RuntimeException("Failed to setup DebugLevel: " + errors.head.getMessage) )
            }
            Failure( new RuntimeException("Failed to setup DebugLevel. Unknown error") )
        }
    }

    def getDebugLevel: Option[DebugLevel] = {
        val queryResult = session.queryTooling(s"select Id, ApexCode, ApexProfiling, Callout, Database, System, Validation, Visualforce, Workflow from DebugLevel where DeveloperName = '$DEBUG_LEVEL_DEVELOPER_NAME'")
        val records = queryResult.getRecords
        if (records.nonEmpty) {
            Some(records.head.asInstanceOf[DebugLevel])
        } else {
            None
        }
    }

    private def isSameConfig(debugLevel: DebugLevel, traceFlagMap: Map[String, String]): Boolean = {
        def check(mapName: String, dbValue: String): Boolean = {
            val dbValueNotNull = if (null == dbValue) "" else dbValue
            traceFlagMap.getOrElse(mapName, "").toLowerCase == dbValueNotNull.toLowerCase
        }
        //ApexCode, ApexProfiling, Callout, Database, System, Validation, Visualforce, Workflow
        check("ApexCode", debugLevel.getApexCode) &&
            check("ApexProfiling", debugLevel.getApexProfiling) &&
            check("Callout", debugLevel.getCallout) &&
            check("Database", debugLevel.getDatabase) &&
            check("System", debugLevel.getSystem) &&
            check("Validation", debugLevel.getValidation) &&
            check("Visualforce", debugLevel.getVisualforce) &&
            check("Workflow", debugLevel.getWorkflow)
    }

    def setupTrace(traceFlagMap: Map[String, String], session:Session, logger: com.neowit.utils.Logging,
                   logType: Option[String], tracedEntityId: String): Try[String] = {

        logger.debug("Setup TraceFlag")

        val traceFlag = new TraceFlag with DebugLevelLike
        //as of Winter 16 there is no way to create TraceFlag without DebugLevelId
        // debug level values set on DebugLevel (e.g. Apexcode=DEBUG, ApexProfiling=INFO, etc) override those set on Trace Flag
        // in other words, as of Winter 16 debug levels set on TraceFlag are no longer relevant, only those set on DebugLevel record are.
        // so 3 lines below are no longer needed
        //traceFlagMap.foreach{
        //    case (logLevelType, logLevel) => setLogLevelByType(logLevelType, logLevel, traceFlag)
        //}

        traceFlag.setTracedEntityId(tracedEntityId)
        traceFlag.setLogType(logType.getOrElse("USER_DEBUG"))

        getDebugLevel match {
            case Some(debugLevel) if isSameConfig(debugLevel, traceFlagMap) =>
                // this is same config we need, no need to create DebugLevel again
                traceFlag.setDebugLevelId(debugLevel.getId)
            case Some(debugLevel) if !isSameConfig(debugLevel, traceFlagMap) =>
                // debug level exists but its config does not match traceFlagMap
                session.deleteTooling(debugLevel.getId)
                createDebugLevel(traceFlagMap).toOption.foreach(traceFlag.setDebugLevelId)
            case None => // no debug level exists, create one
                createDebugLevel(traceFlagMap).toOption.foreach(traceFlag.setDebugLevelId)
        }

        //check if trace for current tracedEntity is already setup and expires too early, and delete it if one found
        val queryIterator = SoqlQuery.getQueryIteratorTooling(session,
                                        s""" select Id from TraceFlag
                                           |where TracedEntityId = '${traceFlag.getTracedEntityId}'
                                           |""".stripMargin)
        if (queryIterator.hasNext) {
            val recordIds = queryIterator.map(obj => new ResultRecord(obj).getFieldAsString("Id")).map(_.get).toArray
            session.deleteTooling(recordIds)
        }

        val calendar = session.getServerTimestamp.getTimestamp
        //set TTL for TraceFlag same as for Server
        calendar.add(java.util.Calendar.SECOND, session.getConfig.getProperty("timeoutSec").getOrElse("30").toInt)

        traceFlag.setExpirationDate(calendar)

        val saveResults: Array[SaveResult] = session.createTooling(Array(traceFlag))
        if (saveResults.nonEmpty && saveResults.head.isSuccess) {
            saveTraceFlagConfig(traceFlag, traceFlagMap)
            Success(saveResults.head.getId)
        } else {
            val errors = saveResults.head.getErrors
            if (null != errors && errors.nonEmpty) {
                if (errors.head.getMessage.contains("This entity is already being traced")) {
                    //
                    logger.error("Failed to setup new TraceFlag: " + errors.head.getMessage)
                    throw new RuntimeException("Failed to setup new TraceFlag: " + errors.head.getMessage)
                } else {
                    logger.error("Failed to setup TraceFlag: " + errors.head.getMessage)
                    throw new RuntimeException("Failed to setup TraceFlag: " + errors.head.getMessage)
                }
            }
            throw new RuntimeException("Failed to setup TraceFlag. Unknown error")
        }
    }

    private def getTracedEntityId: Try[String] = {
        config.getProperty("tracedEntity") match {
            case Some(x) =>
                FileUtils.getExtension(x.toLowerCase) match {
                    case "trigger" =>
                        val iterator = SoqlQuery.getQueryIteratorTooling(session, s"select Id from ApexTrigger where Name = '${FileUtils.removeExtension(x)}'")
                        if (iterator.hasNext) {
                            //TODO check if id is returned correctly
                            Success(new ResultRecord(iterator.next()).getFieldAsString("Id").get)
                        } else {
                            throw new ConfigValueException(s"Trigger with name $x not found")
                        }
                    case "class" =>
                        val iterator = SoqlQuery.getQueryIteratorTooling(session, s"select Id from ApexClass where Name = '${FileUtils.removeExtension(x)}'")
                        if (iterator.hasNext) {
                            //TODO check if id is returned correctly
                            Success(new ResultRecord(iterator.next()).getFieldAsString("Id").get)
                        } else {
                            throw new ConfigValueException(s"Class with name $x not found")
                        }
                    case username =>
                        val iterator = SoqlQuery.getQueryIteratorTooling(session, s"select Id from User where UserName = '$x'")
                        if (iterator.hasNext) {
                            //TODO check if id is returned correctly
                            Success(new ResultRecord(iterator.next()).getFieldAsString("Id").get)
                        } else {
                            throw new ConfigValueException(s"User with UserName $x not found")
                        }

                }
            case None =>
                //Some(session.getUserId)
                Success(session.getUserId)
        }
    }

    trait DebugLevelLike {
        def setApexCode(level: String): Unit
        def setApexProfiling(level: String): Unit
        def setCallout(level: String): Unit
        def setDatabase(level: String): Unit
        def setSystem(level: String): Unit
        def setValidation(level: String): Unit
        def setVisualforce(level: String): Unit
        def setWorkflow(level: String): Unit
    }
    private def setLogLevelByType(logType: String, logLevel: String, traceFlag: DebugLevelLike) = {
        //default - DB & Apex = Debug
        logType match {
            case "ApexCode" =>
                traceFlag.setApexCode(logLevel)
            case "ApexProfiling" =>
                traceFlag.setApexProfiling(logLevel)
            case "Callout" =>
                traceFlag.setCallout(logLevel)
            case "Database" =>
                traceFlag.setDatabase(logLevel)
            case "System" =>
                traceFlag.setSystem(logLevel)
            case "Validation" =>
                traceFlag.setValidation(logLevel)
            case "Visualforce" =>
                traceFlag.setVisualforce(logLevel)
            case "Workflow" =>
                traceFlag.setWorkflow(logLevel)
            case _ =>
        }

    }
}
class ListLogs extends ApexActionWithReadOnlySession {
    override def getHelp: ActionHelp = new ActionHelp {
        override def getExample: String = ""

        override def getParamDescription(paramName: String): String = paramName match {
            case "location" =>
                """--location=Monitoring|SystemLog
                   |  OPTIONAL, If not specified then logs for all locations are returned
                   |  - Monitoring — Generated as part of debug log monitoring and visible to all administrators.
                   |  These types of logs are maintained until the user or the system overwrites them.
                   |  - SystemLog — Generated as part of system log monitoring and visible only to you.
                   |  These types of logs are only maintained for 60 minutes or until the user clears them.
                """.stripMargin
            case "logUserName" =>
                """--logUserName=username
                  |  OPTIONAL, If not specified then defaults to current user
                  |  Username of the user whose actions triggered the debug log or heap dump.
                  |  e.g. --logUserName=admin@acme.com
                  |
                """.stripMargin
            case "request" =>
                """--request=API|Application
                  |  OPTIONAL, If not specified then logs for all request types are returned
                  |  - API — Request came from an API.
                  |  - Application — Request came from the Salesforce user interface.
                  |
                """.stripMargin
        }

        override def getParamNames: List[String] = List("location", "logUserName", "request")

        override def getSummary: String = "Setup TraceFlag that triggers an Apex debug log at the specified logging level"

        override def getName: String = "changeLogLevels"
    }

    //this method should implement main logic of the action
    override protected def act()(implicit ec: ExecutionContext): Future[ActionResult] = {

        val conditions = List.newBuilder[String]
        config.getProperty("location") match {
            case Some(location) => conditions += s"Location = '$location'"
            case _ =>
        }
        config.getProperty("logUserName") match {
            case Some(userName) =>
                val users = SoqlQuery.getQueryIteratorTooling(session, s"select Id from User where UserName = '$userName' limit 1")
                if (users.nonEmpty) {
                    val user = new ResultRecord(users.next())
                    conditions += s"LogUserId = '${user.getFieldAsString("Id").get}'"
                }
            case _ =>
        }
        config.getProperty("request") match {
            case Some(request) => conditions += s"Request = '$request'"
            case _ =>
        }
        val conditionsList = conditions.result()
        var query = "select Id, Application, DurationMilliseconds, Location, LogLength, LogUserId, Operation, Request, StartTime, Status from ApexLog"
        if (conditionsList.nonEmpty) {
            query += " where " + conditionsList.mkString(" and ")
        }

        val queryIterator = SoqlQuery.getQueryIteratorTooling(session, query )

        //responseWriter.println("RESULT=SUCCESS")
        val builder = new ActionResultBuilder(SUCCESS)
        //responseWriter.println("RESULT_SIZE=" + queryIterator.size)
        builder.addMessage(InfoMessage("RESULT_SIZE=" + queryIterator.size))
        if (queryIterator.isEmpty) {
            //responseWriter.println(new ResponseWriter.Message(ResponseWriter.INFO, "No Logs available"))
            builder.addMessage(InfoMessage("No Logs available"))
        } else {
            val outputFilePath = config.getRequiredProperty("outputFilePath").get
            //make sure output file does not exist
            FileUtils.delete(new File(outputFilePath))
            val outputFile = new File(outputFilePath)
            for (batch <- queryIterator) {
                //TODO
                //writeResults(batch, outputFile)
            }
            //responseWriter.println("RESULT_FILE=" + outputFilePath)
            builder.addMessage(KeyValueMessage(Map("RESULT_FILE" -> outputFilePath)))
        }

        //responseWriter.println("RESULT=SUCCESS")
        Future.successful(builder.result())
    }
}
