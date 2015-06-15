package com.neowit.apex.actions.tooling

import java.io.File

import com.neowit.apex.Session
import com.neowit.apex.actions.SoqlQuery.ResultRecord
import com.neowit.apex.actions.{SoqlQuery, ActionHelp, ApexAction}
import com.neowit.utils.{ResponseWriter, ConfigValueException, FileUtils}
import com.sforce.soap.tooling._

import spray.json._
import DefaultJsonProtocol._

import scala.util.{Failure, Success, Try}

class LogActions {

}
class ChangeLogLevels extends ApexAction {
    override def getHelp: ActionHelp = new ActionHelp {
        override def getExample: String = ""
        override def getParamDescription(paramName: String): String = paramName match {
            case "logConfig" =>
                """--logConfig=/full/path/to/traceflag.conf
                  |  traceflag.conf must contain a JSON object with "type":"value" pairs.
                  |  All log types [ApexCode, ApexProfiling, ... Workflow] must be specified.
                  |  File content format is as follows:
                  |  "{"ApexCode": "Debug", "ApexProfiling": "Error", "Callout": "Error", "Database": "Error", "System": "Error", "Validation": "Error", "Visualforce": "Error", "Workflow": "Error"}"
                """.stripMargin
            case "scope" =>
                """--scope=user|<empty>
                  |  This parameter is Optional
                  |  see TraceFlag.ScopeId in Tooling API documentation
                  |  e.g. --scope=user
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

        override def getParamNames: List[String] = List("logConfig", "scope", "tracedEntity")

        override def getSummary: String = "Setup TraceFlag that triggers an Apex debug log at the specified logging level"

        override def getName: String = "changeLogLevels"
    }

    //this method should implement main logic of the action
    override protected def act(): Unit = {
        val traceFlagMap = loadTraceFlagConfig

        setupTrace(traceFlagMap, session, logger) match {
            case Success(traceFlagId) =>
                responseWriter.println("RESULT=SUCCESS")
            case Failure(e) =>
                responseWriter.println("RESULT=FAILURE")
                responseWriter.println(ResponseWriter.Message(ResponseWriter.ERROR, e.getMessage))

        }
    }

    private def loadTraceFlagConfig: Map[String, String] = {
        val traceFlagMap: Map[String, String] = config.getRequiredProperty("logConfig") match {
            case Some(logConfigPath) =>
                val f = new File(logConfigPath)
                if (f.canRead) {
                    val jsonStr = FileUtils.readFile(f).getLines().mkString("")
                    val jsonAst = JsonParser(jsonStr)
                    val pairs = jsonAst.asJsObject.fields.map {
                        case (key, jsVal:JsString) => key -> jsVal.value
                        case (key, jsVal) =>
                            //this case should never be used, but have it here to make compiler happy
                            key -> jsVal.toString()
                    }
                    pairs
                } else {
                    throw new ConfigValueException("file specified in --logConfig is not readable. Path: " + logConfigPath)
                }
            case None =>
                val data = session.getData("traceFlagConfig")
                if (data.nonEmpty) {
                    data.map{case (key, str) => key -> str.toString}
                } else {
                    Map("ApexCode" -> "Debug", "ApexProfiling"-> "Error", "Callout"-> "Error", "Database"-> "Error", "System"-> "Error", "Validation"-> "Error", "Visualforce"-> "Error", "Workflow"-> "Error")

                }
        }
        traceFlagMap
    }

    /**
     * traceFlagConfig={"ApexProfiling" : "Error", "Validation" : "Error", "Database" : "Error", "Workflow" : "Error",
     *                  "ApexCode" : "Debug", "System" : "Error", "Visualforce" : "Error", "Callout" : "Debug"}

     * @param tarceFlag
     * @param traceFlagMap
     */
    private def saveTraceFlagConfig(tarceFlag: TraceFlag, traceFlagMap: Map[String, String]) = {
        //TODO consider saving with Scope and TracedEntity taken into account
        session.setData("traceFlagConfig", traceFlagMap)
    }

    def setupTrace(traceFlagMap: Map[String, String], session:Session, logger: com.neowit.utils.Logging): Try[String] = {

        logger.debug("Setup TraceFlag")

        val traceFlag: TraceFlag = new TraceFlag
        traceFlagMap.foreach{
            case (logType, logLevel) => setLogLevelByType(logType, logLevel, traceFlag)
        }

        val calendar = session.getServerTimestamp.getTimestamp
        //set TTL for TraceFlag same as for Server
        calendar.add(java.util.Calendar.SECOND, session.getConfig.getProperty("timeoutSec").getOrElse("30").toInt)

        traceFlag.setExpirationDate(calendar)

        config.getProperty("scope") match {
          case Some(x) if "user" == x =>
              traceFlag.setScopeId(session.getUserId)
          case _ =>
              traceFlag.setScopeId(null)
        }
        getTracedEntityId match {
            case Success(entityId) =>
                traceFlag.setTracedEntityId(entityId)
            case Failure(e) =>
                //messages += new ResponseWriter.Message(ResponseWriter.ERROR, e.getMessage)
                throw e
        }
        //check if trace for current scope/tracedEntity is already setup and expires too early, and delete it if one found
        val queryIterator = SoqlQuery.getQueryIteratorTooling(session,
                                        s""" select Id from TraceFlag
                                           |where TracedEntityId = '${traceFlag.getTracedEntityId}' and ScopeId = '${traceFlag.getScopeId}'
                                           |""".stripMargin)
        if (queryIterator.hasNext) {
            val recordIds = queryIterator.map(obj => new ResultRecord(obj).getFieldAsString("Id")).map(_.get).toArray
            session.deleteTooling(recordIds)
        }

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
    private def setLogLevelByType(logType: String, logLevel: String, traceFlag: TraceFlag) = {
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
class ListLogs extends ApexAction {
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
    override protected def act(): Unit = {

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

        responseWriter.println("RESULT=SUCCESS")
        responseWriter.println("RESULT_SIZE=" + queryIterator.size)
        if (queryIterator.isEmpty) {
            responseWriter.println(new ResponseWriter.Message(ResponseWriter.INFO, "No Logs available"))
        } else {
            val outputFilePath = config.getRequiredProperty("outputFilePath").get
            //make sure output file does not exist
            FileUtils.delete(new File(outputFilePath))
            val outputFile = new File(outputFilePath)
            for (batch <- queryIterator) {
                //TODO
                //writeResults(batch, outputFile)
            }
            responseWriter.println("RESULT_FILE=" + outputFilePath)
        }

        responseWriter.println("RESULT=SUCCESS")
    }
}
