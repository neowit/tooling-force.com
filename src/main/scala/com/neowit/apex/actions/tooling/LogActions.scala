package com.neowit.apex.actions.tooling

import java.io.File

import com.neowit.apex.Session
import com.neowit.apex.actions.{SoqlQuery, ActionHelp, ApexAction}
import com.neowit.utils.{ZuluTime, ResponseWriter, ConfigValueException, FileUtils}
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
                //TODO generate response file
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
    private def saveTraceFlagConfig(traceFlagMap: Map[String, String]) = {
        session.setData("traceFlagConfig", traceFlagMap)
    }

    def setupTrace(traceFlagMap: Map[String, String], session:Session, logger: com.neowit.utils.Logging): Try[String] = {

        logger.debug("Setup TraceFlag")

        val flag: TraceFlag = new TraceFlag
        traceFlagMap.foreach{
            case (logType, logLevel) => setLogLevelByType(logType, logLevel, flag)
        }

        val calendar = session.getServerTimestamp.getTimestamp
        //set TTL for TraceFlag same as for Server
        calendar.add(java.util.Calendar.SECOND, session.getConfig.getProperty("timeoutSec").getOrElse("30").toInt)

        flag.setExpirationDate(calendar)

        config.getProperty("scope") match {
          case Some(x) if "user" == x =>
              flag.setScopeId(session.getUserId)
          case _ =>
              flag.setScopeId(null)
        }
        getTracedEntityId match {
            case Success(entityId) =>
                flag.setTracedEntityId(entityId)
            case Failure(e) =>
                //messages += new ResponseWriter.Message(ResponseWriter.ERROR, e.getMessage)
                throw e
        }
        //check if trace for current scope/tracedEntity is already setup and expires too early, and delete it if one found
        val queryIterator = SoqlQuery.getQueryIteratorTooling[TraceFlag](session,
                                        s""" select Id from TraceFlag
                                           |where TracedEntityId = '${flag.getTracedEntityId}' and ScopeId = '${flag.getScopeId}'
                                           |""".stripMargin)
        if (queryIterator.hasNext) {
            val recordIds = queryIterator.map(_.getId).toArray
            session.deleteTooling(recordIds)
        }

        val saveResults: Array[SaveResult] = session.createTooling(Array(flag))
        if (saveResults.nonEmpty && saveResults.head.isSuccess) {
            saveTraceFlagConfig(traceFlagMap)
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
                        val iterator = SoqlQuery.getQueryIteratorTooling[ApexTrigger](session, s"select Id from ApexTrigger where Name = '${FileUtils.removeExtension(x)}'")
                        if (iterator.hasNext) {
                            Success(iterator.next().getId)
                        } else {
                            throw new ConfigValueException(s"Trigger with name $x not found")
                        }
                    case "class" =>
                        val iterator = SoqlQuery.getQueryIteratorTooling[ApexClass](session, s"select Id from ApexClass where Name = '${FileUtils.removeExtension(x)}'")
                        if (iterator.hasNext) {
                            Success(iterator.next().getId)
                        } else {
                            throw new ConfigValueException(s"Class with name $x not found")
                        }
                    case username =>
                        val iterator = SoqlQuery.getQueryIteratorTooling[User](session, s"select Id from User where UserName = '$x'")
                        if (iterator.hasNext) {
                            Success(iterator.next().getId)
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
