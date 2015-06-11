package com.neowit.apex.actions.tooling

import java.io.File

import com.neowit.apex.Session
import com.neowit.apex.actions.{SoqlQuery, ActionHelp, ApexAction}
import com.neowit.utils.{ConfigValueException, FileUtils}
import com.sforce.soap.tooling.{SaveResult, TraceFlag}

import spray.json._
import DefaultJsonProtocol._

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
        }

        override def getParamNames: List[String] = List("logConfig")

        override def getSummary: String = "Setup TraceFlag that triggers an Apex debug log at the specified logging level"

        override def getName: String = "setupTraceFlag"
    }

    //this method should implement main logic of the action
    override protected def act(): Unit = {
        val traceFlagMap = loadTraceFlagConfig

        setupTrace(traceFlagMap, session, logger) match {
            case Some(traceFlagId) =>
                //TODO generate response file
                responseWriter.println("RESULT=SUCCESS")
            case None =>
                responseWriter.println("RESULT=FAILURE")
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
                        case (key, jsVal) => {
                            //this case should never be used, but have it here to make compiler happy
                            key -> jsVal.toString()
                        }
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

    def setupTrace(traceFlagMap: Map[String, String], session:Session, logger: com.neowit.utils.Logging): Option[String] = {

        logger.debug("Setup TraceFlag")
        //TODO check if trace for current session.getUserId is already setup, and delete it if it is
        val queryIterator = SoqlQuery.getQueryIteratorTooling[TraceFlag](session, s"select Id from TraceFlag where TracedEntityId = '${session.getUserId}'")
        if (queryIterator.hasNext) {
            val record = queryIterator.next()
            session.deleteTooling(record.getTracedEntityId)
        }

        val flag: TraceFlag = new TraceFlag
        traceFlagMap.foreach{
            case (logType, logLevel) => setLogLevelByType(logType, logLevel, flag)
        }

        val calendar = session.getServerTimestamp.getTimestamp
        //set TTL for TraceFlag same as for Server
        calendar.add(java.util.Calendar.SECOND, session.getConfig.getProperty("timeoutSec").getOrElse("30").toInt)

        flag.setExpirationDate(calendar)
        flag.setScopeId(null)
        flag.setTracedEntityId(session.getUserId)

        val saveResults: Array[SaveResult] = session.createTooling(Array(flag))
        if (saveResults.nonEmpty && saveResults.head.isSuccess) {
            saveTraceFlagConfig(traceFlagMap)
            Some(saveResults.head.getId)
        } else {
            val errors = saveResults.head.getErrors
            if (null != errors && errors.nonEmpty) {
                if (errors.head.getMessage.contains("This entity is already being traced")) {
                    //
                    logger.error("Failed to setup new TraceFlag: " + errors.head.getMessage)
                } else {
                    logger.error("Failed to setup TraceFlag: " + errors.head.getMessage)
                }
            }
            None
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
