package com.neowit.apex.actions.tooling

import java.io.File

import com.neowit.apex.Session
import com.sforce.soap.tooling.{SaveResult, TraceFlag}

object ToolingUtils {

    //we can use ToolingApi in following cases
    //1. there are no -meta.xml files
    //2. there are no new files
    //3. all files are supported by Tooling API
    //4. there is NO mix of aura and non aura files in the list of files to deploy
    def canUseTooling(session: Session, files: List[File]): Boolean = {
        val hasMeta = None != files.find(_.getName.endsWith("-meta.xml"))
        if (hasMeta) {
            return false
        }
        //check if there are new files
        val hasNewFile = None != files.find(f => {
            val key = session.getKeyByFile(f)
            "" == session.getData(key).getOrElse("Id", "")
        })

        if (hasNewFile) {
            return false
        }
        val hasAuraFiles = None != files.find(AuraMember.isSupportedType(_))
        val hasApexFiles = None != files.find(ApexMember.isSupportedType(_, session))
        val hasMixOfApexAndAura = hasAuraFiles && hasApexFiles
        //check if all files supported by tooling api
        val hasUnsupportedType = None != files.find(f => !ApexMember.isSupportedType(f, session) && !AuraMember.isSupportedType(f))
        val canNotUseTooling = hasUnsupportedType || hasMixOfApexAndAura

        !canNotUseTooling
    }

    /**
     * using Metadata Log type - configure all log types for TraceFlag
     * @param metadataLogType
     * @param traceFlag
     */
    private def setMedatataLogTypeToTooling(metadataLogType: String, traceFlag: TraceFlag) = {
        //default - DB & Apex = Debug
        traceFlag.setApexProfiling(null)
        traceFlag.setApexCode("Debug")
        traceFlag.setDatabase("Debug")
        traceFlag.setCallout("Debug")
        metadataLogType match {
            case "Debugonly" =>
                traceFlag.setApexCode("Debug")
                traceFlag.setDatabase("Debug")
            case "DB" =>
                traceFlag.setApexProfiling(null)
                traceFlag.setApexCode("Warn")
                traceFlag.setDatabase("Debug")
                traceFlag.setCallout(null)
            case "Profiling" =>
                traceFlag.setApexProfiling("Fine")
                traceFlag.setDatabase("Fine")
                traceFlag.setApexCode("Debug")
                traceFlag.setCallout(null)
            case "Callout" =>
                traceFlag.setApexProfiling(null)
                traceFlag.setDatabase("Debug")
                traceFlag.setApexCode("Debug")
                traceFlag.setCallout("Fine")
            case "Detail" =>
                traceFlag.setApexProfiling("Debug")
                traceFlag.setDatabase("Fine")
                traceFlag.setApexCode("Fine")
                traceFlag.setCallout("Fine")
        }

    }

    def setupTrace(session:Session, logger: com.neowit.utils.Logging): Option[String] = {
        logger.debug("Setup TraceFlag")
        //config.logLevel
        val flag: TraceFlag = new TraceFlag
        setMedatataLogTypeToTooling(session.config.logLevel, flag)
        val calendar = session.getServerTimestamp.getTimestamp
        //set TTL for TraceFlag same as for Server
        calendar.add(java.util.Calendar.SECOND, session.getConfig.getProperty("timeoutSec").getOrElse("30").toInt)

        flag.setExpirationDate(calendar)
        flag.setScopeId(null)
        flag.setTracedEntityId(session.getUserId)

        val saveResults: Array[SaveResult] = session.createTooling(Array(flag))
        if (saveResults.nonEmpty && saveResults.head.isSuccess) {
            Some(saveResults.head.getId)
        } else {
            val errors = saveResults.head.getErrors
            if (null != errors && errors.nonEmpty) {
                if (errors.head.getMessage.contains("This entity is already being traced")) {
                    //ignore
                } else {
                    logger.error("Failed to setup TraceFlag: " + errors.head.getMessage)
                }
            }
            None
        }
    }

    def getLog(session:Session, logId: String): String = {
        session.getRestContentTooling("/sobjects/ApexLog/"+logId+"/Body/", "") match {
          case Some(log) => log
          case None => ""
        }

    }

    /**
     * retireve content of Last log for current User
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
