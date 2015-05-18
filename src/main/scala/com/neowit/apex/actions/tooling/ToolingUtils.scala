package com.neowit.apex.actions.tooling

import java.io.File

import com.neowit.apex.Session
import com.neowit.utils.ZuluTime
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

    def setupLog(session:Session): Option[String] = {
        //config.logLevel
        val flag: TraceFlag = new TraceFlag
        flag.setApexCode("Debug")
        flag.setExpirationDate(ZuluTime.toCalendar(System.currentTimeMillis() + 10000))
        flag.setTracedEntityId(session.getUserId)

        val saveResults: Array[SaveResult] = session.createTooling(Array(flag))
        if (saveResults.nonEmpty && saveResults.head.isSuccess) {
            Some(saveResults.head.getId)
        } else {
            None
        }
    }

    def getLog(session:Session): String = {
        session.queryTooling()

    }
}
