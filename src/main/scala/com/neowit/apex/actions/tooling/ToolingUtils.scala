package com.neowit.apex.actions.tooling

import java.io.File

import com.neowit.apex.Session

object ToolingUtils {

    //we can use ToolingApi in following cases
    //1. there are no -meta.xml files
    //2. there are no new files
    //3. all files are supported by Tooling API
    //4. there is NO mix of aura and non aura files in the list of files to deploy
    def canUseTooling(session: Session, files: List[File]): Boolean = {
        val hasMeta = files.exists(_.getName.endsWith("-meta.xml"))
        if (hasMeta) {
            return false
        }
        //check if there are new files
        val hasNewFile = files.exists(f => {
            val key = session.getKeyByFile(f)
            "" == session.getData(key).getOrElse("Id", "")
        })

        if (hasNewFile) {
            return false
        }
        val hasAuraFiles = files.exists(AuraMember.isSupportedType(_))
        val hasApexFiles = files.exists(ApexMember.isSupportedType(_, session))
        val hasMixOfApexAndAura = hasAuraFiles && hasApexFiles
        //check if all files supported by tooling api
        val hasUnsupportedType = files.exists(f => !ApexMember.isSupportedType(f, session) && !AuraMember.isSupportedType(f))
        val canNotUseTooling = hasUnsupportedType || hasMixOfApexAndAura

        !canNotUseTooling
    }


}
