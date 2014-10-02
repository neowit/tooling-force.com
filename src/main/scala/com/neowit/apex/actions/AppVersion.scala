package com.neowit.apex.actions

import com.neowit.utils.{ResponseWriter, BasicConfig}
import com.neowit.utils.ResponseWriter.Message

object AppVersion {
    val APP_NAME = "tooling-force.com"
    val VERSION = "0.3.0"
}
class AppVersion(basicConfig: BasicConfig) extends ApexAction(basicConfig: BasicConfig) {
    override def act(): Unit = {
        /*
        val p = getClass.getPackage
        val name = p.getImplementationTitle
        val version = p.getImplementationVersion
        */
        config.responseWriter.println("RESULT=SUCCESS")
        config.responseWriter.println(new Message(ResponseWriter.INFO, AppVersion.APP_NAME + " - version: " + AppVersion.VERSION))
        logger.debug("version: " + AppVersion.VERSION)
    }

    override def getParamNames: List[String] = List()

    override def getSummary: String = "return version of tooling-force.com"

    override def getName: String = "version"

    override def getExample: String = ""

    override def getParamDescription(paramName: String): String = ""
}
