package com.neowit.apex

import com.neowit.apex.actions.{ActionHelp, ApexAction}
import com.neowit.utils.ResponseWriter
import com.neowit.utils.ResponseWriter.Message

object AppVersion {
    val APP_NAME = "tooling-force.com"
    val VERSION = "0.3.1.0"
}
class AppVersion extends ApexAction {
    override def act(): Unit = {
        /*
        val p = getClass.getPackage
        val name = p.getImplementationTitle
        val version = p.getImplementationVersion
        */
        config.responseWriter.println("RESULT=SUCCESS")
        config.responseWriter.println(new Message(ResponseWriter.INFO, AppVersion.APP_NAME + " - version: " + AppVersion.VERSION + "; SFDC API Version: " + config.apiVersion))
        logger.debug("version: " + AppVersion.VERSION)
    }


    override def getHelp: ActionHelp = new ActionHelp {
        override def getParamNames: List[String] = List()

        override def getSummary: String = "return version of tooling-force.com"

        override def getName: String = "version"

        override def getExample: String = ""

        override def getParamDescription(paramName: String): String = ""

    }

}
