package com.neowit.apex

import com.neowit.apex.actions.{ActionHelp, ApexAction}
import com.neowit.utils.ResponseWriter.{InfoMessage, MessageDetail, SUCCESS}

object AppVersion {
    val APP_NAME = "tooling-force.com"
    val VERSION = "0.3.6.7"
}
class AppVersion extends ApexAction {
    override def act(): Unit = {
        /*
        val p = getClass.getPackage
        val name = p.getImplementationTitle
        val version = p.getImplementationVersion
        */
        config.responseWriter.println(SUCCESS)
        val versionMessage = InfoMessage(AppVersion.APP_NAME + " - version: " + AppVersion.VERSION + "; SFDC API Version: " + config.apiVersion)
        config.responseWriter.println(versionMessage)
        val mb = 1024*1024
        val runtime = Runtime.getRuntime
        responseWriter.println( MessageDetail(versionMessage, Map("type" -> "DEBUG", "text" -> s"Used Memory: ${(runtime.totalMemory - runtime.freeMemory) / mb} MB")))
        responseWriter.println( MessageDetail(versionMessage, Map("type" -> "DEBUG", "text" -> s"Free Memory: ${runtime.freeMemory / mb} MB")))
        responseWriter.println( MessageDetail(versionMessage, Map("type" -> "DEBUG", "text" -> s"Total Memory: ${runtime.totalMemory / mb} MB")))
        responseWriter.println( MessageDetail(versionMessage, Map("type" -> "DEBUG", "text" -> s"Max Memory: ${runtime.maxMemory / mb} MB")))
        logger.debug("version: " + AppVersion.VERSION)
    }


    override def getHelp: ActionHelp = new ActionHelp {
        override def getParamNames: List[String] = List("responseFilePath")

        override def getSummary: String = "return version of tooling-force.com"

        override def getName: String = "version"

        override def getExample: String = ""

        override def getParamDescription(paramName: String): String = paramName match {
            case "responseFilePath" => "--responseFilePath - path to file where operation result will be reported"
            case x => s"Parameter '$x' is not supported for this action"
        }

    }

}
