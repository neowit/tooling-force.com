package com.neowit.apex

import com.neowit.apex.actions.{ActionHelp, ActionResult, ActionResultBuilder, ApexAction}
import com.neowit.response.{InfoMessage, MessageDetailMap, SUCCESS}

import scala.concurrent.{ExecutionContext, Future}

object AppVersion {
    val APP_NAME = "tooling-force.com"
    val VERSION = "0.3.6.7"
}
class AppVersion extends ApexAction {
    protected override def act()(implicit ec: ExecutionContext): Future[ActionResult] = {
        /*
        val p = getClass.getPackage
        val name = p.getImplementationTitle
        val version = p.getImplementationVersion
        */

        //config.responseWriter.println(SUCCESS)
        val builder = new ActionResultBuilder(SUCCESS)
        val versionMessage = InfoMessage(AppVersion.APP_NAME + " - version: " + AppVersion.VERSION + "; SFDC API Version: " + config.apiVersion)
        //config.responseWriter.println(versionMessage)
        builder.addMessage(versionMessage)
        val mb = 1024*1024
        val runtime = Runtime.getRuntime
        builder.addDetail( MessageDetailMap(versionMessage, Map("type" -> "DEBUG", "text" -> s"Used Memory: ${(runtime.totalMemory - runtime.freeMemory) / mb} MB")))
        builder.addDetail( MessageDetailMap(versionMessage, Map("type" -> "DEBUG", "text" -> s"Free Memory: ${runtime.freeMemory / mb} MB")))
        builder.addDetail( MessageDetailMap(versionMessage, Map("type" -> "DEBUG", "text" -> s"Total Memory: ${runtime.totalMemory / mb} MB")))
        builder.addDetail( MessageDetailMap(versionMessage, Map("type" -> "DEBUG", "text" -> s"Max Memory: ${runtime.maxMemory / mb} MB")))
        logger.debug("version: " + AppVersion.VERSION)
        Future.successful(builder.result())
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
