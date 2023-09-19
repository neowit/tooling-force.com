/*
 * Copyright (c) 2017 Andrey Gavrikov.
 * this file is part of tooling-force.com application
 * https://github.com/neowit/tooling-force.com
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package com.neowit.apex

import com.neowit.apex.actions._
import com.neowit.response._

import scala.concurrent.{ExecutionContext, Future}

object AppVersion {
    val APP_NAME = "tooling-force.com"
    val VERSION = "0.5.8.1"
    val SFDC_API_VERSION:Double = 58.0
}
class AppVersion extends ApexAction {
    protected override def act()(implicit ec: ExecutionContext): Future[ActionResult] = {
        /*
        val p = getClass.getPackage
        val name = p.getImplementationTitle
        val version = p.getImplementationVersion
        */
        logger.debug("version: " + AppVersion.VERSION)
        Future.successful(
            ActionSuccess(
                AppVersionResult(
                    appName = AppVersion.APP_NAME,
                    appVersion = AppVersion.VERSION,
                    sfdcApiVersion = String.valueOf(config.apiVersion),
                    javaVersion = System.getProperty("java.version"),
                    os = System.getProperty("os.name")
                )
            )
        )
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
