/*
 * Copyright (c) 2020 Andrey Gavrikov.
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

package com.neowit.apex.actions
import com.neowit.response.RenameMetadataResult

import scala.concurrent.{ExecutionContext, Future}

class RenameMetadata extends ApexActionWithReadOnlySession {

    override def getHelp: ActionHelp = new ActionHelp {
        override def getParamDescription(paramName: String): String = paramName match {
            case "config" => "--config - full path to config.properties file"
            case "projectPath" => "--projectPath - full path to folder which contains ./src/ of apex project."
            case "responseFilePath" => "--responseFilePath - full path to file where result of the operation will be documented."
            case "metadataType" => "--metadataType - The metadata type of the components to rename."
            case "oldFullName" => "--oldFullName - The current component full name."
            case "newFullName" => "--packageName - The new component full name."
        }

        override def getParamNames: List[String] = List("config", "projectPath", "responseFilePath", "metadataType", "oldFullName", "newFullName")

        override def getExample: String = "--action=renameMetadata metadataType=CustomField --metadataType=CustomField --oldFullName=My_Object__c.Field_Name_Old__c --newFullName=My_Object__c.Field_Name_New__c"

        override def getSummary: String = "Renames a metadata component in your organization synchronously."

        override def getName: String = "renameMetadata"
    }
    override protected def act()(implicit ec: ExecutionContext): Future[ActionResult] = {
        val metadataType = config.getRequiredProperty("metadataType")
        val oldFullName = config.getRequiredProperty("oldFullName")
        val newFullName = config.getRequiredProperty("newFullName")

        val saveResult = session.renameMetadata(metadataType, oldFullName, newFullName)
        val actionResult =
            if (saveResult.isSuccess) {
                ActionSuccess(RenameMetadataResult(errors = Nil))
            } else {
                ActionFailure(RenameMetadataResult(errors = saveResult.getErrors.map(_.toString).toList))
            }
        Future.successful(actionResult)
    }
}
