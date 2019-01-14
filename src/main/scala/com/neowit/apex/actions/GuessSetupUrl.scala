/*
 * Copyright (c) 2018 Andrey Gavrikov.
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

import com.neowit.apex.actions.SoqlQuery.ResultRecord
import com.neowit.response.GuessSetupUrlResult
import com.neowit.utils.{FileUtils, JsonSupport, OsUtils}

import scala.concurrent.{ExecutionContext, Future}

/**
  * Created by Andrey Gavrikov 
  */
class GuessSetupUrl extends ApexActionWithReadOnlySession with JsonSupport {
    override def getHelp: ActionHelp = new ActionHelp {
        override def getExample: String = "--action=guessSetupUrl --name=MyClass --type=ApexClass --openInBrowser=false"

        override def getParamDescription(paramName: String): String = paramName match {
            case "name" => "--name - setup resource name. REQUIRED"
            case "type" => "--type - one of the following: SObject, ApexClass, ApexTrigger, ApexPage, ApexComponent, CustomLabels, SControl, StaticResource"
            case "openInBrowser" => "--openInBrowser if true then attempt to open resulting URL in a web browser. OPTIONAL"
            case x => s"Parameter '$x' is not supported for this action"
        }

        override def getParamNames: List[String] = List("name", "type", "api")

        override def getSummary: String =
            """
              |Using provided resource name try to guess what type of resource this is and its possible URL in SFDC Setup UI
              |
              |example 1:
              |--action=guessSetupUrl --name=MySomething --type=ApexClass
              |
              |example 2:
              |--action=guessSetupUrl --name=MySomething.label --openInBrowser=true
              | here we give a hint that MySomething is a custom label and ask to open found URL in web browser
              |
              |example 2:
              |--action=guessSetupUrl --name=MySomething.cls
              | here we give a hint that MySomething is an ApexClass
              |
              |when --type is not specified we try to guess the type and api as follows
              |1. check if provided name ends with either of the following
              |- .cls, .trigger, __c, .page, .component, .resource,
              |
              |if it does then use determined type to get resource Id and probable Setup URL
              |
              |2. if name does not contain a known file suffix/extension
              |- check if there is an sobject with name "MyClass"
              |- if not found then keep trying in the following order
              |-- ApexClass, ApexTrigger, ApexPage, ApexComponent, AuraDefinitionBundle, StaticResource, Scontrol, CustomLabels
            """.stripMargin

        override def getName: String = "guessSetupUrl"
    }
    override protected def act()(implicit ec: ExecutionContext): Future[ActionResult] = {
        val resourceName = config.getRequiredProperty("name")
        val actionResult  =
            guessUrl(resourceName, config.getProperty("type")) match {
                case Some(_url) if config.getProperty("openInBrowser").contains("true") =>
                    OsUtils.openUrl(_url)
                    ActionSuccess( GuessSetupUrlResult( url = _url ) )
                case Some(_url) => ActionSuccess( GuessSetupUrlResult( url = _url ) )
                case None =>  ActionFailure("unable to guess URL using provided data")
            }

        Future.successful(actionResult)
    }

    private def guessUrl(name: String, resourceType: Option[String]): Option[String] = {
        val probableResourceTypeOpt =
        resourceType match {
            case typeOpt @ Some(_) => typeOpt
            case None => guessType(name)
        }
        probableResourceTypeOpt.map(_.toLowerCase) match {
            case Some("sobject") =>
                guessSObjectUrl(name)
            case Some("apexclass") =>
                guessClassUrl(name)
            case Some("apextrigger") =>
                guessApexTriggerUrl(name)
            case Some("apexpage") =>
                guessPageUrl(name)
            case Some("staticresource") =>
                guessStaticResourceUrl(name)
            case Some("apexcomponent") =>
                guessApexComponentUrl(name)
            case _ =>
                ???
        }
    }
    private def guessType(name: String): Option[String] = name match {
        case n if n.endsWith("__c") => Option("SObject")
        case n if n.endsWith(".object") => Option("SObject")
        case n if n.endsWith(".label") => Option("CustomLabels")
        case n if n.endsWith(".cls") => Option("ApexClass")
        case n if n.endsWith(".trigger") => Option("ApexTrigger")
        case n if n.endsWith(".page") => Option("ApexPage")
        case n if n.endsWith(".component") => Option("ApexComponent")
        case n if n.endsWith(".scf") => Option("Scontrol")
        case n if n.endsWith(".resource") => Option("StaticResource")
        case _ => None

    }
    private def guessSObjectUrl(targetName: String): Option[String] = {
        var objectName = FileUtils.removeExtension(targetName)
        objectName = if (objectName.endsWith("__c")) objectName.replaceAll("__c$", "") else objectName
        queryObjectId("CustomObject", objectName, "DeveloperName")
    }
    private def guessClassUrl(targetName: String): Option[String] = {
        val className = FileUtils.removeExtension(targetName)
        queryObjectId("ApexClass", className)
    }
    private def guessApexTriggerUrl(targetName: String): Option[String] = {
        val className = FileUtils.removeExtension(targetName)
        queryObjectId("ApexTrigger", className)
    }
    private def guessPageUrl(targetName: String): Option[String] = {
        val pageName = FileUtils.removeExtension(targetName)
        queryObjectId("ApexPage", pageName)
    }
    private def guessStaticResourceUrl(targetName: String): Option[String] = {
        val objectName = FileUtils.removeExtension(targetName)
        queryObjectId("StaticResource", objectName)
    }
    private def guessApexComponentUrl(targetName: String): Option[String] = {
        val objectName = FileUtils.removeExtension(targetName)
        queryObjectId("ApexComponent", objectName)
    }
    private def queryObjectId(objectType: String, objectName: String, nameField: String = "Name"): Option[String] = {
        val query =
            s"""select Id
               |from $objectType
               |where $nameField = '$objectName'
               |limit 1
            """.stripMargin

        val queryIterator = SoqlQuery.getQueryIteratorTooling(session, query).map(new ResultRecord(_))
        if (queryIterator.nonEmpty) {
            queryIterator.next().getFieldAsString("Id") match {
                case Some(id) =>
                    session.getServiceDomain.map(_ + id)
                case None => None
            }

        } else {
            None
        }
    }
}
