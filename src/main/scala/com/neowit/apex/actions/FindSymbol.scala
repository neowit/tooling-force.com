/*
 * Copyright (c) 2016 Andrey Gavrikov.
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

import java.io.File
import java.nio.file.{Files, Paths}

import com.neowit.apex.completion._
import com.neowit.apex.parser._
import com.neowit.response.FindSymbolResult
import com.neowit.utils.ConfigValueException

import scala.concurrent.{ExecutionContext, Future}

class FindSymbol extends ApexActionWithReadOnlySession {
    override def getHelp: ActionHelp = new ActionHelp {
        override def getExample: String = ""

        override def getParamDescription(paramName: String): String = {
            paramName match {
                case "projectPath" => "--projectPath - full path to project folder"
                case "currentFilePath" => "--currentFilePath - full path to current code file"
                case "currentFileContentPath" => "--currentFileContentPath - full path to temp file where current code file content is saved. If current file is saved then can be the same as currentFilePath"
                case "line" => "--line - line of cursor position in the current code file, starts with 1"
                case "column" => "--column - column of cursor position in the current code file, starts with 1"
                case "responseFilePath" => "--responseFilePath - path to file where completion candidates will be saved in JSON format"
                case _ => ""
            }
        }

        override def getParamNames: List[String] = List("projectPath", "currentFilePath", "currentFileContentPath",
            "line", "column", "responseFilePath")

        override def getSummary: String = "find definition of token in specified position"

        override def getName: String = "findSymbol"
    }

    //this method should implement main logic of the action
    override protected def act()(implicit ec: ExecutionContext): Future[ActionResult] = {
        val config = session.getConfig
        if (config.projectDirOpt.isEmpty) {
            // this action is not applicable when project is not provided
            throw new ConfigValueException("Invalid or Missing --projectPath parameter")
        }
        val projectDir = config.projectDirOpt.get

        val actionResultOpt =
            for (   filePath <- config.getRequiredProperty("currentFileContentPath");
                    line <- config.getRequiredProperty("line");
                    column <- config.getRequiredProperty("column")
            ) yield {
                if (! Files.isReadable(Paths.get(filePath))) {
                    //config.responseWriter.println(FAILURE)
                    //config.responseWriter.println(ErrorMessage(s"'currentFileContentPath' must point to readable file"))
                    ActionFailure(s"'currentFileContentPath' must point to readable file")
                } else {
                    val inputFile = new File(filePath)
                    val scanner = new ScanSource().load[ScanSource](session)
                    //provide alternative location for current file (it may not be saved in project location)
                    val currentFilePath = config.getRequiredProperty("currentFilePath")
                    val classes = scanner.getClassFiles.filterNot(_.getAbsolutePath == currentFilePath) ++ List(new File(filePath))
                    scanner.scan(classes)

                    val cachedTree:ApexTree = SourceScannerCache.getScanResult(projectDir)  match {
                        case Some(sourceScanner) => sourceScanner.getTree
                        case None => new ApexTree
                    }

                    val completion = new AutoComplete(inputFile, line.toInt, column.toInt, cachedTree, session, isDefinitionOnly = true)
                    val definitionOpt = completion.getDefinition
                    //config.responseWriter.println(SUCCESS)

                    val memberOpt =
                        definitionOpt match {
                            case Some(definition) =>
                                getSymbol(definition, completion) match {
                                    case Some(member) =>
                                        //config.responseWriter.println(member.serialise.compactPrint)
                                        Option(member)
                                    case None =>
                                        println("Not local resource, no location available")
                                        //config.responseWriter.println("{}")
                                        None
                                }
                            case None =>
                                println("Definition not found")
                                //config.responseWriter.println("{}")
                                None
                        }
                    ActionSuccess(FindSymbolResult(memberOpt))
                }
            }
        Future.successful(actionResultOpt.getOrElse(ActionSuccess()))

    }

    private def getSymbol(definition: TokenDefinition, completion: AutoComplete): Option[Member] = {
        definition match {
            case ApexTokenDefinition(definitionMember, typeMember) =>
                Option(definitionMember)
            case apexTokenDefinition @ ApexTokenDefinitionWithContext(definitionMember, typeMember, _, _, expressionTokens) =>
                findSymbolDefinition(apexTokenDefinition, completion)
            case unresolvedDefinition @ UnresolvedApexTokenDefinition(extractor, fullApexTree, expressionTokens) =>
                findSymbolDefinition(unresolvedDefinition, completion)
            case _ =>
                None
        }
    }

    private def findSymbolDefinition(unresolvedDefinition: UnresolvedApexTokenDefinition, completion: AutoComplete): Option[Member] = {
        completion.resolveApexDefinition(Option(unresolvedDefinition))
    }

    private def findSymbolDefinition(apexTokenDefinition: ApexTokenDefinitionWithContext, completion: AutoComplete): Option[Member] = {
        completion.resolveApexDefinition(Option(apexTokenDefinition))
    }
}

