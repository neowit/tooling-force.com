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

import java.io.{File, FileInputStream, InputStream}
import java.nio.file.{Files, Path, Paths}

import com.neowit.apex.ProjectsCache
import com.neowit.apex.parser.{CompoundMember, Member}
import com.neowit.apexscanner.{FileBasedDocument, Project}
import com.neowit.apexscanner.nodes._
import com.neowit.apexscanner.resolvers.AscendingDefinitionFinder
import com.neowit.apexscanner.scanner.actions.{ActionContext, FindSymbolActionType}
import com.neowit.apexscanner.symbols._
import com.neowit.response.FindSymbolResult
import com.neowit.utils.ConfigValueException

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Random

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
                case "actionId" =>
                    """--actionId - Id of this action.
                      |     Can be used later to cancel this action using:
                      |         --action=cancel --actionId=<specific-action-id>
                      |     if --actionId is not provided in findSymbol call then all previously submitted 'findSymbol' actions will be cancelled
                      """.stripMargin
                case _ => ""
            }
        }

        override def getParamNames: List[String] = List("projectPath", "currentFilePath", "currentFileContentPath",
            "line", "column", "responseFilePath")

        override def getSummary: String = "find definition of token in specified position"

        override def getName: String = "findSymbol"
    }
    override protected def act()(implicit ec: ExecutionContext): Future[ActionResult] = {
        val config = session.getConfig
        if (config.projectDirOpt.isEmpty) {
            // this action is not applicable when project is not provided
            throw new ConfigValueException("Invalid or Missing --projectPath parameter")
        }
        val projectDir = config.projectDirOpt.get

        val actionResultOptOfFuture: Option[Future[ActionResult]] =
            for (   filePath <- config.getRequiredProperty("currentFileContentPath");
                    currentFilePath <- config.getRequiredProperty("currentFilePath");
                    line <- config.getRequiredProperty("line");
                    column <- config.getRequiredProperty("column");
                    project <- ProjectsCache.getProject(projectDir, session)
            ) yield {
                if (! Files.isReadable(Paths.get(filePath))) {
                    Future.successful(ActionFailure(s"'currentFileContentPath' must point to readable file"))
                } else {
                    val inputFile = new File(filePath) // temp file - has latest content
                    val sourceFile = new File(currentFilePath) // real file (may not have latest content)
                    val document = new FileBasedDocument(sourceFile.toPath) {
                        override def inputStream: InputStream = new FileInputStream(inputFile)
                    }
                    val position = Position(line.toInt, column.toInt - 1)

                    // create ActionContext
                    val actionIdOpt = config.getProperty("actionId")
                    val actionContext: ActionContext = actionIdOpt match {
                        case Some(actionId) => ActionContext(actionId, FindSymbolActionType)
                        case None =>
                            // cancel all running actions of FindSymbolActionType
                            ActionContext.cancelAll(FindSymbolActionType)
                            ActionContext("FindSymbol-temp-" + Random.nextString(5), FindSymbolActionType)
                    }
                    project.getAst(document) match {
                        case Some(result) =>
                            if (actionContext.isCancelled) {
                                return Future.successful(ActionSuccess(FindSymbolResult(None)))
                            }
                            val finder = new AscendingDefinitionFinder(actionContext)
                            val allDefNodes = finder.findUltimateDefinition(result.rootNode, position, project)
                            val members = allDefNodes.filter {
                                case defNode: AstNode if Range.INVALID_LOCATION != defNode.range => true
                                case _ => false
                            }.map { defNode =>
                                val s = nodeToSymbol(defNode)
                                Member.symbolToMember(s)
                            }

                            if (members.nonEmpty) {
                                Future.successful(ActionSuccess(FindSymbolResult(Option(CompoundMember(members.toList)))))
                            } else {
                                Future.successful(ActionSuccess(FindSymbolResult(None)))
                            }

                        case _ => Future.successful(ActionSuccess(FindSymbolResult(None)))
                    }

                }
            }
        actionResultOptOfFuture match {
            case Some(actionSuccess) => actionSuccess
            case None =>
                Future.successful(ActionSuccess())
        }
    }

    private def nodeToSymbol(defNode: AstNode with IsTypeDefinition): Symbol = {
        new Symbol {

            override def symbolLocation: com.neowit.apexscanner.nodes.Location = {
                defNode.getFileNode  match {
                    case Some(fileNode) =>
                        new com.neowit.apexscanner.nodes.Location {
                            def project: Project = fileNode.project
                            def range: Range = defNode.range
                            def path: Path = fileNode.file
                        }
                    case None => LocationUndefined
                }
            }
            override def symbolName: String = defNode.qualifiedName.map(_.getLastComponent).getOrElse("")
            // all other methods are not used presently in FindSymbolResult
            override def parentSymbol: Option[Symbol] = None
            override def symbolValueType: Option[String] = None
            override def symbolKind: SymbolKind = null
            override def symbolIsStatic: Boolean = false

            override def visibility: Option[String] = None
        }
    }
    /*
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
                    val classes = scanner.getRecognisedApexFiles.filterNot(_.getAbsolutePath == currentFilePath) ++ List(new File(filePath))
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
    */
}

