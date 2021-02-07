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

package com.neowit.apex.actions

import java.io.File
import java.nio.file.Path

import com.neowit.apex.ProjectsCache
import com.neowit.apexscanner.VirtualDocument
import com.neowit.apexscanner.scanner._
import com.neowit.apexscanner.scanner.actions.SyntaxChecker
import com.neowit.apexscanner.server.handlers.DidSaveHandler
import com.neowit.response.CheckSyntaxResult

import scala.concurrent.{ExecutionContext, Future}

/**
  * 'checkSyntax' action runs apexcode parser against specified file and reports syntax errors
  */
class CheckSyntax extends ApexActionWithReadOnlySession {

    override def getHelp: ActionHelp = new ActionHelp {
        override def getExample: String = ""

        override def getParamDescription(paramName: String): String = {
            paramName match {
                case "projectPath" => "--projectPath - full path to project folder"
                case "currentFilePath" => "--currentFilePath - full path to current code file"
                case "currentFileContentPath" => "--currentFileContentPath - full path to temp file where current code file content is saved. If current file is saved then can be the same as currentFilePath"
                //case "line" => "--line - line of cursor position in the current code file, starts with 1"
                //case "column" => "--column - column of cursor position in the current code file, starts with 1"
                case "responseFilePath" => "--responseFilePath - path to file where completion candidates will be saved in JSON format"
                case _ => ""
            }
        }

        override def getParamNames: List[String] =
            List("projectPath", "currentFilePath", "currentFileContentPath", /*"line", "column",*/ "responseFilePath")

        override def getSummary: String = "list syntax errors found in apex file"

        override def getName: String = "checkSyntax"
    }

    override protected def act()(implicit ec: ExecutionContext): Future[ActionResult] = {
        val config = session.getConfig

        val filePath = config.getRequiredProperty("currentFileContentPath")
        //val sourceFilePath = config.getRequiredProperty("currentFilePath")
        val actionResultOpt =
            config.projectDirOpt.map{ projectDir =>
                val inputFile = new File(filePath)
                if (!filePath.endsWith(".cls") && !filePath.endsWith(".trigger")) {
                    // unsupported file extension
                    Future.successful(ActionFailure("Only .cls and .trigger files supported"))
                } else {
                    val project = ProjectsCache.getProject(projectDir, session)
                    val apexcodeScanner = new ApexcodeScanner() {
                        override def isIgnoredPath(path: Path): Boolean = !DidSaveHandler.isSupportedPath(path)
                        override def onEachResult(result: DocumentScanResult): DocumentScanResult = {
                            // document has been saved, its AST may have changed, purge AST from cache
                            result.document.fileOpt.map(filePath => project.clearFileContent(filePath))
                            result
                        }
                        override def errorListenerFactory(document: VirtualDocument): ApexErrorListener = SyntaxChecker.errorListenerCreator(document)
                    }
                    val soqlScanner = new SoqlScanner() {
                        override def isIgnoredPath(path: Path): Boolean = true
                        override def onEachResult(result: DocumentScanResult): DocumentScanResult = Scanner.defaultOnEachResult(result)
                        override def errorListenerFactory(document: VirtualDocument): ApexErrorListener = SyntaxChecker.errorListenerCreator(document)
                    }
                    val checker = new SyntaxCheckScanner(Seq(apexcodeScanner, soqlScanner))
                    val path = inputFile.toPath

                    checker.scan(path).map{_ =>
                        val results = checker.getResult
                        val syntaxErrors = results.map(_.errors).fold(Seq.empty)(_ ++ _).toList
                        if (syntaxErrors.isEmpty) {
                            ActionSuccess()
                        } else {
                            ActionFailure(CheckSyntaxResult(inputFile, syntaxErrors))
                        }
                    }
                }
            }

        actionResultOpt match {
            case Some(actionSuccess) => actionSuccess
            case None =>
                Future.successful(ActionFailure("Check command line parameters"))
        }
    }

}
