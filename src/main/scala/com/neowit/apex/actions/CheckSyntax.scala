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

import com.neowit.apexscanner.nodes.Language
import com.neowit.apexscanner.scanner.actions.SyntaxChecker
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

        val actionResultOpt =
            for (   filePath <- config.getRequiredProperty("currentFileContentPath");
                    sourceFilePath <- config.getRequiredProperty("currentFilePath")
            ) yield {
                val inputFile = new File(filePath)
                if (!filePath.endsWith(".cls") && !filePath.endsWith(".trigger")) {
                    // unsupported file extension
                    Future.successful(ActionFailure("Only .cls and .trigger files supported"))
                } else {
                    val checker = new SyntaxChecker(secondaryLanguages = Set(Language.SOQL))
                    val path = inputFile.toPath

                    checker.check(path, _ => false, _ => Unit).map{results =>
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
