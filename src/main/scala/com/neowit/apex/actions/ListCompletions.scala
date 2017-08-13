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

import com.neowit.apex.ProjectsCache
import com.neowit.apex.parser.{Member, MemberJsonSupport}
import com.neowit.response.ListCompletionsResult
import com.neowit.utils.JsonSupport
import com.neowit.apexscanner.FileBasedDocument
import com.neowit.apexscanner.symbols.Symbol

import scala.concurrent.{ExecutionContext, Future}

object ListCompletions {

    def symbolToMember(symbol: Symbol): Member = {
        new Member {
            override def getLocation: Option[MemberJsonSupport.Location] = {

                //     case class Location ( file: Path, line: Option[Int], column: Option[Int], identity: String )
                val location =
                    MemberJsonSupport.Location(
                        symbol.symbolLocation.path,
                        Option(symbol.symbolLocation.range.start.line),
                        Option(symbol.symbolLocation.range.start.col),
                        symbol.symbolName
                    )
                Option(location)
            }

            override def isStatic: Boolean = symbol.symbolIsStatic

            override def getSignature: String = symbol.symbolLabel

            override def getType: String = symbol.symbolValueType.getOrElse("")

            override def getIdentity: String = symbol.symbolName
        }
    }
}
class ListCompletions extends ApexActionWithReadOnlySession with JsonSupport {

    protected override def act()(implicit ec: ExecutionContext): Future[ActionResult] = {
        val config = session.getConfig

        val resultOpt =
            for {
                projectDir <- config.projectDirOpt
                project <- ProjectsCache.getProject(projectDir, session)
                filePath <- config.getRequiredProperty("currentFileContentPath")
                line <- config.getRequiredProperty("line")
                column <- config.getRequiredProperty("column")
            } yield {
                // path to file in project
                //val currentFilePath = config.getRequiredProperty("currentFilePath")
                // path to current file content (file in project may not be saved)
                val inputFile = new File(filePath)
                val inputFilePath = inputFile.toPath
                val document = FileBasedDocument(inputFilePath)
                val completions = new com.neowit.apexscanner.scanner.actions.ListCompletions(project)
                completions.list(document, line.toInt, column.toInt - 1).map{res =>
                    val members = res.options.map(ListCompletions.symbolToMember)
                    val resultList = sortMembers(members.toList)
                    ActionSuccess(ListCompletionsResult(resultList))
                }
            }
        resultOpt match {
            case Some(actionSuccess) => actionSuccess
            case None =>
                Future.successful(ActionFailure("Check command line parameters"))
        }
    }


    private def sortMembers(members: List[Member]): List[Member] = {
        val res = members.sortWith(
            (m1, m2) => {
                //static members on top then alphabetically
                val m1static = if (m1.isStatic) 1 else 0
                val m2static = if (m2.isStatic) 1 else 0
                val staticRes = m1static - m2static
                if (0 == staticRes) {
                    m1.getIdentity.compareTo(m2.getIdentity) < 0
                } else {
                    staticRes > 0
                }
            }
        )
        res
    }

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

        override def getSummary: String = "list potential candidates for code completion"

        override def getName: String = "listCompletions"
    }

}
