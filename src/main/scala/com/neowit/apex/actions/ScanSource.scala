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

import com.neowit.apex.parser.SourceScanner
import com.neowit.utils.FileUtils

import scala.concurrent.{ExecutionContext, Future}

object SourceScannerCache {
    private val scannerByProject = Map.newBuilder[File, SourceScanner]
    def addScanResult(project: File, scanner: SourceScanner): Unit = {
        scannerByProject += (project -> scanner)

    }
    def getScanResult(project: File): Option[SourceScanner] = {
        scannerByProject.result().get(project)
    }

}

class ScanSource extends ApexActionWithReadOnlySession {
    val APEX_EXTENSIONS = Set("cls", "trigger")

    protected override def act()(implicit ec: ExecutionContext): Future[ActionResult] = {
        //val config = session.getConfig

        val classes = getRecognisedApexFiles
        val actionResult = scan(classes)
        Future.successful(actionResult)
    }

    def scan(files: List[File]): ActionResult = {
        val config = session.getConfig
        config.projectDirOpt match {
            case Some(projectDir) =>
                var completeScan = false
                val scanner = SourceScannerCache.getScanResult(projectDir) match {
                    case Some(_scanner) => _scanner
                    case None => completeScan = true; new SourceScanner(files)
                }

                scanner.scan(completeScan)
                SourceScannerCache.addScanResult(projectDir, scanner)
                ActionSuccess()
            case None =>
                ActionFailure("Nothing to scan when no --projectPath provided")
        }

    }
    override def getHelp: ActionHelp = new ActionHelp {
        override def getExample: String = ""

        override def getParamDescription(paramName: String): String = {
            paramName match {
                case "projectPath" => "full path to project folder"
                case _ => ""
            }
        }

        override def getParamNames: List[String] = List("projectPath")

        override def getSummary: String = "scan project files to build completion tree"

        override def getName: String = "scanSource"
    }

    def getClassFiles:List[File] = {
        val config = session.getConfig

        val allFiles = config.srcDirOpt match {
            case Some(srcDir) =>
                FileUtils.listFiles(srcDir).filter(
                    //remove all non apex files
                    file => APEX_EXTENSIONS.contains(FileUtils.getExtension(file))
                ).toSet
            case None => Set()
        }

        val classFiles = allFiles.filter("cls" == FileUtils.getExtension(_))
        classFiles.toList
    }

    def getRecognisedApexFiles:List[File] = {
        val config = session.getConfig

        val allFiles = config.srcDirOpt match {
            case Some(srcDir) =>
                FileUtils.listFiles(srcDir).filter(
                    //remove all non apex files
                    file => APEX_EXTENSIONS.contains(FileUtils.getExtension(file))
                ).toSet
            case None => Set()
        }

        allFiles.toList
    }
}
