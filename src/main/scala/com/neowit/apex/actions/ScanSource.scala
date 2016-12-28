package com.neowit.apex.actions

import java.io.File

import com.neowit.apex.parser.SourceScanner
import com.neowit.utils.FileUtils

object SourceScannerCache {
    val scannerByProject = Map.newBuilder[File, SourceScanner]
    def addScanResult(project: File, scanner: SourceScanner): Unit = {
        scannerByProject += (project -> scanner)

    }
    def getScanResult(project: File): Option[SourceScanner] = {
        scannerByProject.result().get(project)
    }

}

class ScanSource extends ApexActionWithReadOnlySession {
    val APEX_EXTENSIONS = Set("cls", "trigger")

    override def act(): Unit = {
        //val config = session.getConfig

        val classes = getClassFiles
        scan(classes)
    }

    def scan(files: List[File]): Unit = {
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
            case None =>
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
}
