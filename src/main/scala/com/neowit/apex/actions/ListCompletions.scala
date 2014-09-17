package com.neowit.apex.actions

import java.io.File

import com.neowit.apex.completion.AutoComplete
import com.neowit.apex.parser.SourceScanner
import com.neowit.apex.parser.TreeListener.ApexTree
import com.neowit.utils.BasicConfig

class ListCompletions (basicConfig: BasicConfig) extends ApexAction(basicConfig: BasicConfig){

    override def act(): Unit = {
        val config = session.getConfig

        for (   filePath <- config.getRequiredProperty("currentFileContentPath");
                line <- config.getRequiredProperty("line");
                column <- config.getRequiredProperty("column")
        ) yield {
            val inputFile = new File(filePath)
            val cachedTree:ApexTree = SourceScannerCache.getScanResult(config.projectDir) match {
              case Some(sourceScanner) => sourceScanner.getTree
              case None =>
                  //no tree has been cached, load it now
                  val scanSourceAction = new ScanSource(basicConfig)
                  //exclude current file
                  val currentFilePath = config.getRequiredProperty("currentFilePath")
                  val classes = scanSourceAction.getClassFiles.filterNot(_.getAbsolutePath == currentFilePath)
                  //scan all project files (except the current one)
                  val scanner = new ScanSource(basicConfig)
                  scanner.scan(classes)

                  SourceScannerCache.getScanResult(config.projectDir)  match {
                      case Some(sourceScanner) => sourceScanner.getTree
                      case None => Map()
                  }
            }
            val completion = new AutoComplete(inputFile, line.toInt, column.toInt, cachedTree)
            val members = completion.listOptions
            config.responseWriter.println(members.map(_.toJson))
        }
    }

    override def getExample: String = ""

    override def getParamDescription(paramName: String): String = {
        paramName match {
            case "projectPath" => "full path to project folder"
            case "currentFilePath" => "full path to current code file"
            case "currentFileContentPath" => "full path to temp file where current code file content is saved. If current file is saved then can be the same as currentFilePath"
            case "line" => "line of cursor position in the current code file, starts with 1"
            case "column" => "column of cursor position in the current code file, starts with 1"
            case "responseFilePath" => "path to file where completion candidates will be saved in JSON format"
            case _ => ""
        }
    }

    override def getParamNames: List[String] = List("projectPath", "currentFilePath", "currentFileContentPath",
                                                    "line", "column", "responseFilePath")

    override def getSummary: String = "list potential candidates for code completion"

    override def getName: String = "listCompletions"
}
