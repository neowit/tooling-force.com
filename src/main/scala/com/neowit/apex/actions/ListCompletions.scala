package com.neowit.apex.actions

import java.io.File

import com.neowit.apex.completion.{AutoComplete}
import com.neowit.apex.parser.Member
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
            val scanSourceAction = new ScanSource(basicConfig)
            //exclude current file
            val currentFilePath = config.getRequiredProperty("currentFilePath")
            val classes = scanSourceAction.getClassFiles.filterNot(_.getAbsolutePath == currentFilePath)
            //scan all project files (except the current one)
            val scanner = new ScanSource(basicConfig)
            scanner.scan(classes)

            val cachedTree:ApexTree = SourceScannerCache.getScanResult(config.projectDir)  match {
                case Some(sourceScanner) => sourceScanner.getTree
                case None => Map()
            }
            val completion = new AutoComplete(inputFile, line.toInt, column.toInt, cachedTree)
            val members = completion.listOptions
            //dump completion options into a file - 1 line per option
            config.responseWriter.println("RESULT=SUCCESS")
            config.responseWriter.println(sortMembers(members).map(_.toJson).mkString("\n"))
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
