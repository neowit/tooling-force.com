package com.neowit.apex.actions

import java.io.File

import com.neowit.apex.completion.AutoComplete
import com.neowit.apex.parser.ApexTree

import spray.json._
import com.neowit.apex.parser.Location._
/**
  * Author: Andrey Gavrikov (westbrook)
  * Date: 01/11/2016
  */
class FindDefinition extends ApexActionWithReadOnlySession {
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

        override def getName: String = "findDefinition"
    }

    //this method should implement main logic of the action
    override protected def act(): Unit = {
        /*
        val config = session.getConfig

        for (   filePath <- config.getRequiredProperty("currentFileContentPath");
                line <- config.getRequiredProperty("line");
                column <- config.getRequiredProperty("column")
        ) yield {
            val inputFile = new File(filePath)
            val scanner = new ScanSource().load[ScanSource](session)
            val classes = scanner.getClassFiles
            //scan all project files
            //val scanner = new ScanSource().load[ScanSource](session.basicConfig)
            scanner.scan(classes)

            val cachedTree:ApexTree = SourceScannerCache.getScanResult(config.projectDir)  match {
                case Some(sourceScanner) => sourceScanner.getTree
                case None => new ApexTree
            }
            val completion = new AutoComplete(inputFile, line.toInt, column.toInt, cachedTree, session)
            val definition = completion.findDefinition
            //dump completion options into a file - 1 line per option
            config.responseWriter.println("RESULT=SUCCESS")
            definition match {
                case Some(_definitionWithType) =>
                    val definitionMember = _definitionWithType.definitionMember
                    definitionMember.getLocation match {
                        case Some(location) =>
                            config.responseWriter.println(location.toJson)
                        case None => Map()
                            config.responseWriter.println(Map())
                    }
                case None =>
            }
            //config.responseWriter.println(sortMembers(members).map(_.toJson).mkString("\n"))
        }
        */
    }


}
