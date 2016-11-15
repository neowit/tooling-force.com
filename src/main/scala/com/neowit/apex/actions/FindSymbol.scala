package com.neowit.apex.actions

import java.io.File
import java.nio.file.{Files, Paths}

import com.neowit.apex.Session
import com.neowit.apex.completion.{UnresolvedApexTokenDefinition, _}
import com.neowit.apex.completion.models.ApexModel
import com.neowit.apex.parser._
import spray.json._
import com.neowit.apex.parser.Location._
import com.neowit.apex.parser.antlr.ApexcodeParser
import com.neowit.apex.parser.antlr.ApexcodeParser.ClassDeclarationContext
import com.neowit.utils.ResponseWriter
import com.neowit.utils.ResponseWriter.Message
import org.antlr.v4.runtime.CommonTokenStream
import org.antlr.v4.runtime.tree.ParseTreeWalker
/**
  * Author: Andrey Gavrikov (westbrook)
  * Date: 01/11/2016
  */
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
    override protected def act(): Unit = {
        val config = session.getConfig

        for (   filePath <- config.getRequiredProperty("currentFileContentPath");
                line <- config.getRequiredProperty("line");
                column <- config.getRequiredProperty("column")
        ) yield {
            if (! Files.isReadable(Paths.get(filePath))) {
                config.responseWriter.println("RESULT=FAILURE")
                config.responseWriter.println(new Message(ResponseWriter.ERROR, s"'currentFileContentPath' must point to readable file"))
                return
            }
            val inputFile = new File(filePath)
            val scanner = new ScanSource().load[ScanSource](session)
            //provide alternative location for current file (it may not be saved in project location)
            val currentFilePath = config.getRequiredProperty("currentFilePath")
            val classes = scanner.getClassFiles.filterNot(_.getAbsolutePath == currentFilePath) ++ List(new File(filePath))
            scanner.scan(classes)

            val cachedTree:ApexTree = SourceScannerCache.getScanResult(config.projectDir)  match {
                case Some(sourceScanner) => sourceScanner.getTree
                case None => new ApexTree
            }

            val completion = new AutoComplete(inputFile, line.toInt, column.toInt, cachedTree, session)
            val definitionOpt = completion.getDefinition
            config.responseWriter.println("RESULT=SUCCESS")

            definitionOpt match {
                case Some(definition) =>
                    getSymbolLocation(definition, completion) match {
                        case Some(location) =>
                            config.responseWriter.println(location.toJson)
                        case None =>
                            println("Not local resource, no location available")
                            config.responseWriter.println("{}")
                    }
                case None =>
                    println("Definition not found")
                    config.responseWriter.println("{}")
            }
            /*
            val resolvedExpression = completion.resolveApexDefinition(definitionOpt)
            resolvedExpression.definitionMemberOpt match {
                case Some(member) =>
                    member.getLocation match {
                        case Some(location) =>
                            config.responseWriter.println(location.toJson)
                        case None =>
                            println("Not local resource, no location available")
                            config.responseWriter.println(Map())
                    }
                case _ =>
                    println("Definition not found")
                    config.responseWriter.println(Map())

            }
            */
        }
    }

    private def getSymbolLocation(definition: TokenDefinition, completion: AutoComplete): Option[Location] = {
        definition match {
            case ApexTokenDefinition(definitionMember, typeMember) =>
                definitionMember.getLocation
            case ApexTokenDefinitionWithContext(definitionMember, typeMember, _, _, _) =>
                definitionMember.getLocation
            case unresolvedDefinition @ UnresolvedApexTokenDefinition(extractor, fullApexTree, expressionTokens) =>
                findSymbolDefinition(unresolvedDefinition, completion) match {
                    case Some(member) => member.getLocation
                    case None => None
                }
            case _ =>
                None
        }
    }

    private def findSymbolDefinition(unresolvedDefinition: UnresolvedApexTokenDefinition, completion: AutoComplete): Option[Member] = {
        val resolvedExpression = completion.resolveApexDefinition(Option(unresolvedDefinition))
        resolvedExpression.definitionMemberOpt
    }

}

