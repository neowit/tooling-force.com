package com.neowit.apex.actions

import java.io.File

import com.neowit.apex.parser.SourceScanner
import org.antlr.v4.runtime._

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

    override protected def act(): Unit = {
        val config = session.getConfig

        for (   filePath <- config.getRequiredProperty("currentFileContentPath");
                sourceFilePath <- config.getRequiredProperty("currentFilePath")
                //line <- config.getProperty("line");
                //column <- config.getProperty("column")
        ) yield {
            val inputFile = new File(filePath)
            val scanner = new SourceScanner(List(inputFile))
            val errorListener = new SyntaxErrorListener()
            scanner.parseOne(inputFile, Some(errorListener))
            val errors = errorListener.result
            if (errors.isEmpty) {
                config.responseWriter.println("RESULT=SUCCESS")
            } else {
                responseWriter.println("RESULT=FAILURE")
                responseWriter.startSection("ERROR LIST")
                val pathToReport = session.getRelativePath(new File(sourceFilePath))
                errors.foreach{e =>
                    responseWriter.println("ERROR", Map("filePath" -> pathToReport, "line" -> e.line, "column" -> e.charPositionInLine, "text" -> e.msg))
                }
                config.responseWriter.endSection("ERROR LIST")

            }
        }

    }

    class SyntaxErrorListener extends BaseErrorListener {
        private val errors = List.newBuilder[SyntaxError]
        override def syntaxError(
                                    recognizer: Recognizer[_, _],
                                    offendingSymbol: scala.Any,
                                    line: Int,
                                    charPositionInLine: Int,
                                    msg: String, e: RecognitionException
                                ): Unit = {
            //super.syntaxError(recognizer, offendingSymbol, line, charPositionInLine, msg, e)
            logger.error(s"Syntax error:: line: $line, pos: $charPositionInLine, msg: $msg")
            errors += SyntaxError(offendingSymbol, line, charPositionInLine, msg)
        }
        def result: List[SyntaxError] = errors.result()
    }
    case class SyntaxError(offendingSymbol: scala.Any,
                           line: Int,
                           charPositionInLine: Int,
                           msg: String)
}
