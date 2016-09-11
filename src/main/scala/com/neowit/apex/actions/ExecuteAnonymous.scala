package com.neowit.apex.actions

import java.io.File
import com.neowit.utils.FileUtils

/**
 * 'executeAnonymous' action Executes the specified block of Apex anonymously and returns the result
 * Extra command line params:
 * --codeFile=/path/to/file with apex code to execute
 * --logFile=/path/to/file where log shall be stored
 */
class ExecuteAnonymous extends ApexActionWithReadOnlySession {

    override def getHelp: ActionHelp = new ActionHelp {
        override def getExample: String = ""

        override def getParamDescription(paramName: String): String = paramName match {
            case "codeFile" => "full path to file containing piece of Apex code to run"
            case "logFile" => "[optional] full path to file where resulting log file will be saved"
            case "logLevel" => "[optional], if not specified then defaults to None. Accepted values: 'None', 'Debugonly', 'Db', 'Profiling', 'Callout', 'Detail'  "
        }

        override def getParamNames: List[String] = List("codeFile", "logFile", "logLevel")

        override def getSummary: String = "Executes the specified block of Apex anonymously"

        override def getName: String = "executeAnonymous"
    }

    def act(): Unit = {
        val codeFile = new File(config.getRequiredProperty("codeFile").get)
        val apexCode = FileUtils.readFile(codeFile).getLines().mkString("\n")
        val (executeAnonymousResult, log) = session.executeAnonymous(apexCode)

        if (executeAnonymousResult.isSuccess) {
            responseWriter.println("RESULT=SUCCESS")
        } else {
            responseWriter.println("RESULT=FAILURE")

            if (executeAnonymousResult.isCompiled) {
                //non compile error
                responseWriter.startSection("ERROR LIST")
                responseWriter.println("ERROR", Map("text" -> executeAnonymousResult.getExceptionMessage))
                config.responseWriter.endSection("ERROR LIST")
                responseWriter.println("STACK_TRACE", Map("text" -> executeAnonymousResult.getExceptionStackTrace))
            } else {
                //compile error
                responseWriter.startSection("ERROR LIST")
                val line = executeAnonymousResult.getLine
                val column = executeAnonymousResult.getColumn
                val problem = executeAnonymousResult.getCompileProblem
                responseWriter.println("ERROR", Map("line" -> line, "column" -> column, "text" -> problem))
                config.responseWriter.endSection("ERROR LIST")
            }
        }

        if (!log.isEmpty) {
            val logFile = config.getLogFile
            FileUtils.writeFile(log, logFile)
            responseWriter.println("LOG_FILE=" + logFile.getAbsolutePath)
        }
    }
}
