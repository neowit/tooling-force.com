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

import com.neowit.utils.{FileUtils, JsonSupport}
import com.neowit.response._

import scala.annotation.tailrec
import scala.concurrent.{ExecutionContext, Future}
/**
 * 'executeAnonymous' action Executes the specified block of Apex anonymously and returns the result
 * Extra command line params:
 * --codeFile=/path/to/file with apex code to execute
 * --logFile=/path/to/file where log shall be stored
 */
class ExecuteAnonymous extends ApexActionWithReadOnlySession with JsonSupport {

    override def getHelp: ActionHelp = new ActionHelp {
        override def getExample: String = ""

        override def getParamDescription(paramName: String): String = paramName match {
            case "codeFile" => "full path to file containing piece of Apex code to run"
            case "logFile" => "[optional] full path to file where resulting log file will be saved"
            case "while" =>
                """
                  | Allows to repeat anonymous block if specified string is present in log output
                  | For instance - you need to delete 10K records matching certain condition in batches of 100 records.
                  |
                  | Anonymous block may look like:
                  |
                  |     Database.delete([select Id from MyObject__c where Status__c = 'To Delete' limit 100]);
                  |     if ([select count() from MyObject__c where Status__c = 'To Delete' limit 100 ] > 0) {
                  |         System.debug('KEEP RUNNING')
                  |     }
                  |
                  | Command line looks like:
                  | $ java -jar tooling-force.com.jar --codeFile='path/goes/here' --while='KEEP RUNNING' ...
                  |
                  | In the above example selected block of code will be executed repeatedly as long as
                  | substring 'KEEP RUNNING' is present in the output log of "Execute Anonymous" block.
                """.stripMargin
            case "logLevel" =>
                """
                  |[optional],
                  | if not specified then defaults to None.
                  | If --while='condition' is provided then defaults to 'Debugonly'.
                  | Accepted values: 'None', 'Debugonly', 'Db', 'Profiling', 'Callout', 'Detail'
                  | """.stripMargin
        }

        override def getParamNames: List[String] = List("codeFile", "logFile", "logLevel")

        override def getSummary: String = "Executes the specified block of Apex anonymously"

        override def getName: String = "executeAnonymous"
    }

    private def containsCondition(log: String, condition: String): Boolean = {
        //skip first lines with anonymous block definition
        log.split("""\r?\n""").exists(line => !line.contains("Execute Anonymous:") && line.contains(condition))
    }

    @tailrec
    private def executeOnce(apexCode: String, whileSubstringOpt: Option[String], logs: List[String]): (com.sforce.soap.apex.ExecuteAnonymousResult, String, List[String]) = {
        val (executeAnonymousResult, log) = session.executeAnonymous(apexCode)
        whileSubstringOpt match {
            case Some(condition) if executeAnonymousResult.isSuccess && containsCondition(log, condition)=>
                executeOnce(apexCode, whileSubstringOpt, log :: logs)
            case _ =>
                (executeAnonymousResult, log, log :: logs)
        }
    }
    protected def act()(implicit ec: ExecutionContext): Future[ActionResult] = {
        val codeFile = new File(config.getRequiredProperty("codeFile"))
        val apexCode = FileUtils.readFile(codeFile).getLines().mkString("\n")
        val whileSubstringOpt = config.getProperty("while")


        val (executeAnonymousResult, lastLog, allLogs) = executeOnce(apexCode, whileSubstringOpt, Nil)

        val logFileOpt =
            if (allLogs.nonEmpty) {
                val logFile = getProjectConfig.getLogFile
                val separatorString = List.fill(80)("=").mkString("") + "\n"
                FileUtils.writeFile(allLogs.mkString(separatorString), logFile)
                //responseWriter.println("LOG_FILE=" + logFile.getAbsolutePath)
                Option(logFile)
            } else {
                None
            }

        val actionResult =
            if (executeAnonymousResult.isSuccess) {
                //responseWriter.println("RESULT=SUCCESS")
                ActionSuccess(ExecuteAnonymousResult(errors = Nil, stackTraceOpt = None, logFileOpt = logFileOpt, allLogs.length))
            } else {
                //responseWriter.println("RESULT=FAILURE")
                val errorBuilder = List.newBuilder[DeploymentError]

                if (executeAnonymousResult.isCompiled) {
                    //non compile error
                    //responseWriter.startSection("ERROR LIST")
                    //responseWriter.println("ERROR", Map("text" -> executeAnonymousResult.getExceptionMessage))
                    errorBuilder += GenericError(problemType = GenericDeploymentError, problem = executeAnonymousResult.getExceptionMessage)

                    //config.responseWriter.endSection("ERROR LIST")
                    //responseWriter.println("STACK_TRACE", Map("text" -> executeAnonymousResult.getExceptionStackTrace))

                } else {
                    //compile error
                    //responseWriter.startSection("ERROR LIST")
                    val line = executeAnonymousResult.getLine
                    val column = executeAnonymousResult.getColumn
                    val problem = executeAnonymousResult.getCompileProblem
                    //responseWriter.println("ERROR", Map("line" -> line, "column" -> column, "text" -> problem))

                    errorBuilder +=
                        ErrorWithLocation(
                            problemType = DeploymentCompileError,
                            problem = problem,
                            location = Location("", line, column)
                        )
                    //config.responseWriter.endSection("ERROR LIST")
                }
                ActionFailure(
                    ExecuteAnonymousResult(
                        errors = errorBuilder.result(),
                        stackTraceOpt = Option(executeAnonymousResult.getExceptionStackTrace),
                        logFileOpt = logFileOpt,
                        allLogs.length
                    )
                )
            }


        Future.successful(actionResult)
    }
}
