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
            case "logLevel" => "[optional], if not specified then defaults to None. Accepted values: 'None', 'Debugonly', 'Db', 'Profiling', 'Callout', 'Detail'  "
        }

        override def getParamNames: List[String] = List("codeFile", "logFile", "logLevel")

        override def getSummary: String = "Executes the specified block of Apex anonymously"

        override def getName: String = "executeAnonymous"
    }

    protected def act()(implicit ec: ExecutionContext): Future[ActionResult] = {
        val codeFile = new File(config.getRequiredProperty("codeFile").get)
        val apexCode = FileUtils.readFile(codeFile).getLines().mkString("\n")
        val (executeAnonymousResult, log) = session.executeAnonymous(apexCode)


        //val builderWithSuccess = new ActionResultBuilder(SUCCESS)
        //val builderWithFailure = new ActionResultBuilder(FAILURE)
        val logFileOpt =
        if (!log.isEmpty) {
            val logFile = getProjectConfig.getLogFile
            FileUtils.writeFile(log, logFile)
            //responseWriter.println("LOG_FILE=" + logFile.getAbsolutePath)
            Option(logFile)
        } else {
            None
        }

        val actionResult =
            if (executeAnonymousResult.isSuccess) {
                //responseWriter.println("RESULT=SUCCESS")
                ActionSuccess(ExecuteAnonymousResult(errors = Nil, stackTraceOpt = None, logFileOpt = logFileOpt))
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
                ActionFailure(ExecuteAnonymousResult(errors = errorBuilder.result(), stackTraceOpt = Option(executeAnonymousResult.getExceptionStackTrace), logFileOpt = logFileOpt))
            }


        Future.successful(actionResult)
    }
}
