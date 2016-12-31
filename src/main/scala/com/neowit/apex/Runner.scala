/*
 * Copyright (c) 2014 Andrey Gavrikov.
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

package com.neowit.apex

import java.io.{PrintWriter, StringWriter}

import com.neowit.utils._
import com.neowit.apex.actions._

import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration._
import ExecutionContext.Implicits.global

object Runner extends Logging {
    def main(args: Array[String]): Unit = {
        val runner = new Executor()
        val exitCode = runner.execute(args)
        sys.exit(exitCode)
    }
}

class Executor extends Logging {
    val basicConfig = new BasicConfig()

    def execute(args: Array[String]): Int = {
        var exitCode = 1
        if (args.isEmpty) {
            help()
        } else {
            var isGoodConfig = false
            try {
                basicConfig.load(args.toList)
                run()
                isGoodConfig = true
                exitCode = 0
            } catch {
                case ex: InvalidCommandLineException =>
                    if (null != ex.getMessage) {
                        basicConfig.getResponseWriter.println(ex.getMessage)
                        logger.error(ex.getMessage)
                    }
                    basicConfig.help()
                case ex: ShowHelpException =>
                    /* uncomment to display actual stack trace
                    val sw = new StringWriter
                    ex.printStackTrace(new PrintWriter(sw))
                    val stackTraceStr = sw.toString
                    // dump exception information to log
                    logger.error(ex)
                    logger.error(stackTraceStr)
                    // dump exception information to System.out
                    System.out.println(stackTraceStr)
                    */
                    if (ex.message.nonEmpty) {
                        basicConfig.getResponseWriter.println(ex.message)
                        logger.error(ex.message)
                    }
                    help(ex.help)
                case ex: MissingRequiredConfigParameterException =>
                    basicConfig.getProperty("help") match {
                        case Some(actionName) =>
                            //display help for specific action
                            help(actionName)
                        case _ =>
                            if (args.indexOf("--help") >= 0) {
                                help()
                            } else {
                                logger.error(ex.getMessage)
                                throw ex
                            }
                    }
                case e: RetrieveError =>
                    val messages = e.retrieveResult.getMessages
                    val _responseWriter = basicConfig.getResponseWriter//e.responseWriterOpt.getOrElse(basicConfig.getResponseWriter)
                    _responseWriter.println(ResponseWriter.FAILURE)
                    for (msg <- messages) {
                        _responseWriter.println("ERROR", Map("filePath" -> msg.getFileName, "text" -> msg.getProblem))
                    }
                    isGoodConfig = true
                case e: com.sforce.soap.partner.fault.ApiFault =>
                    logger.error(e)
                    basicConfig.getResponseWriter.println(ResponseWriter.FAILURE)
                    basicConfig.getResponseWriter.println(ResponseWriter.ErrorMessage(e.getExceptionMessage, Map("code" -> e.getExceptionCode.toString)))
                    isGoodConfig = true
                case ex: Throwable =>
                    //val response = appConfig.responseWriter with Response
                    val sw = new StringWriter
                    ex.printStackTrace(new PrintWriter(sw))
                    val stackTraceStr = sw.toString
                    // dump exception information to log
                    logger.error(ex)
                    logger.error(stackTraceStr)
                    // dump exception information to System.out
                    System.out.println(stackTraceStr)
                    //ex.printStackTrace(System.out)

                    basicConfig.getResponseWriter.println(ResponseWriter.FAILURE)
                    basicConfig.getResponseWriter.println("ERROR", Map("text" -> ex.getMessage))
                    isGoodConfig = true
            } finally {
                if (isGoodConfig) {
                    basicConfig.getResponseWriter.close()
                }
            }
        }
        exitCode
    }
    private def run (): Unit = {
        //logger.debug("Server Timestamp" + session.getServerTimestamp)
        if (basicConfig.getProperty("help").isEmpty) {
            val start = System.currentTimeMillis
            //report usage if allowed
            val usage = new UsageReporter(basicConfig)
            val usageFuture = Future {
                usage.report()
            }

            ActionFactory.getAction(basicConfig, basicConfig.action) match {
                case Some(action) =>
                    val actionResultFuture = action.execute()
                    val responseWriter = basicConfig.getResponseWriter
                    Await.result(actionResultFuture, Duration.Inf)
                    actionResultFuture.map{
                        case ActionSuccess(messages) =>
                            responseWriter.println("RESULT=SUCCESS")
                            messages.foreach(responseWriter.println(_))
                        case ActionFailure(messages) =>
                            responseWriter.println("RESULT=FAILURE")
                            messages.foreach(responseWriter.println(_))
                    }
                case None =>
            }
            //if operation took too little for usage report to complete, then do NOT delay user by waiting for usage report completion
            //scala.concurrent.Await.result(usageFuture, Duration.Inf)
            val diff = System.currentTimeMillis - start
            logger.info("# Time taken: " + diff / 1000.0 +  "s")
        } else {
            // looks like --help=<action> has been requested
            // force help display for specified <action>
            throw new MissingRequiredConfigParameterException("")
        }
    }

    def help(actionName: String): Unit = {
        try {
            ActionFactory.getAction(basicConfig, actionName, skipLoading = true) match {
                case Some(x) => help(x.getHelp)
                case None =>
            }
        } catch {
            case ex: ShowHelpException => help(ex.help)
        }
    }
    def help(actionHelp: ActionHelp): Unit = {
        System.out.println("\n--action=" + actionHelp.getName)
        System.out.println(" " + actionHelp.getSummary)
        if (actionHelp.getParamNames.nonEmpty) {
            System.out.println("\nAdditional parameters:")
            for(paramName <- actionHelp.getParamNames) {
                System.out.println(actionHelp.getParamDescription(paramName))
            }
        }
        if (!actionHelp.getExample.isEmpty) {
            System.out.println("Example:")
            System.out.println(actionHelp.getExample)
        }
    }

    def help(): Unit = {
        basicConfig.help()
        System.out.println("Available Actions:")
        for (actionName <- ActionFactory.getActionNames) {
            System.out.println("    - " + actionName + ", see --help=" + actionName)
        }
    }
}
