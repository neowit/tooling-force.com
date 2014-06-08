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

import com.neowit.utils._
import com.neowit.apex.actions.{RetrieveError, ActionFactory}
import scala.concurrent.{ExecutionContext, Future}
import ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration

object Runner extends Logging {
    //var appConfig = Config.getConfig(new BasicConfig())
    def main(args: Array[String]) {
        val runner = new Executor()
        runner.execute(args)
    }
}

class Executor extends Logging {
    val basicConfig = new BasicConfig()

    def execute(args: Array[String]) {
        if (args.isEmpty) {
            help()
        } else {
            var isGoodConfig = false
            try {
                basicConfig.load(args.toList)
                run()
                isGoodConfig = true
            } catch {
                case ex: InvalidCommandLineException => basicConfig.help()
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
                    basicConfig.responseWriter.println("RESULT=FAILURE")
                    for (msg <- messages) {
                        basicConfig.responseWriter.println("ERROR", Map("filePath" -> msg.getFileName, "text" -> msg.getProblem))
                    }
                    isGoodConfig = true
                case e: com.sforce.soap.partner.fault.ApiFault =>
                    logger.error(e)
                    basicConfig.responseWriter.println("RESULT=FAILURE")
                    basicConfig.responseWriter.println(new ResponseWriter.Message(ResponseWriter.ERROR, e.getExceptionMessage, Map("code" -> e.getExceptionCode.toString)))
                    isGoodConfig = true
                case ex: Throwable =>
                    //val response = appConfig.responseWriter with Response
                    logger.error(ex)
                    logger.error(ex.printStackTrace())
                    basicConfig.responseWriter.println("RESULT=FAILURE")
                    basicConfig.responseWriter.println("ERROR", Map("text" -> ex.getMessage))
                    isGoodConfig = true
            } finally {
                if (isGoodConfig) {
                    basicConfig.responseWriter.close()
                }
            }
        }
    }

    private def run () {
        //logger.debug("Server Timestamp" + session.getServerTimestamp)
        val start = System.currentTimeMillis
        //report usage if allowed
        val usage = new UsageReporter(basicConfig)
        val usageFuture = Future {
            usage.report()
        }

        ActionFactory.getAction(basicConfig, basicConfig.action) match {
            case Some(action) => action.act()
            case None =>
        }

        //if operation took too little for usage report to complete, then do NOT delay user by waiting for usage report completion
        //scala.concurrent.Await.result(usageFuture, Duration.Inf)
        val diff = System.currentTimeMillis - start
        logger.info("# Time taken: " + diff / 1000.0 +  "s")

    }

    def help(actionName: String) {
        val action = ActionFactory.getAction(null, actionName).get
        System.out.println("\n--action=" + actionName)
        System.out.println(" " + action.getSummary)
        if (!action.getParamNames.isEmpty) {
            System.out.println("Additional parameters:")
            for(paramName <- action.getParamNames) {
                System.out.println(action.getParamDescription(paramName))
            }
        }
        if (!action.getExample.isEmpty) {
            System.out.println("Example:")
            System.out.println(action.getExample)
        }
    }

    def help() {
        basicConfig.help()
        System.out.println("Available Actions")
        for (actionName <- ActionFactory.getActionNames) {
            System.out.println("    --" + actionName + " see --help=" + actionName)
        }
    }
}
