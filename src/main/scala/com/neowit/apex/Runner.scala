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
import com.neowit.utils.ResponseWriter.Message

object Runner extends Logging {
    val appConfig = Config.getConfig

    def main(args: Array[String]) {
        if (args.isEmpty) {
            help()
        } else {
            var isGoodConfig = false
            try {
                appConfig.load(args.toList)
                run()
                isGoodConfig = true
            } catch {
                case ex: InvalidCommandLineException => appConfig.help()
                case ex: MissingRequiredConfigParameterException =>
                    appConfig.getProperty("help") match {
                        case Some(actionName) =>
                            //display help for specific action
                            help(actionName)
                        case _ =>
                            if (args.indexOf("--help") >=0) {
                                help()
                            } else {
                                logger.error(ex.getMessage)
                                throw ex
                            }
                    }
                case e: RetrieveError =>
                    val messages = e.retrieveResult.getMessages
                    appConfig.responseWriter.println("RESULT=FAILURE")
                    for(msg <- messages) {
                        appConfig.responseWriter.println("ERROR", Map("filePath" -> msg.getFileName, "text" -> msg.getProblem))
                    }
                    isGoodConfig = true
                case e: com.sforce.soap.partner.fault.ApiFault =>
                    logger.error(e)
                    appConfig.responseWriter.println("RESULT=FAILURE")
                    appConfig.responseWriter.println(new ResponseWriter.Message(ResponseWriter.ERROR, e.getExceptionMessage, Map("code" -> e.getExceptionCode.toString)))
                    isGoodConfig = true
                case ex: Throwable =>
                    //val response = appConfig.responseWriter with Response
                    logger.error(ex)
                    logger.error(ex.printStackTrace())
                    appConfig.responseWriter.println("RESULT=FAILURE")
                    appConfig.responseWriter.println("ERROR", Map("text" -> ex.getMessage))
                    isGoodConfig = true
            } finally {
                if (isGoodConfig) {
                    appConfig.responseWriter.close()
                }
            }
        }
    }
    def run () {
        val start = System.currentTimeMillis

        val session = Session(appConfig)
        //logger.debug("Server Timestamp" + session.getServerTimestamp)
        ActionFactory.getAction(session, session.getConfig.action) match {
            case Some(action) => action.act()
            case None =>
        }

        val diff = System.currentTimeMillis - start
        logger.info("# Time taken: " + diff / 1000.0 +  "s")

    }

    def help(actionName: String) {
        val action = ActionFactory.getAction(null, actionName).get
        println("\n--action=" + actionName)
        println(" " + action.getSummary)
        if (!action.getParamNames.isEmpty) {
            println("Additional parameters:")
            for(paramName <- action.getParamNames) {
                println(action.getParamDescription(paramName))
            }
        }
        if (!action.getExample.isEmpty) {
            println("Example:")
            println(action.getExample)
        }
    }

    def help() {
        appConfig.help()
        println("Available Actions")
        for (actionName <- ActionFactory.getActionNames) {
            println("    --" + actionName + " see --help=" + actionName)
        }
    }
}
