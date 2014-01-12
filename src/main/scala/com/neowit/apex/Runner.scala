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

object Runner extends Logging {
    val appConfig = Config.getConfig

    def main(args: Array[String]) {
        if (args.isEmpty) {
            appConfig.help()

        } else {
            try {
                appConfig.load(args.toList)
                run()
            } catch {
                case ex: InvalidCommandLineException => appConfig.help()
                case ex: MissingRequiredConfigParameterException =>
                    logger.error(ex.getMessage)
                case ex: Throwable =>
                    //val response = appConfig.responseWriter with Response
                    logger.error(ex)
                    appConfig.responseWriter.println("RESULT=FAILURE")
            } finally {
                appConfig.responseWriterClose
            }
        }
    }
    def run () {
        val start = System.currentTimeMillis

        val session = Session(appConfig)
        //logger.debug("Server Timestamp" + session.getServerTimestamp)

        ActionFactory.getAction(session, session.getConfig.action) match {
          case Some(action) => action.act
          case None =>
        }

        val diff = System.currentTimeMillis - start
        logger.info("# Time taken: " + diff / 1000.0 +  "s")

        //Tester.runTest(session)

    }


}
