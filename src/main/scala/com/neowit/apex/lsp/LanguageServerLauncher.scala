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

package com.neowit.apex.lsp

import java.net.ServerSocket
import java.util.concurrent.{ExecutorService, Executors}

import com.neowit.utils.Logging

import scala.concurrent.ExecutionContext

/**
  * Created by Andrey Gavrikov 
  */
object LanguageServerLauncher extends Logging {
    private implicit val ec: ExecutionContext = scala.concurrent.ExecutionContext.Implicits.global


    def main(args: Array[String]): Unit = {
        logger.debug("command line: " + args.mkString(" "))

        import LanguageServerConfig._

        // consume arguments
        LanguageServerConfig.parse(args, LanguageServerConfig()) match {
            case Some(config) =>
                if (config.authConfigDirPath.isEmpty && config.authConfigPath.isEmpty) {
                    parser.reportError("'authConfigDirPath' or 'authConfigPath' is required.")
                } else {
                    config.communicationMethod match {
                        case "socket" =>
                            //val port = config.port
                            val server = new SocketServer(poolSize = 2, config)
                            server.start()
                        case "stdio" =>
                            // when using STDIO we have to disable STDOUT log otherwise LSP Client gets confused
                            //val loggerContext: LoggerContext = LoggerFactory.getILoggerFactory.asInstanceOf[LoggerContext]
                            //val rootLogger = loggerContext.getLogger("root")
                            //rootLogger.setLevel(Level.DEBUG)
                            System.setProperty("STDOUT_LEVEL", "debug")

                            val server = new ApexLanguageServerBase(System.in, System.out, config)
                            server.start()
                        case _ =>
                            //parser.showUsageAsError()
                    }
                }
            case None =>
                // bad config
                //parser.showUsageAsError()
        }
    }

}

private class SocketServer(poolSize: Int, config: LanguageServerConfig)(implicit val ex: ExecutionContext) {thisServer =>
    val serverSocket = new ServerSocket(config.port)
    private val pool: ExecutorService = Executors.newFixedThreadPool(poolSize)

    def start(): Unit = {
        println(s"READY to accept connection on localhost:${config.port}") // this line is important, otherwise client does not know that server has started
        try {
            while (true) {
                // This will block until a connection comes in.
                val socket = serverSocket.accept()
                //pool.execute(new SocketLanguageServer(socket))
                val langServer = new ApexLanguageServerBase(socket.getInputStream, socket.getOutputStream, config) with Runnable {
                    override implicit val ex: ExecutionContext = thisServer.ex

                    override protected def isConnectionOpen: Boolean = super.isConnectionOpen && !socket.isClosed

                    override def shutdown(): Unit = {
                        socket.close()
                        super.shutdown()
                    }

                    override def run(): Unit = start()

                    override def start(): Unit = {
                        logger.debug("Starting SocketServer")
                        super.start()
                    }
                }
                pool.execute(langServer)
            }
        } finally {
            shutdown()
        }
    }

    def shutdown(): Unit = {
        if (!pool.isShutdown)
            pool.shutdown()
    }
}