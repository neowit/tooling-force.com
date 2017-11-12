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

package com.neowit.webserver

import java.util.concurrent.TimeUnit
import javax.servlet.http.{HttpServletRequest, HttpServletResponse}

import com.neowit.utils.Logging
import org.eclipse.jetty.server.handler.AbstractHandler
import org.eclipse.jetty.server.{Handler, Request, Server}

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success, Try}

/**
  * Author: Andrey Gavrikov
  */

sealed trait HandleStatus
case object Handled extends HandleStatus
case object NotHandled extends HandleStatus

trait EmbeddedJettyHandler {
    def handle(target: String, baseRequest: Request, request: HttpServletRequest, response: HttpServletResponse): Future[HandleStatus]
}

//http://www.eclipse.org/jetty/documentation/current/embedding-jetty.html
object EmbeddedJetty extends Logging {
    private var serverOpt: Option[EmbeddedJetty] = None
    private val handlersMap = new collection.mutable.HashMap[String, EmbeddedJettyHandler]()

    def start(port: Int): Either[String, EmbeddedJetty] = {
        serverOpt match {
            case Some(_server) =>
                Right(_server)
            case None =>
                Try(new EmbeddedJetty(port, _internalHandler)) match {
                    case Success(_server) => serverOpt = Option(_server)
                        Right(_server)
                    case Failure(ex) => Left(ex.getMessage)
                }
        }
    }

    /**
      * server can only be stopped when there are no handlers left
      * @return
      */
    def stop(): Boolean = {
        serverOpt match {
            case Some(_server) =>
                // check if no handlers left and we can stop
                if ( isOkToStop ) {
                    //1sec delay to give server a chance to complete current response
                    delayedExecution(1) {
                        logger.debug("stopping server")
                        _server.stop()
                    }
                    serverOpt = None
                    true
                } else {
                    // some handlers still exist, unprocessed requests may be in progress
                    false
                }
            case None => // nothing to stop
                false
        }
    }

    def hasHandler(id: String): Boolean = {
        handlersMap.contains(id)
    }

    def addHandler(id: String, handler: EmbeddedJettyHandler): Unit = {
        handlersMap += (id -> handler)
        ()
    }

    def removeHandler(id: String): Option[EmbeddedJettyHandler] = {
        handlersMap.remove(id)
    }

    protected def isOkToStop: Boolean = {
        handlersMap.isEmpty
    }

    private val _internalHandler = new AbstractHandler {
        override def handle(target: String, baseRequest: Request, request: HttpServletRequest, response: HttpServletResponse): Unit = {
            for (handler <- handlersMap.values) {
                // only call next handler if request has not already been handled
                if ( !baseRequest.isHandled ) {
                    handler.handle(target, baseRequest, request, response).map{
                        case Handled =>
                            if (! baseRequest.isHandled ) {
                                baseRequest.setHandled(true)
                            }
                        case _ => // continue to next handler
                    }
                }
            }

        }
    }
    private def delayedExecution(delaySec: Long) (codeUnit: => Unit): Future[Unit] = {
        Future.successful {
            TimeUnit.SECONDS.sleep(delaySec)
            codeUnit
        }
    }
}

class EmbeddedJetty private (port: Int, handler: Handler ) {

    val server = new Server(port)
    server.setStopAtShutdown(true)

    server.setHandler(handler)

    server.start()
    //server.join() // commented out because do not need to join current thread

    protected def isAlive: Boolean = {
        server.isRunning || server.isStarting
    }

    protected def stop(): Unit = {
        server.stop()
    }
}

