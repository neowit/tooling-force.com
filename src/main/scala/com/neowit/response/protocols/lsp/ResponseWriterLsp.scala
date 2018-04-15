/*
 * Copyright (c) 2018 Andrey Gavrikov.
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

package com.neowit.response.protocols.lsp

import java.io.{PrintWriter, StringWriter}

import com.neowit.apex.actions.{ActionFailure, ActionResult, ActionSuccess}
import com.neowit.apexscanner.server.protocol.LanguageServer
import com.neowit.apexscanner.server.protocol.messages._
import com.neowit.response
import com.neowit.response.{BaseResult, RESULT, ResponseWriter}
import com.neowit.utils.Logging

import io.circe.syntax._
/**
  * Created by Andrey Gavrikov 
  */
trait LspProtocol[A <: BaseResult] {
    def send(result: A): Unit
}
class ResponseWriterLsp(messageId: Int, server: LanguageServer) extends ResponseWriter with Logging with MessageJsonSupport {
    private var _responseMessage: Option[ResponseMessage] = None

    override def sendResponse(result: ActionResult): Unit = {
        // TODO - implement proper handling of messages and resultOpt

        result match {
            case ActionSuccess(messages, resultOpt) =>
                val totalMessage = messages.mkString("; ")
                _responseMessage = Option(ResponseMessage(messageId, result = Option(totalMessage.asJson), error = None))
            case ActionFailure(messages, resultOpt) =>
                val totalMessage = messages.mkString("; ")
                val error = ResponseError(0, totalMessage, messageId = Option(messageId))
                _responseMessage = Option(ResponseMessage(messageId, result = None, Option(error)))
        }
    }

    override def send(msg: response.Message): response.Message = throw new UnsupportedOperationException

    override def send(msg: String): Unit = throw new UnsupportedOperationException

    override def send(msg: RESULT): Unit = throw new UnsupportedOperationException

    override def sendError(msg: String, ex: Exception): Unit = {
        val sw = new StringWriter
        ex.printStackTrace(new PrintWriter(sw))
        val stackTraceStr = sw.toString
        // dump exception information to log
        logger.error(ex.getMessage)
        logger.error(stackTraceStr)

        val err = ResponseError(0, msg + ex.getMessage + "\n" + stackTraceStr)
        _responseMessage = Option(ResponseMessage(messageId, result = None, error = Option(err)))
    }

    override def close(): Unit = ()

    def debug(msg: String): Unit = {
        server.sendLogMessageNotification(MessageType.Log, msg)
    }

    def result(): Option[ResponseMessage] = _responseMessage
}
