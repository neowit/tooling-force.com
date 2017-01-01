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

import com.neowit.response.Message.MessageId
import com.neowit.response._

import scala.collection.mutable

/**
  * Author: Andrey Gavrikov
  */
sealed trait ActionResult

object ActionSuccess {
    def apply(): ActionSuccess = {
        new ActionSuccess(Nil, None)
    }
    def apply(messages: List[Message]): ActionSuccess = {
        new ActionSuccess(messages, None)
    }
    def apply(messages: List[Message], result: BaseResult): ActionSuccess = {
        new ActionSuccess(messages, Option(result))
    }
    def apply(result: BaseResult): ActionSuccess = {
        new ActionSuccess(Nil, Option(result))
    }
    def apply(msg: Message, result: BaseResult): ActionSuccess = {
        new ActionSuccess(List(msg), Option(result))
    }
    def apply(msg: Message): ActionSuccess = {
        new ActionSuccess(List(msg), None)
    }
    def apply(msg: String): ActionSuccess = {
        new ActionSuccess(List(InfoMessage(msg)), None)
    }
}


object ActionFailure {
    def apply(): ActionFailure = {
        new ActionFailure(Nil, None)
    }
    def apply(msg: Message): ActionFailure = {
        new ActionFailure(List(msg), None)
    }
    def apply(msg: String): ActionFailure = {
        new ActionFailure(List(ErrorMessage(msg)), None)
    }
    def apply(result: BaseResult): ActionFailure = {
        new ActionFailure(Nil, Option(result))
    }
    def apply(messages: List[Message]): ActionFailure = {
        new ActionFailure(messages, None)
    }

}

case class ActionSuccess(messages: List[Message], result: Option[BaseResult]) extends ActionResult
case class ActionFailure(messages: List[Message], result: Option[BaseResult]) extends ActionResult

class ActionResultBuilder() {
    private var _actionResultType: Option[RESULT] = None
    private var _actionResult: Option[BaseResult] = None

    def this(actionResultType: RESULT) = {
        this()
        setResultType(actionResultType)
    }

    def setResultType(actionResult: RESULT): Unit = {
        _actionResultType match {
            case Some(FAILURE) =>
                // do not allow to overwrite FAILURE
            case _ =>
                _actionResultType = Option(actionResult)
        }
    }
    def setResult(actionResult: BaseResult): Unit = {
        _actionResult match {
            case Some(_) =>
            // do not allow to overwrite FAILURE
            case _ =>
                _actionResult = Option(actionResult)
        }
    }
    private val messageListBuilder = List.newBuilder[Message]
    private val detailsByMessage = new mutable.HashMap[MessageId, List[MessageDetail]]()

    def addMessage(msg: Message): Message = {
        messageListBuilder.+=(msg)
        msg
    }
    def addMessages(messages: List[Message]): Unit = {
        messageListBuilder.++=(messages)
    }

    def addDetail(detail: MessageDetail): Unit = {
        val messageId = detail.parentMessage.id
        val existingDetails = detailsByMessage.getOrElse(messageId, Nil)
        val allDetails = existingDetails ++ List(detail)
        detailsByMessage.+=(messageId -> allDetails)
    }
    def addDetails(details: Iterable[MessageDetail]): Unit = {
        details.foreach(addDetail(_))
    }

    /**
      * add existing result and its messages to current builder.
      * FAILURE in existingResult makes this builder also FAILURE
      *
      * @param existingResult result to add
      */
    def add(existingResult: ActionResult): Unit = {
        existingResult match {
            case ActionSuccess(messages, _) =>
                messages.foreach(msg => addMessage(msg))
            case ActionFailure(messages, _) =>
                setResultType(FAILURE)
                messages.foreach(msg => addMessage(msg))
        }
    }

    def result(): ActionResult = {
        val messages = messageListBuilder.result()
        // init message details
        val messagesWithDetails =
            messages.map{msg =>
                val messageId = msg.id
                detailsByMessage.get(messageId) match {
                    case Some(details) if details.nonEmpty => //extra details have been added
                        msg match {
                            case m @ InfoMessage(_, _, _, id) => m.copy(details = details, id = id)
                            case m @ WarnMessage(_, _, _, id) => m.copy(details = details, id = id)
                            case m @ ErrorMessage(_, _, _, id) => m.copy(details = details, id = id)
                            case m @ DebugMessage(_, _, _, id) => m.copy(details = details, id = id)
                            case m @ KeyValueMessage(_) => m
                            case m @ ArbitraryTypeMessage(_, _, _, _, id) => m.copy(details = details, id = id)
                        }
                    case _ => // no extra details have been added
                        msg
                }
            }

        _actionResultType match {
            case Some(SUCCESS) => ActionSuccess(messagesWithDetails, _actionResult)
            case Some(FAILURE) => ActionFailure(messagesWithDetails, _actionResult)
            case _ =>
                throw new IllegalStateException("Action Result has not been set")
        }
    }
}