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
        new ActionFailure(Nil)
    }
    def apply(msg: Message): ActionFailure = {
        new ActionFailure(List(msg))
    }
    def apply(msg: String): ActionFailure = {
        new ActionFailure(List(ErrorMessage(msg)))
    }

}

case class ActionSuccess(messages: List[Message], result: Option[BaseResult]) extends ActionResult
case class ActionFailure(messages: List[Message]) extends ActionResult

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
        detailsByMessage.+=(detail.parentMessage.id -> allDetails)
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
            case ActionSuccess(messages, resultOpt) =>
                messages.foreach(msg => addMessage(msg))
            case ActionFailure(messages) =>
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
                            case m @ InfoMessage(_, _, _) => m.copy(details = details)
                            case m @ WarnMessage(_, _, _) => m.copy(details = details)
                            case m @ ErrorMessage(_, _, _) => m.copy(details = details)
                            case m @ DebugMessage(_, _, _) => m.copy(details = details)
                            case m @ KeyValueMessage(_) => m
                            case m @ ArbitraryTypeMessage(_, _, _, _) => m.copy(details = details)
                        }
                    case _ => // no extra details have been added
                        msg
                }
            }

        _actionResultType match {
            case Some(SUCCESS) => ActionSuccess(messagesWithDetails, _actionResult)
            case Some(FAILURE) => ActionFailure(messagesWithDetails)
            case _ =>
                throw new IllegalStateException("Action Result has not been set")
        }
    }
}