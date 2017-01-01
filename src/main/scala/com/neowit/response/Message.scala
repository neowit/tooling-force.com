package com.neowit.response

import com.neowit.response.Message.MessageId
import com.neowit.utils.JsonSupport
import spray.json._
/**
  * Author: Andrey Gavrikov
  * Date: 01/01/2017
  */
object Message {
    type MessageId = Int
    private var COUNTER = 0
    def generateNextId(): MessageId = {
        COUNTER += 1
        COUNTER
    }
}
sealed trait Message {
    val id: MessageId = Message.generateNextId()
    //val text: String
    //val data: Map[String, Any]
    //val details: List[MessageDetail]

    def toJson: JsValue
}
trait BaseSerialiser extends JsonSupport {

    def toJson(msgType: MessageType, id: MessageId, text: String, data: Map[String, Any] = Map(), details: List[MessageDetail] = Nil): JsValue = {
        val msgData = Map("id"-> id, "text" -> text, "type" -> msgType) ++ ResponseWriter.ensureNoNulls(data)
        msgData.toJson
    }
}
case class InfoMessage(text: String, data: Map[String, Any] = Map(), details: List[MessageDetail] = Nil,
                       override val id: MessageId = Message.generateNextId()) extends Message with BaseSerialiser {
    override def toJson: JsValue = toJson(INFO, id, text, data, details)
}
case class WarnMessage(text: String, data: Map[String, Any] = Map(), details: List[MessageDetail] = Nil,
                       override val id: MessageId = Message.generateNextId()) extends Message with BaseSerialiser {
    override def toJson: JsValue = toJson(WARN, id, text, data, details)
}
case class ErrorMessage(text: String, data: Map[String, Any] = Map(), details: List[MessageDetail] = Nil,
                        override val id: MessageId = Message.generateNextId()) extends Message with BaseSerialiser {
    override def toJson: JsValue = toJson(ERROR, id, text, data, details)
}
case class DebugMessage(text: String, data: Map[String, Any] = Map(), details: List[MessageDetail] = Nil,
                        override val id: MessageId = Message.generateNextId()) extends Message with BaseSerialiser {
    override def toJson: JsValue = toJson(DEBUG, id, text, data, details)
}
// text is section name, e.g. "ERROR"
/*
case class SectionMessage(text: String, data: Map[String, Any] = Map(), details: List[MessageDetail] = Nil) extends Message {
    override val msgType: MessageType = SECTION
}
*/
//case class SectionDetailText(message: SectionMessage, text: String)

case class KeyValueMessage(data: Map[String, Any] = Map()) extends Message with BaseSerialiser {
    override def toJson: JsValue = {
        data.toJson
    }
}

sealed trait MessageDetail {
    val parentMessage: Message
}
case class MessageDetailMap(parentMessage: Message, data: Map[String, Any]) extends MessageDetail with JsonSupport {
    def toJson: JsValue = {
        val msgData = Map("messageId"-> parentMessage.id) ++ ResponseWriter.ensureNoNulls(data)
        msgData.toJson
    }
}
case class MessageDetailText(parentMessage: Message, text: String) extends MessageDetail

case class ArbitraryTypeMessage(msgType: MessageType, text: String, data: Map[String, Any] = Map(), details: List[MessageDetail] = Nil,
                                override val id: MessageId = Message.generateNextId()) extends Message with BaseSerialiser {
    override def toJson: JsValue = toJson(msgType, id, text, data, details)
}

