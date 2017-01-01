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

package com.neowit.response

import java.io.{File, FileOutputStream, OutputStream, PrintWriter}

import com.neowit.utils.JsonUtils._
import com.neowit.utils.Logging
import spray.json.DefaultJsonProtocol._
import spray.json._

object ResponseWriter {

    def ensureNoNulls(data: Map[String, Any]): Map[String, Any] = data.mapValues(value => if (null == value) "" else value)

    type MessageId = Int
    object Message {
        private var COUNTER = 0
        def getNextId(): MessageId = {
            COUNTER += 1
            COUNTER
        }
    }

    sealed trait Message {
        val id: MessageId = Message.getNextId()
        val msgType: MessageType
        val text: String
        val data: Map[String, Any]
        val details: List[MessageDetail]

        def toJSONObject: JsObject = {
            val msgData = Map("id"-> id, "text" -> text, "type" -> msgType) ++ ensureNoNulls(data)
            msgData.toJson.asJsObject
        }
    }
    case class InfoMessage(text: String, data: Map[String, Any] = Map(), details: List[MessageDetail] = Nil) extends Message {
        override val msgType: MessageType = INFO
    }
    case class WarnMessage(text: String, data: Map[String, Any] = Map(), details: List[MessageDetail] = Nil) extends Message {
        override val msgType: MessageType = WARN
    }
    case class ErrorMessage(text: String, data: Map[String, Any] = Map(), details: List[MessageDetail] = Nil) extends Message {
        override val msgType: MessageType = ERROR
    }
    case class DebugMessage(text: String, data: Map[String, Any] = Map(), details: List[MessageDetail] = Nil) extends Message {
        override val msgType: MessageType = DEBUG
    }
    // text is section name, e.g. "ERROR"
    /*
    case class SectionMessage(text: String, data: Map[String, Any] = Map(), details: List[MessageDetail] = Nil) extends Message {
        override val msgType: MessageType = SECTION
    }
    */
    //case class SectionDetailText(message: SectionMessage, text: String)

    case class KeyValueMessage(data: Map[String, Any] = Map()) extends Message {
        override val text: String = ""
        override val details: List[MessageDetail] = Nil
        override val msgType: MessageType = KEY_VALUE
    }

    sealed trait MessageDetail {
        val parentMessage: Message
    }
    case class MessageDetailMap(parentMessage: Message, data: Map[String, Any]) extends MessageDetail {
        def toJSONObject: JsObject = {
            val msgData = Map("messageId"-> parentMessage.id) ++ ensureNoNulls(data)
            msgData.toJson.asJsObject
        }
    }
    case class MessageDetailText(parentMessage: Message, text: String) extends MessageDetail

    case class ArbitraryTypeMessage(msgType: MessageType, text: String, data: Map[String, Any] = Map(), details: List[MessageDetail] = Nil) extends Message

    sealed trait MessageType {
        def getTypeString: String
        override def toString: String = getTypeString
    }
    case object INFO extends MessageType {
        def getTypeString: String = "INFO"
    }
    case object WARN extends MessageType {
        def getTypeString: String = "WARN"
    }
    case object ERROR extends MessageType {
        def getTypeString: String = "ERROR"
    }
    case object DEBUG extends MessageType {
        def getTypeString: String = "DEBUG"
    }
    case object SECTION extends MessageType {
        def getTypeString: String = "SECTION"
    }
    case object KEY_VALUE extends MessageType {
        def getTypeString: String = "KEY_VALUE"
    }
    case class CustomMessageType(msgType: String) extends MessageType {
        def getTypeString: String = msgType
    }

    /**
     * slightly modified version of JSONFormat.quoteString
     * @param s - string to check and escape if required
     * @return
     */
    def escapeString (s : String) : String =
        s.map {
            case '"'  => "\\\""
            case '\\' => "\\\\"
            //case '/'  => "\\/" //AG - do not think we need this
            case '\b' => "\\b"
            case '\f' => "\\f"
            case '\n' => "\\n"
            case '\r' => "\\r"
            case '\t' => "\\t"
            /* We'll unicode escape any control characters. These include:
             * 0x0 -> 0x1f  : ASCII Control (C0 Control Codes)
             * 0x7f         : ASCII DELETE
             * 0x80 -> 0x9f : C1 Control Codes
             *
             * Per RFC4627, section 2.5, we're not technically required to
             * encode the C1 codes, but we do to be safe.
             */
            case c if ((c >= '\u0000' && c <= '\u001f') || (c >= '\u007f' && c <= '\u009f')) => "\\u%04x".format(c: Int)
            case c => c
        }.mkString
}


class ResponseWriter(out: OutputStream, autoFlush: Boolean = true, append: Boolean = false) extends Logging{
    import ResponseWriter._

    private val _writer = new PrintWriter(out, autoFlush)

    var needClosing = false

    def this(file: File) {
        this(new FileOutputStream(file), autoFlush = true, append = false)
    }
    def this(file: File, append: Boolean) {
        this(new FileOutputStream(file), autoFlush = true, append)
    }

    def println(result: RESULT): Unit = {
        result match {
            case SUCCESS => this.println("RESULT=SUCCESS")
            case FAILURE => this.println("RESULT=FAILURE")
        }
    }
    def println(p1: String): Unit = {
        _writer.println(p1)
        needClosing = true
        logger.debug(p1)
    }
    def println(msg: Message): Unit = {
        //TODO implement special treatment for KeyValueMessage and SectionMessage
        println(s"MESSAGE: " + msg.toJSONObject.compactPrint)
    }
    def println(messageDetail: MessageDetailMap): Unit = {
        println(s"MESSAGE DETAIL: " + messageDetail.toJSONObject.compactPrint)
    }
    def println(messageDetail: MessageDetailText): Unit = {
        println(s"MESSAGE DETAIL: " + messageDetail.text)
    }
    def println(messageDetails: List[MessageDetail]): Unit = {
        messageDetails.foreach{
            case msg @ MessageDetailMap(_, _) => println(msg)
            case msg @ MessageDetailText(_, _) => println(msg)
        }
    }
    def println(prefix: String, data: Map[String, Any]): Unit = {
        println(prefix + ": " + ResponseWriter.ensureNoNulls(data).toJson.compactPrint)
    }
    def println(data: Map[String, Any]): Unit = {
        println("", data)
    }
    def startSection(sectionName: String): Unit = {
        println("#SECTION START: " + sectionName)
    }
    def endSection(sectionName: String): Unit = {
        println("#SECTION END: " + sectionName)
    }

    def close(): Unit = {
        if (needClosing)
            _writer.close()
    }


}
