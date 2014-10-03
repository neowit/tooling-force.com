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

package com.neowit.utils

import java.io.{FileOutputStream, OutputStream, File, PrintWriter}
import scala.util.parsing.json.{JSONArray, JSONObject}
import scala.util.parsing.json.JSONFormat.ValueFormatter
import com.neowit.utils.ResponseWriter.{MessageType, MessageDetail, Message}

object ResponseWriter {
    object Message {
        private var COUNTER = 0
        def getNextId() = {
            COUNTER += 1
            COUNTER
        }
    }
    case class Message(msgType: MessageType, text: String, data: Map[String, Any] = Map()) {
        val id = Message.getNextId()
        def toJSONObject = {
            val msgData = Map("id"-> id, "text" -> text, "type" -> msgType) ++ data
            JSONObject(msgData)
        }
    }
    case class MessageDetail(message: Message, data: Map[String, Any]) {
        def toJSONObject = {
            val msgData = Map("messageId"-> message.id) ++ data
            JSONObject(msgData)
        }
    }

    trait MessageType {
        def getTypeString: String
        override def toString: String = "\"" + getTypeString+ "\""
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
    case class CustomMessageType(msgType: String) extends MessageType{
        def getTypeString: String = msgType
    }

    val defaultFormatter : ValueFormatter = (x : Any) => x match {
        case s : String => "\"" + escapeString(s) + "\""
        case jo : JSONObject => jo.toString(defaultFormatter)
        case ja : JSONArray =>
            val res = ja.toString(defaultFormatter)
            //sometimes scala adds comma at the end of the array, make sure it does not happen
            res.replace(", ]", "]")
        case other if null != other => other.toString
        case other => "" //all unhandled cases, like null, etc
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
class ResponseWriter(out: OutputStream, autoFlush: Boolean = true) extends PrintWriter(out, autoFlush) with Logging{
    var needClosing = false

    def this(file: File) {
        this(new FileOutputStream(file), true)
    }
    override def println(p1: String): Unit = {
        super.println(p1)
        needClosing = true
        logger.debug(p1)
    }
    def println(msg: Message): Unit = {
        println(s"MESSAGE: " + msg.toJSONObject.toString(ResponseWriter.defaultFormatter))
    }
    def println(messageDetail: MessageDetail): Unit = {
        println(s"MESSAGE DETAIL: " + messageDetail.toJSONObject.toString(ResponseWriter.defaultFormatter))
    }
    def println(prefix: String, data: Map[String, Any]): Unit = {
        println(prefix + ": " + JSONObject(data).toString(ResponseWriter.defaultFormatter))
    }
    def println(data: Map[String, Any]): Unit = {
        println("", data)
    }
    def startSection(sectionName: String) {
        println("#SECTION START: " + sectionName)
    }
    def endSection(sectionName: String) {
        println("#SECTION END: " + sectionName)
    }

    override def close(): Unit = {
        if (needClosing)
            super.close()
    }


}
