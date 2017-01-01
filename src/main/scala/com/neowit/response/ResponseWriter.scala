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

import com.neowit.apex.actions.{ActionFailure, ActionResult, ActionSuccess}
import com.neowit.utils.{JsonSupport, Logging}
import spray.json._

object ResponseWriter {

    def ensureNoNulls(data: Map[String, Any]): Map[String, Any] = data.mapValues(value => if (null == value) "" else value)

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
            case c if (c >= '\u0000' && c <= '\u001f') || (c >= '\u007f' && c <= '\u009f') => "\\u%04x".format(c: Int)
            case c => c
        }.mkString
}


class ResponseWriter(out: OutputStream, autoFlush: Boolean = true, append: Boolean = false) extends Logging  with JsonSupport {

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
        println(/*s"MESSAGE: " + */msg.toJson.compactPrint)
    }
    def println(messageDetail: MessageDetailMap): Unit = {
        println(/*s"MESSAGE DETAIL: " + */messageDetail.toJson.compactPrint)
    }
    def println(messageDetail: MessageDetailText): Unit = {
        println(/*s"MESSAGE DETAIL: " + */messageDetail.text)
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

    def println(result: BaseResult): Unit = {
        result match {
            case FindSymbolResult(Some(member)) => println(member.serialise.compactPrint)
            case FindSymbolResult(None) => // do nothing
            case ListCompletionsResult(members) => println(members.map(_.toJson).mkString("\n"))
        }
    }

    def sendResponse(result: ActionResult): Unit = {
        result match {
            case ActionSuccess(messages, None) =>
                println(SUCCESS)
                messages.foreach(println(_))
            case ActionSuccess(messages, Some(_result)) =>
                println(SUCCESS)
                println(_result)
            case ActionFailure(messages) =>
                println(FAILURE)
                messages.foreach(println(_))
        }
    }

    def close(): Unit = {
        if (needClosing)
            _writer.close()
    }


}
