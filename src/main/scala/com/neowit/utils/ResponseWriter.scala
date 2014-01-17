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

import java.io.{File, PrintWriter}
import scala.util.parsing.json.{JSONArray, JSONObject}
import scala.util.parsing.json.JSONFormat.ValueFormatter

class ResponseWriter(file: File) extends PrintWriter(file: File) with Logging{
    var needClosing = false

    val defaultFormatter : ValueFormatter = (x : Any) => x match {
        case s : String => "\"" + escapeString(s) + "\""
        case jo : JSONObject => jo.toString(defaultFormatter)
        case ja : JSONArray => ja.toString(defaultFormatter)
        case other => other.toString
    }

    override def println(p1: String): Unit = {
        super.println(p1)
        needClosing = true
        logger.debug(p1)
    }
    def println(data: Map[String, Any]): Unit = {
        println(JSONObject(data).toString(defaultFormatter))
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
