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

import java.io.File

import com.neowit.apex.actions.ActionResult
import com.neowit.utils.{FileUtils, JsonSupport, Logging}

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

    /**
      * return relative path inside project folder
      * this path is used as key in session
      * @param file - resource under project folder
      * @return - string, looks like: unpackaged/pages/Hello.page
      */
    def getRelativePath(file: File): String = {
        //find parent src/ or unpackaged/
        FileUtils.getParentByName(file, Set("src", "unpackaged")) match {
            case Some(srcFolder) if null != srcFolder.getParentFile =>
                val projectPath = srcFolder.getParentFile.getAbsolutePath + File.separator
                val res = file.getAbsolutePath.substring(projectPath.length)
                FileUtils.normalizePath(res)
            case None => file.getAbsolutePath
        }
    }
}


trait ResponseWriter extends Logging  with JsonSupport {

    def sendResponse(result: ActionResult): Unit
    def send(msg: Message): Unit
    def send(msg: String): Unit
    def send(msg: RESULT): Unit

    def close(): Unit


}
