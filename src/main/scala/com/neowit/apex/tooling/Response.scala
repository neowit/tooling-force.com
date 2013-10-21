/*
 * Copyright (c) 2013 Andrey Gavrikov.
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

package com.neowit.apex.tooling

import java.io.PrintWriter

/**
 * User: andrey
 * Date: 21/10/2013
 */
trait Response {
    private[this] val writer = Config.getConfig.responseWriter
    //def formatter: ResponseFormatter

    private def write(text: String) = writer.println("MESSAGE: " + text)

    private def escape(msg: String) = {
        msg.replaceAll("'", "\\\\'").replaceAll("\"", "\\\\\"")
    }
    protected def compilerOutput(msgType: String, text: String, fPath:String, fName:String, msg:String = "", line: Int = -1, col: Int = -1) {
        if ("" != msg) {
            write(s"""{"type":"$msgType", "msg":"${escape(msg)}", "lnum":$line, "col":$col, "fPath":"${escape(fPath)}", "fName":"${escape(fName)}", "text":"${escape(text)}"}""")
        } else {
            write(s"{'type':'$msgType', 'text':'$text'}")
        }
    }
    def compilerError(text: String, fPath:String, fName:String, msg:String = "", line: Int = -1, col: Int = -1) {
        compilerOutput("CompilerError", text, fPath, fName, msg, line, col)
    }
    def genericError(msgType: String, fPath:String, fName:String, msg:String = "") {
        write(s"""{"type":"$msgType", "msg":"${escape(msg)}", "fPath":"${escape(fPath)}", "fName":"${escape(fName)}", "text":"${escape(msg)}"}""")
    }

    protected def genericOutput(msgType: String, text: String, msg:String = "") {
        write(s"{'type':'Generic$msgType', 'text':'$text', 'msg':'$msg'}")
    }
    def response = this

}
/*
sealed trait ResponseFormatter {
    def format(msg: String) = msg
}
object GenericResponseFormatter {
    case class GeneralError(msg: String) extends ResponseFormatter
    case class CompileError(msg: String) extends ResponseType
    case class Result(msg: String) extends ResponseType
}




trait ResponseFormatter extends {
    def format(msgType: ResponseType, msg: String) = msg
}

*/