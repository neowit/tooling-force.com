/*
 *
 *  * Copyright (c) 2017 Andrey Gavrikov.
 *  * this file is part of tooling-force.com application
 *  * https://github.com/neowit/tooling-force.com
 *  *
 *  * This program is free software: you can redistribute it and/or modify
 *  * it under the terms of the GNU Lesser General Public License as published by
 *  * the Free Software Foundation, either version 3 of the License, or
 *  * (at your option) any later version.
 *  *
 *  * This program is distributed in the hope that it will be useful,
 *  * but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  * GNU Lesser General Public License for more details.
 *  *
 *  * You should have received a copy of the GNU Lesser General Public License
 *  * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 */

package com.neowit.response.protocols.vim

import com.neowit.response.ExecuteAnonymousResult

class ExecuteAnonymous(writer: ResponseWriterVim) extends VimProtocol[ExecuteAnonymousResult] {
    def send(result: ExecuteAnonymousResult): Unit = {
        val (printDeploymentFailures, printStackTrace) =
        result.stackTraceOpt  match {
            case Some(stackTrace) if 1 == result.errors.length =>
                // when there is Stack Trace & single error they are both usually related
                writer.send("ERROR", Map("text" -> (result.errors.head.problem + ". " + stackTrace) ))
                (false, false)
            case _ => (true, true)
        }
        if (printDeploymentFailures) {
            DeploymentReportUtils.printDeploymentFailures(writer, result.errors)
        }
        if (printStackTrace) {
            result.stackTraceOpt.foreach{s =>
                writer.send("STACK_TRACE", Map("text" -> s))
                writer.send("ERROR", Map("text" -> s))
            }
        }
        result.logFileOpt.foreach(f => writer.send("LOG_FILE=" + f.getAbsolutePath))

    }
}
