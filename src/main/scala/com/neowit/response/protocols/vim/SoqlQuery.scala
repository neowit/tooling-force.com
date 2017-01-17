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

import com.neowit.response.{InfoMessage, SoqlQueryResult}

class SoqlQuery(writer: ResponseWriterVim) extends VimProtocol[SoqlQueryResult] {
    def send(result: SoqlQueryResult): Unit = {
        val report = result.queryReport
        report.resultSizeOpt.foreach{size =>
            writer.send("RESULT_SIZE=" + size)
            writer.send(InfoMessage("Record Count: " + size))
        }
        report.resultFileOpt.foreach{file =>
            // make sure all path folders exist
            file.getParentFile.mkdirs()
            writer.send("RESULT_FILE=" + file.getAbsolutePath)
        }
        report.errors.foreach(writer.send(_))
    }
}
