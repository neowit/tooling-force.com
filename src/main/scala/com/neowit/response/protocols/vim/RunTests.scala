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

import com.neowit.apex.ProcessedTestFailure
import com.neowit.apex.actions.CodeCoverageReport
import com.neowit.response.RunTestsResult

object RunTests {
    def printTestFailures(writer: ResponseWriterVim, failures: List[ProcessedTestFailure]): Unit = {
        if (failures.nonEmpty) {
            val section = writer.startSection("ERROR LIST")
            for (failure <- failures) {
                writer.println("ERROR", Map("line" -> failure.line, "column" -> failure.column, "filePath" -> failure.filePath, "text" -> failure.message))
            }
            writer.endSection(section)
        }
        Unit
    }
    def printCoverageReport(writerVim: ResponseWriterVim, coverageReportOpt: Option[CodeCoverageReport]): Unit = {
        if (coverageReportOpt.nonEmpty) {

        }
        //TODO
        Unit
    }

}
class RunTests(writer: ResponseWriterVim) extends VimProtocol[RunTestsResult] {
    import RunTests._
    def send(result: RunTestsResult): Unit = {

        printTestFailures(writer, result.testFailures)
        printCoverageReport(writer, result.coverageReportOpt)

        writer.send("NOT IMPLEMENTED")
    }
}
