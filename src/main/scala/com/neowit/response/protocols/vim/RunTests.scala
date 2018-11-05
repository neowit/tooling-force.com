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

import java.io.{File, PrintWriter}

import com.neowit.apex.ProcessedTestFailure
import com.neowit.apex.actions.CodeCoverageReport
import com.neowit.response._
import com.neowit.utils.{FileUtils, JsonSupport}
import spray.json._

object RunTests extends JsonSupport {
    def printTestFailures(writer: ResponseWriterVim, failures: List[ProcessedTestFailure]): Unit = {
        if (failures.nonEmpty) {
            val section = writer.startSection("ERROR LIST")
            for (failure <- failures) {
                writer.send("ERROR", Map("line" -> failure.line.getOrElse(-1), "column" -> failure.column.getOrElse(-1), "filePath" -> failure.filePath.getOrElse(""), "text" -> failure.message))
            }
            writer.endSection(section)
        } else {
            writer.send(InfoMessage("Tests PASSED"))
        }
        Unit
    }
    def printCoverageReport(writer: ResponseWriterVim, coverageReportOpt: Option[CodeCoverageReport]): Unit = {
        coverageReportOpt match {
            case Some(coverageReport) =>
                val coverageDetails = writer.send(WarnMessage("Code coverage details"))
                val coveragePerFileSorted = coverageReport.coveragePerFile.sortBy(_.filePathInProject)

                for (fileCoverage <- coveragePerFileSorted) {
                    val linesCovered = fileCoverage.linesTotalNum - fileCoverage.linesNotCoveredNum
                    val coveragePercent = if (fileCoverage.linesTotalNum > 0) linesCovered * 100 / fileCoverage.linesTotalNum else 0
                    //val messageType = (if (coveragePercent >= 75) ResponseWriter.INFO else ResponseWriter.WARN)
                    val fileName = fileCoverage.name

                    writer.println(
                        MessageDetailMap(
                            coverageDetails,
                            Map(
                                "text" ->
                                    (fileName +
                                        ": lines total " + fileCoverage.linesTotalNum +
                                        "; lines not covered " + fileCoverage.linesNotCoveredNum +
                                        "; covered " + coveragePercent + "%"),
                                "type" -> (if (coveragePercent >= 75) INFO else WARN)
                            )
                        )
                    )
                }
                for (warning <- coverageReport.coverageWarnings) {
                    val text =
                        if (null != warning.getName && warning.getName.nonEmpty) {
                            warning.getName + ": " + warning.getMessage
                        } else {
                            warning.getMessage
                        }
                    writer.println(MessageDetailMap(coverageDetails, Map("text" -> text)))
                }
            case None =>
        }
        Unit
    }

    def prepareDetailedPerFileCoverage(coverageReportOpt: Option[CodeCoverageReport]): Option[File] = {
        coverageReportOpt match {
            case Some(coverageReport) =>
                val coverageFile = FileUtils.createTempFile("coverage", ".txt")
                val coverageWriter = new PrintWriter(coverageFile)
                for (coverage <- coverageReport.coveragePerFile) {
                    val locations = List.newBuilder[Int]
                    // handling for Winter 19 SFDC bug:
                    // https://salesforce.stackexchange.com/questions/237432/bug-blank-locationsnotcovered-returned-from-rest-tooling-runtestssynchronous
                    val validUncoveredLocations = coverage.linesNotCovered.filter(_.getLine > 0)

                    for (codeLocation <- validUncoveredLocations) {
                        locations += codeLocation.getLine
                    }
                    val coverageJSON: JsValue = Map(
                        "path" -> coverage.filePathInProject,
                        "linesTotalNum" -> coverage.linesTotalNum,
                        "linesNotCoveredNum" -> coverage.linesNotCoveredNum,
                        "linesNotCovered" -> locations.result().toJson
                    ).toJson
                    // end result looks like so:
                    // {"path" : "src/classes/AccountController.cls", "linesNotCovered" : [1, 2, 3,  4, 15, 16,...]}
                    coverageWriter.println(coverageJSON.compactPrint)
                }
                coverageWriter.close()
                Option(coverageFile)
            case None =>
                None
        }
    }

    def printLogFiles(writer: ResponseWriterVim, result: RunTestsResult): Unit = {
        result.log match {
            case Some(logFile) =>
                writer.send("LOG_FILE=" + logFile.getAbsolutePath)
            case None =>
        }
        if (result.logFilePathByClassName.nonEmpty) {
            writer.send("LOG_FILE_BY_CLASS_NAME=" + result.logFilePathByClassName.toJson.compactPrint)
        }
    }

}
class RunTests(writer: ResponseWriterVim) extends VimProtocol[RunTestsResult] {
    import RunTests._

    def send(result: RunTestsResult): Unit = {
        printTestFailures(writer, result.testFailures)
        printCoverageReport(writer, result.coverageReportOpt)
        printLogFiles(writer, result)

        prepareDetailedPerFileCoverage(result.coverageReportOpt) match {
            case Some(resultFile) =>
                writer.send("COVERAGE_FILE=" + resultFile.getAbsolutePath)
            case None =>
        }
        Unit
    }
}
