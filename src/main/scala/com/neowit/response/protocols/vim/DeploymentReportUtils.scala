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

import java.io.File

import com.neowit.apex.ProcessedTestFailure
import com.neowit.apex.actions._
import com.neowit.response.{ErrorMessage, InfoMessage, MessageDetailMap, WarnMessage}

object DeploymentReportUtils {

    def sendDeploymentFailureReport(writer: ResponseWriterVim, report: DeploymentFailureReport): Unit = {

        if (report.failures.nonEmpty) {
            printDeploymentFailures(writer, report.failures)
        }
        if (report.testFailures.nonEmpty) {
            //print test failures
        }

        Unit
    }

    def printDeploymentFailures(writer: ResponseWriterVim, failures: List[DeploymentError]): Unit = {
        if (failures.nonEmpty) {
            val hasGenericErrors = failures.exists{
                case GenericError(_, _, _, _) => true
                case _ => false
            }

            val componentFailureMessage = WarnMessage("Component failures")
            if (hasGenericErrors) {
                writer.send(componentFailureMessage)
            }
            val section = writer.startSection("ERROR LIST")
            for (failure <- failures) {
                failure match {
                    case GenericError(problemType, problem, statusCodeOpt, fields) =>
                        // for :ApexMessages window
                        writer.println(MessageDetailMap(componentFailureMessage, Map("type" -> problemType, "text" -> problem)))
                    case ErrorWithLocation(problemType, problem, location, statusCodeOpt, fields) =>
                        // for vim quickfix
                        writer.println(
                                "ERROR",
                                Map("type" -> problemType,
                                    "filePath" -> location.filePath,
                                    "line" -> location.line,
                                    "column" -> location.column,
                                    "text" -> problem
                                )
                        )
                }
            }
            if (hasGenericErrors) {
                writer.endSection(section)
            }
        }
        Unit
    }

    def printTestFailures(writer: ResponseWriterVim, failures: List[ProcessedTestFailure]): Unit = {
        //TODO
        Unit
    }

    def sendOtherErrors(writer: ResponseWriterVim, errors: List[ErrorMessage]): Unit = {
        errors.foreach(writer.send(_))
        Unit
    }

    def sendLogFile(writer: ResponseWriterVim, logFileOpt: Option[File]): Unit = {
        logFileOpt.foreach(logFile => writer.send("LOG_FILE=" + logFile.getAbsolutePath) )
        Unit
    }

    def printCoverageReport(writerVim: ResponseWriterVim, coverageReportOpt: Option[CodeCoverageReport]): Unit = {
        if (coverageReportOpt.nonEmpty) {

        }
        //TODO
        Unit
    }

    def sendSuccessReport(writer: ResponseWriterVim, report: DeploymentReport ): Unit = {
        if (report.isSuccess) {
            if (report.fileCount > 0) {
                if (!report.isCheckOnly) {
                    writer.send("FILE_COUNT=" + report.fileCount)
                    writer.startSection("DEPLOYED FILES")
                    report.deployedFiles.foreach(f => writer.send(f.getName))
                    writer.endSection("DEPLOYED FILES")
                }
            } else {
                writer.send(InfoMessage("no modified files detected."))
            }
            if (report.testsPassedOpt.contains(true)) {
                writer.send(InfoMessage("Tests PASSED"))
            }
            printCoverageReport(writer, report.coverageReportOpt)
        }

        Unit
    }
}
