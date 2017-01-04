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

package com.neowit.apex.actions

import java.io.File

import com.neowit.apex.{CodeCoverageWarning, CodeLocation, ProcessedTestFailure}
import com.neowit.response.ErrorMessage

case class Location(
                   filePath: String,
                   line: Int,
                   column: Int
                   )

sealed trait DeploymentProblemType {
    def getDisplayName: String
    override def toString: String = getDisplayName
}
case object GenericDeploymentError extends DeploymentProblemType {
    override def getDisplayName: String = "ERROR"
}
case object GenericDeploymentWarning extends DeploymentProblemType {
    override def getDisplayName: String = "WARN"
}
case object DeploymentCompileError extends DeploymentProblemType {
    override def getDisplayName: String = "ERROR"
}

sealed trait DeploymentError {
    val problemType: DeploymentProblemType
    val problem: String
    val statusCode: Option[String]
    val fields: List[String]

}
case class GenericError (
                            problemType: DeploymentProblemType,
                            problem: String,
                            statusCode: Option[String] = None,
                            fields: List[String] = Nil

                        ) extends DeploymentError


case class ErrorWithLocation(
                                problemType: DeploymentProblemType,
                                problem: String,
                                location: Location,
                                statusCode: Option[String] = None,
                                fields: List[String] = Nil
                            ) extends DeploymentError

case class DeploymentReport(
                               isSuccess: Boolean,
                               isCheckOnly: Boolean,
                               failureReportOpt: Option[DeploymentFailureReport] = None,
                               fileCount: Int = 0, // number of deployed files
                               coverageReportOpt: Option[CodeCoverageReport] = None,
                               logFileOpt: Option[File] = None,
                               deployedFiles: List[File] = Nil,
                               deletedComponents: List[String] = Nil,
                               testsPassedOpt: Option[Boolean] = None,
                               otherErrors: List[ErrorMessage] = Nil, // use it only for errors not covered by DeploymentFailureReport
                               conflictsReportOpt: Option[DeploymentConflictsReport] = None
                            )

case class DeploymentFailureReport (
                                       failures: List[DeploymentError], //syntax and metadata errors
                                       testFailures: List[ProcessedTestFailure]
                                   )

case class SingleFileTestCoverage(
                                 name: String,
                                 filePathInProject: String,
                                 linesTotalNum: Int,
                                 linesNotCoveredNum: Int,
                                 linesNotCovered: Array[CodeLocation]
                                 )
case class CodeCoverageReport (
                              coveragePerFile: List[SingleFileTestCoverage],
                              coverageWarnings: Array[CodeCoverageWarning]
                              )

case class DeploymentConflictsReport (
                                     hasConflicts: Boolean,
                                     conflicts: List[SingleFileConflictDetail] = Nil
                                     )

case class SingleFileConflictDetail (
                                    file: File,
                                    lastModifiedByName: Option[String],
                                    lastModifiedById: Option[String],
                                    remoteLastModifiedDate: Option[String],
                                    localLastModifiedDate: Option[String]
                                    )