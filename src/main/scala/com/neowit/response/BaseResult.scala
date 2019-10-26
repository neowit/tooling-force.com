/*
 * Copyright (c) 2017 Andrey Gavrikov.
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

import com.neowit.apex.ProcessedTestFailure
import com.neowit.apex.actions._
import com.neowit.apex.parser.Member
import com.neowit.auth.Oauth2Tokens
import com.neowit.utils.JsonSupport
import spray.json._

/**
  * Author: Andrey Gavrikov
  * Date: 01/01/2017
  */
sealed abstract class BaseResult

case class FindSymbolResult(memberOpt: Option[Member]) extends BaseResult with JsonSupport {
    def toJson: JsValue = memberOpt match {
        case Some(member) => member.serialise
        case None => Map.empty.toJson
    }
}

case class ListCompletionsResult(members: List[Member]) extends BaseResult


case class RunTestsResult(
                             testFailures: List[ProcessedTestFailure],
                             logFilePathByClassName: Map[String, String] = Map.empty,
                             log: Option[File] = None,
                             coverageReportOpt: Option[CodeCoverageReport] = None,
                             deploymentFailureReport: Option[DeploymentFailureReport] = None
                         ) extends BaseResult


case class AppVersionResult(appName: String, appVersion: String, sfdcApiVersion: String, javaVersion: String, os: String) extends BaseResult
case class BulkRetrieveActionResult(resultFolder: File, fileCountByType: Map[String, Int], errors: List[Message]) extends BaseResult
case class CheckSyntaxResult(sourceFile: File, errors: List[com.neowit.apexscanner.scanner.actions.SyntaxError]) extends BaseResult
case class DeployAllDestructiveResult(deploymentReport: DeploymentReport, diffReportOpt: Option[DiffWithRemoteReport]) extends BaseResult
case class DeployAllResult(deploymentReport: DeploymentReport) extends BaseResult
case class DeployDestructiveResult(deploymentReport: DeploymentReport) extends BaseResult
case class DeployModifiedDestructiveResult(deploymentReportOpt: Option[DeploymentReport]) extends BaseResult
case class DeployModifiedResult(deploymentReport: DeploymentReport) extends BaseResult
case class DiffWithRemoteResult(report: DiffWithRemoteReport) extends BaseResult
case class ExecuteAnonymousResult(errors: List[DeploymentError], stackTraceOpt: Option[String], logFileOpt: Option[File], repeatCount: Int) extends BaseResult
case class ListConflictingResult(conflictReport: DeploymentConflictsReport) extends BaseResult
case class ListModifiedResult(modified: List[File], deleted: List[File]) extends BaseResult
case class LoginOauthResult(tokens: Option[Oauth2Tokens], resultFileOpt: Option[File]) extends BaseResult
case class RefreshMetadataResult(retrieveResult: Option[UpdateFromRetrieveResult], modifiedFiles: List[File]) extends BaseResult
case class SoqlQueryResult(queryReport: SoqlQueryReport) extends BaseResult
case class LoadApexCodeCoverageAggregateResult(coverageReport: CodeCoverageReport) extends BaseResult
case class GuessSetupUrlResult(url: String) extends BaseResult
case class RenameMetadataResult(errors: List[String]) extends BaseResult

