/*
 * Copyright (c) 2018 Andrey Gavrikov.
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

package com.neowit.apex.actions.tooling

import com.neowit.apex._
import com.neowit.apex.actions._
import com.neowit.utils.FileUtils

import scala.concurrent.{ExecutionContext, Future}

/**
  * Created by Andrey Gavrikov 
  */
class LoadApexCodeCoverageAggregate extends ApexActionWithReadOnlySession with RunTestJsonSupport {

    override def getHelp: ActionHelp = new ActionHelp {
        override def getExample: String = ""

        override def getParamDescription(paramName: String): String = paramName match {
            case "classOrTriggerName" => "Name of Class or Trigger for which to load coverage"
            case x => "Unsupported parameter: " + x
        }

        override def getParamNames: List[String] = List()

        override def getSummary: String = "Get code coverage"

        override def getName: String = "loadApexCodeCoverageAggregate"
    }

    override protected def act()(implicit ec: ExecutionContext): Future[ActionResult] = {

        val actionResult = config.getProperty("classOrTriggerName") match {
            case Some(classOrTriggerNameProvided) =>
                var classOrTriggerName = FileUtils.removeExtension(classOrTriggerNameProvided)
                val queryIterator = SoqlQuery.getQueryIteratorTooling(session,
                    s""" select ApexClassOrTrigger.Name, ApexClassOrTriggerId, NumLinesCovered, NumLinesUncovered, Coverage
                       | from ApexCodeCoverageAggregate
                       | where ApexClassOrTrigger.Name = '$classOrTriggerName'
                       |""".stripMargin)

                var errorBuilder = Array.newBuilder[String]

                val coverageResultOpt =
                    if (queryIterator.hasNext) {
                        val record = queryIterator.map(obj => obj.convertTo[RunTestJsonSupport.ApexCodeCoverageAggregate]).next()
                        println(record)
                        val coverageResult = RunTestConversions.toCodeCoverageResult(record)
                        Option(coverageResult)
                    } else {
                        errorBuilder += "No coverage available for " + classOrTriggerName
                        None
                    }

                val runTestResult = new com.neowit.apex.RunTestsResult {
                    override def getCodeCoverage: Array[CodeCoverageResult] = {
                        coverageResultOpt match {
                            case Some(coverageResult) =>
                                List(new RunTests.CodeCoverageResultTooling(coverageResult)).toArray
                            case None => Array()
                        }
                    }

                    override def getCodeCoverageWarnings: Array[CodeCoverageWarning] = Array()

                    override def getFailures: Array[RunTestFailure] = Array()
                }

                val errors = errorBuilder.result()
                if (errors.nonEmpty) {
                    ActionFailure(errors.mkString(";"))
                } else {
                    ApexTestUtils.processCodeCoverage(runTestResult, session) match {
                        case Some(codeCoverageReport) =>
                            ActionSuccess(com.neowit.response.LoadApexCodeCoverageAggregateResult(codeCoverageReport))
                        case None =>
                            ActionFailure("No coverage available for " + classOrTriggerName)
                    }
                }
            case None =>
                ActionFailure("Missing required parameter: classOrTriggerName")
        }
        Future.successful(actionResult)
    }
}
