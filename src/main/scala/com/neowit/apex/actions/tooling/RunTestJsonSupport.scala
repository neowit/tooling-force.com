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

package com.neowit.apex.actions.tooling

import com.neowit.utils.JsonSupport

trait RunTestJsonSupport extends JsonSupport {
    import RunTestJsonSupport._
    implicit val CoverageWarningFormat = jsonFormat4(CoverageWarning) //TODO
    implicit val RunTestSuccessFormat = jsonFormat6(RunTestSuccess) //TODO
    implicit val MethodInfoFormat = jsonFormat1(MethodInfo) //TODO
    implicit val DmlInfoFormat = jsonFormat1(DmlInfo) //TODO
    implicit val SoqlInfoFormat = jsonFormat1(SoqlInfo) //TODO
    implicit val SoslInfoFormat = jsonFormat1(SoslInfo) //TODO
    implicit val LocationFormat = jsonFormat4(CodeLocation) //TODO

    implicit val TestCodeCoverageFormat = jsonFormat11(TestCodeCoverage)
    implicit val TestFailureFormat = jsonFormat10(TestFailure)

    implicit val RunTestsSynchronousResultFormat = jsonFormat8(RunTestsSynchronousResult)

    implicit val ApexClassOrTriggerFormat = jsonFormat1(ApexClassOrTrigger)
    implicit val CoverageFormat = jsonFormat2(Coverage)
    implicit val ApexCodeCoverageAggregateFormat = jsonFormat5(ApexCodeCoverageAggregate)
}
object RunTestJsonSupport {

    case class RunTestsSynchronousResult(codeCoverageWarnings: List[CoverageWarning],
                                         apexLogId: Option[String],
                                         successes: List[RunTestSuccess],
                                         codeCoverage: List[TestCodeCoverage],
                                         failures: List[TestFailure],
                                         numTestsRun: Int,
                                         numFailures: Int,
                                         totalTime: Double
                                        )


    case class CoverageWarning(id: Option[String], name: Option[String], namespace: Option[String], message: Option[String])

    case class RunTestSuccess(methodName: Option[String], id: Option[String], name: Option[String], time: Option[Double], seeAllData: Option[Boolean], namespace: Option[String])

    case class TestCodeCoverage(numLocationsNotCovered: Option[Int],
                                name: Option[String],
                                methodInfo: Option[List[MethodInfo]],
                                dmlInfo: Option[List[DmlInfo]],
                                numLocations: Option[Int],
                                soqlInfo: Option[List[SoqlInfo]],
                                id: Option[String],
                                soslInfo: Option[List[SoslInfo]],
                                locationsNotCovered: Option[List[CodeLocation]],
                                `type`: Option[String],
                                namespace: Option[String]
                               )

    case class TestFailure(methodName: Option[String],
                           name: Option[String],
                           packageName: Option[String],
                           stackTrace: Option[String],
                           id: Option[String],
                           message: Option[String],
                           time: Option[Double],
                           seeAllData: Option[Boolean],
                           `type`: Option[String],
                           namespace: Option[String])

    case class MethodInfo(todo: Option[String])

    case class DmlInfo(todo: Option[String])

    case class SoqlInfo(todo: Option[String])

    case class SoslInfo(todo: Option[String])

    case class CodeLocation(line: Option[Int], column: Option[Int], numExecutions: Option[Int], time: Option[Double])

    case class ApexClassOrTrigger(Name: String)
    case class Coverage(coveredLines: Array[Int], uncoveredLines: Array[Int])

    case class ApexCodeCoverageAggregate(ApexClassOrTrigger: ApexClassOrTrigger,
                                         ApexClassOrTriggerId: String,
                                         Coverage: Coverage,
                                         NumLinesCovered: Int,
                                         NumLinesUncovered: Int)

}