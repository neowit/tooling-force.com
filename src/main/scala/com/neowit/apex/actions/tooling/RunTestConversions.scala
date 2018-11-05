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

/**
  * Author: Andrey Gavrikov
  * Date: 07/01/2017
  */
object RunTestConversions {
    import RunTestJsonSupport._

    def toRunTestResult(res: RunTestsSynchronousResult): com.sforce.soap.tooling.RunTestsResult = {
        val result = new com.sforce.soap.tooling.RunTestsResult()
        res.apexLogId.foreach(result.setApexLogId(_))
        result.setCodeCoverage(toCodeCoverageResult(res.codeCoverage))
        result.setCodeCoverageWarnings(toCodeCoverageWarnings(res.codeCoverageWarnings))
        result.setFailures(toRunTestFailures(res.failures))
        result.setNumFailures(res.numFailures)
        result.setNumTestsRun(res.numTestsRun)
        result.setSuccesses(toRunTestSuccess(res.successes))
        result.setTotalTime(res.totalTime)

        result
    }

    def toCodeCoverageResult(coverageAggregate: ApexCodeCoverageAggregate): com.sforce.soap.tooling.CodeCoverageResult = {
        val res = new com.sforce.soap.tooling.CodeCoverageResult()
        res.setId(coverageAggregate.ApexClassOrTriggerId)
        res.setName(coverageAggregate.ApexClassOrTrigger.Name)
        res.setNumLocations(coverageAggregate.NumLinesCovered  + coverageAggregate.NumLinesUncovered )
        res.setNumLocationsNotCovered(coverageAggregate.NumLinesUncovered)
        res.setLocationsNotCovered(
            coverageAggregate.Coverage.uncoveredLines.map{line =>
                val loc = new com.sforce.soap.tooling.CodeLocation()
                loc.setLine(line)
                loc
            }
        )
        res
    }

    private def toCodeCoverageResult(coverage: List[TestCodeCoverage]): Array[com.sforce.soap.tooling.CodeCoverageResult] = {
        coverage.map{c =>
            val res = new com.sforce.soap.tooling.CodeCoverageResult()
            c.`type`.foreach(res.setType(_))
            //c.dmlInfo
            c.id.foreach(res.setId(_))
            val locationsNotCovered =
                c.locationsNotCovered  match {
                    case Some(locations) =>
                        locations.map{loc =>
                            val res = new com.sforce.soap.tooling.CodeLocation()
                            loc.column.foreach(res.setColumn(_))
                            loc.line.foreach(res.setLine(_))
                            loc.numExecutions.foreach(res.setNumExecutions(_))
                            loc.time.foreach(res.setTime(_))
                            res
                        }
                    case None => Nil
                }
            res.setLocationsNotCovered(locationsNotCovered.toArray)

            //c.methodInfo
            c.name.foreach(res.setName(_))
            c.namespace.foreach(res.setNamespace(_))
            c.numLocations.foreach(res.setNumLocations(_))
            c.numLocationsNotCovered.foreach(res.setNumLocationsNotCovered(_))
            //c.soqlInfo
            //c.soslInfo

            res
        }.toArray

    }

    private def toCodeCoverageWarnings(coverage: List[CoverageWarning]): Array[com.sforce.soap.tooling.CodeCoverageWarning] = {
        coverage.map{c =>
            val res = new com.sforce.soap.tooling.CodeCoverageWarning()
            c.id.foreach(res.setId(_))
            c.message.foreach(res.setMessage(_))
            c.name.foreach(res.setName(_))
            c.namespace.foreach(res.setNamespace(_))

            res
        }.toArray
    }

    private def toRunTestFailures(failures: List[TestFailure]): Array[com.sforce.soap.tooling.RunTestFailure] = {
        failures.map{f =>
            val res = new com.sforce.soap.tooling.RunTestFailure()
            f.id.foreach(res.setId(_))
            f.message.foreach(res.setMessage(_))
            f.methodName.foreach(res.setMethodName(_))
            f.name.foreach(res.setName(_))
            f.namespace.foreach(res.setNamespace(_))
            f.seeAllData.foreach(res.setSeeAllData(_))
            f.stackTrace.foreach(res.setStackTrace(_))
            f.time.foreach(res.setTime(_))
            f.`type`.foreach(res.setType(_))

            res
        }.toArray
    }
    private def toRunTestSuccess(successes: List[RunTestSuccess]): Array[com.sforce.soap.tooling.RunTestSuccess] = {
        successes.map{s =>
            val res = new com.sforce.soap.tooling.RunTestSuccess()
            s.id.foreach(res.setId(_))
            s.methodName.foreach(res.setMethodName(_))
            s.name.foreach(res.setName(_))
            s.namespace.foreach(res.setNamespace(_))
            s.seeAllData.foreach(res.setSeeAllData(_))
            s.time.foreach(res.setTime(_))
            res
        }.toArray
    }

}
