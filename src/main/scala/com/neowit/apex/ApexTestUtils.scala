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

package com.neowit.apex


import com.neowit.apex.actions._
import com.neowit.utils.JsonSupport


trait RunTestsResult {
    def getCodeCoverage: Array[CodeCoverageResult]
    def getCodeCoverageWarnings: Array[CodeCoverageWarning]
    def getFailures: Array[RunTestFailure]
}
trait CodeCoverageResult {
    def getNumLocations: Int
    def getNumLocationsNotCovered: Int
    def getName: String
    def getLocationsNotCovered: Array[CodeLocation]
}
trait CodeLocation {
    def getLine: Int
}

trait CodeCoverageWarning {
    def getName: String
    def getMessage: String
}
trait RunTestFailure {
    def getId: String
    def getType: String
    def getName: String
    def getMessage: String
    def getMethodName: String
    def getStackTrace: String
}

case class ProcessedTestFailure(id: Option[String],
                                `type`: String,
                                line: Option[Int],
                                column: Option[Int],
                                filePath: Option[String],
                                message: String,
                                methodName: Option[String],
                                parentId: Option[String])

object ApexTestUtils extends JsonSupport {
    //Class.Test1: line 19, column 1
    private val TypeFileLineColumnRegex = """.*(Class|Trigger)\.(\w*).*line (\d+), column (\d+).*""".r
    //Class.Test1.prepareData: line 13, column 1
    private val TypeFileMethodLineColumnRegex = """.*(Class|Trigger)\.(\w*)\.(\w*).*line (\d+), column (\d+).*""".r

    /**
     * process code coverage results
     * @param runTestResult - deployDetails.getRunTestResult
     * @return CodeCoverageReport
     */
    def processCodeCoverage(runTestResult: com.neowit.apex.RunTestsResult, session: Session ): Option[CodeCoverageReport] = {
        if (runTestResult.getCodeCoverage.nonEmpty || runTestResult.getCodeCoverageWarnings.nonEmpty) {
            val metadataByXmlName = DescribeMetadata.getMap(session)
            val (classDir, classExtension) = metadataByXmlName.get("ApexClass") match {
                case Some(describeObject) => (describeObject.getDirectoryName, describeObject.getSuffix)
                case None => ("classes", "cls")
            }
            val (triggerDir, triggerExtension) = metadataByXmlName.get("ApexTrigger") match {
                case Some(describeObject) => (describeObject.getDirectoryName, describeObject.getSuffix)
                case None => ("triggers", "trigger")
            }
            val coveragePerFile = List.newBuilder[SingleFileTestCoverage]
            for ( coverageResult <- runTestResult.getCodeCoverage) {
                val relativeFilePath = session.getRelativePath(classDir, coverageResult.getName + "." + classExtension) match {
                    case Some(relPath) => Some(relPath)
                    case None =>
                        //check if this is a trigger name
                        session.getRelativePath(triggerDir, coverageResult.getName + "." + triggerExtension) match {
                            case Some(relPath) => Some(relPath)
                            case None => None
                        }
                }
                relativeFilePath match {
                    case Some(relPath) =>
                        coveragePerFile +=
                            SingleFileTestCoverage(
                                coverageResult.getName,
                                relPath,
                                coverageResult.getNumLocations,
                                coverageResult.getNumLocationsNotCovered,
                                coverageResult.getLocationsNotCovered)
                    case None =>
                }

            }
            Option(CodeCoverageReport(coveragePerFile.result(), runTestResult.getCodeCoverageWarnings))

        } else {
            None
        }
    }
    /**
     * --testsToRun="comma separated list of class.method names",
     *      e.g. "ControllerTest.myTest1, ControllerTest.myTest2, HandlerTest1.someTest, Test3.anotherTest1"
     *
     *      class/method can be specified in two forms
     *      - ClassName.<methodName> -  means specific method of specific class
     *      - ClassName -  means *all* test methodsToKeep of specific class
     *      Special case: *  - means "run all Local tests in the Org (exclude Packaged classes) "
     * @return if user passed any classes to run tests via "testsToRun" the return them here
     */
    def getTestMethodsByClassName(testsToRunStr: Option[String]): Map[String, Set[String]] = {
        var methodsByClassName = Map[String, Set[String]]()
        testsToRunStr match {
            case Some(x) if "*" == x =>
                methodsByClassName += "*" -> Set[String]()

            case Some(x) if !x.isEmpty =>
                for (classAndMethodStr <- x.split(","); if !classAndMethodStr.isEmpty) {
                    val classAndMethod = classAndMethodStr.split("\\.")
                    val className = classAndMethod(0).trim
                    if (className.isEmpty) {
                        throw new ActionError("invalid --testsToRun: " + x)
                    }
                    if (classAndMethod.size > 1) {
                        val methodName = classAndMethod(1).trim
                        if (methodName.isEmpty) {
                            throw new ActionError("invalid --testsToRun: " + x)
                        }
                        methodsByClassName = addToMap(methodsByClassName, className, methodName)
                    } else {
                        methodsByClassName += className -> Set[String]()
                    }
                }
            case _ => Map[String, Set[String]]()
        }
        methodsByClassName
    }

    private def addToMap(originalMap: Map[String, Set[String]], key: String, value: String): Map[String, Set[String]] = {
        originalMap.get(key)  match {
            case Some(list) =>
                val newList: Set[String] = list + value
                originalMap ++ Map(key -> newList)
            case None => originalMap ++ Map(key -> Set(value))
        }
    }

    def processTestResult(runTestResult: com.neowit.apex.RunTestsResult, session: Session): List[ProcessedTestFailure] = {
        //val metadataByXmlName = DescribeMetadata.getMap(session)
        val errorBuilder = List.newBuilder[ProcessedTestFailure]

        for ( failureMessage <- runTestResult.getFailures) {

            val problem = failureMessage.getMessage
            //val className = failureMessage.getName
            //now parse stack trace
            val stackTrace = failureMessage.getStackTrace
            if (null != stackTrace) {
                //each line is separated by '\n'
                var showProblem = true
                for (traceLine <- stackTrace.split("\n")) {
                    //Class.Test1.prepareData: line 13, column 1
                    parseStackTraceLine(traceLine) match {
                        case Some(StackTraceLine(typeName, fileName, methodName, line, column)) =>
                            val (_line, _column, filePath) = Deploy.getMessageData(session, problem, typeName, fileName)
                            val inMethod = if (methodName.isEmpty) None else Option(methodName)
                            val _problem = if (showProblem) problem else "...continuing stack trace: " +inMethod.getOrElse("")+ ". Details see above"
                            //responseWriter.println("ERROR", Map("line" -> line, "column" -> column, "filePath" -> filePath, "text" -> _problem))
                            errorBuilder +=
                                ProcessedTestFailure(
                                    id = None,
                                    `type` = failureMessage.getType,
                                    line = Option(line),
                                    column = Option(column),
                                    Option(filePath),
                                    message = _problem,
                                    methodName = inMethod,
                                    parentId = None
                                )
                            /*
                            if (showProblem) {
                                //responseWriter.println(new MessageDetail(testFailureMessage, Map("type" -> ERROR, "filePath" -> filePath, "text" -> problem)))
                                //actionResultBuilder.addDetail(MessageDetailMap(testFailureMessage, Map("type" -> ERROR, "line" -> line, "column" -> column, "filePath" -> filePath, "text" -> _problem)))
                                errorBuilder +=
                                    ProcessedTestFailure(
                                        id = None,
                                        `type` = failureMessage.getType,
                                        line = None,
                                        column = None,
                                        Option(filePath),
                                        message = problem,
                                        methodName = None,
                                        parentId = Option(failureMessage.getId)
                                    )
                            }
                            */
                        case None => //failed to parse anything meaningful, fall back to simple message
                            //responseWriter.println("ERROR", Map("line" -> -1, "column" -> -1, "filePath" -> "", "text" -> problem))
                            errorBuilder +=
                                ProcessedTestFailure(
                                    id = Option(failureMessage.getId),
                                    `type` = failureMessage.getType,
                                    line = None,
                                    column = None,
                                    filePath = None,
                                    message = problem,
                                    methodName = None,
                                    parentId = None
                                )
                            /*
                            if (showProblem) {
                                //responseWriter.println(new MessageDetail(testFailureMessage, Map("type" -> ERROR, "filePath" -> "", "text" -> problem)))
                                //actionResultBuilder.addDetail(MessageDetailMap(testFailureMessage, Map("type" -> ERROR, "filePath" -> "", "text" -> problem)))
                            }
                            */
                    }
                    showProblem = false
                }
            } else { //no stack trace, try to parse line/column/filePath from error message
                val typeName = failureMessage.getType
                val fileName = failureMessage.getName
                val (line, column, filePath) = Deploy.getMessageData(session, problem, typeName, fileName)
                //responseWriter.println("ERROR", Map("line" -> line, "column" -> column, "filePath" -> filePath, "text" -> problem))
                //responseWriter.println(new MessageDetail(testFailureMessage, Map("type" -> ResponseWriter.ERROR, "filePath" -> filePath, "text" -> problem)))
                errorBuilder +=
                    ProcessedTestFailure(
                        id = Option(failureMessage.getId),
                        `type` = failureMessage.getType,
                        line = Option(line),
                        column = Option(column),
                        Option(filePath),
                        message = problem,
                        methodName = None,
                        parentId = None
                    )
            }

        }
        //responseWriter.endSection("ERROR LIST")

        errorBuilder.result()
    }
    /**
     *
     * @param traceLine -
     *                  Class.Test1.prepareData: line 13, column 1
     *                  Class.Test1: line 19, column 1
     * @return (typeName, fileName, methodName, line, column)
     */
    protected def parseStackTraceLine(traceLine: String): Option[StackTraceLine] = {

        //Class.Test1.prepareData: line 13, column 1
        //val (typeName, fileName, methodName, line, column) =
        try {
            val TypeFileMethodLineColumnRegex(_typeName, _fileName, _methodName, _line, _column) = traceLine
            Some(StackTraceLine(_typeName, _fileName, _methodName, _line.toInt, _column.toInt))
        } catch {
            case _:scala.MatchError =>
                //Class.Test1: line 19, column 1
                try {
                    val TypeFileLineColumnRegex(_typeName, _fileName, _line, _column) = traceLine
                    Some(StackTraceLine(_typeName, _fileName, "", _line.toInt, _column.toInt))
                } catch {
                    case _:scala.MatchError =>
                        //... line 155, column 41: ....
                        Deploy.parseLineColFromErrorMessage(traceLine)  match {
                            case Some((_line, _column)) => Some(StackTraceLine("", "", "", _line, _column))
                            case None => None
                        }
                    case _:Throwable => None
                }
            case _:Throwable => None
        }
    }

}

case class StackTraceLine(typeName: String, fileName: String, methodName: String, line: Int, column: Int)