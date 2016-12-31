package com.neowit.apex

import java.io.{File, PrintWriter}

import com.neowit.apex.actions.{ActionError, ActionResultBuilder, Deploy, DescribeMetadata}
import com.neowit.utils.{Config, FileUtils, ResponseWriter}
import com.neowit.utils.ResponseWriter._
import spray.json._
import DefaultJsonProtocol._
import com.neowit.utils.JsonUtils._


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

object ApexTestUtils {
    //Class.Test1: line 19, column 1
    private val TypeFileLineColumnRegex = """.*(Class|Trigger)\.(\w*).*line (\d+), column (\d+).*""".r
    //Class.Test1.prepareData: line 13, column 1
    private val TypeFileMethodLineColumnRegex = """.*(Class|Trigger)\.(\w*)\.(\w*).*line (\d+), column (\d+).*""".r

    /**
     * process code coverage results
     * @param runTestResult - deployDetails.getRunTestResult
     * @return set of file names (e.g. MyClass) for which we had coverage
     */
    def processCodeCoverage(runTestResult: com.neowit.apex.RunTestsResult, session: Session,
                            actionResultBuilder: ActionResultBuilder ): Option[File] = {
        val config: Config = session.getConfig
        val metadataByXmlName = DescribeMetadata.getMap(session)
        val (classDir, classExtension) = metadataByXmlName.get("ApexClass") match {
            case Some(describeObject) => (describeObject.getDirectoryName, describeObject.getSuffix)
            case None => ("classes", "cls")
        }
        val (triggerDir, triggerExtension) = metadataByXmlName.get("ApexTrigger") match {
            case Some(describeObject) => (describeObject.getDirectoryName, describeObject.getSuffix)
            case None => ("triggers", "trigger")
        }
        //only display coverage details of files included in deployment package
        val coverageDetails = WarnMessage("Code coverage details")
        val hasCoverageData = runTestResult.getCodeCoverage.nonEmpty
        var coverageFile: Option[File] = None
        val coverageWriter = config.getProperty("reportCoverage").getOrElse("false") match {
            case "true" if hasCoverageData =>
                coverageFile = Some(FileUtils.createTempFile("coverage", ".txt"))
                val writer = new PrintWriter(coverageFile.get)
                Some(writer)
            case _ => None
        }

        if (runTestResult.getCodeCoverage.nonEmpty) {
            //responseWriter.println(coverageDetails)
            actionResultBuilder.addMessage(coverageDetails)
        }

        val reportedNames = Set.newBuilder[String]
        val coverageRelatedMessages = Map.newBuilder[String, MessageDetail]
        for ( coverageResult <- runTestResult.getCodeCoverage) {
            reportedNames += coverageResult.getName
            val linesCovered = coverageResult.getNumLocations - coverageResult.getNumLocationsNotCovered
            val coveragePercent = if (coverageResult.getNumLocations > 0) linesCovered * 100 / coverageResult.getNumLocations else 0
            coverageRelatedMessages += coverageResult.getName -> MessageDetailMap(coverageDetails,
                Map("text" ->
                    (coverageResult.getName +
                        ": lines total " + coverageResult.getNumLocations +
                        "; lines not covered " + coverageResult.getNumLocationsNotCovered +
                        "; covered " + coveragePercent + "%"),
                    "type" -> (if (coveragePercent >= 75) ResponseWriter.INFO else ResponseWriter.WARN)
                )
            )

            coverageWriter match {
                case Some(writer) =>
                    val filePath = session.getRelativePath(classDir, coverageResult.getName + "." + classExtension) match {
                        case Some(relPath) => Some(relPath)
                        case None =>
                            //check if this is a trigger name
                            session.getRelativePath(triggerDir, coverageResult.getName + "." + triggerExtension) match {
                                case Some(relPath) => Some(relPath)
                                case None => None
                            }
                    }
                    filePath match {
                        case Some(relPath) =>
                            val locations = List.newBuilder[Int]
                            for (codeLocation <- coverageResult.getLocationsNotCovered) {
                                locations += codeLocation.getLine
                            }
                            val coverageJSON: JsValue = Map("path" -> relPath, "linesTotalNum" -> coverageResult.getNumLocations,
                                "linesNotCoveredNum" -> coverageResult.getNumLocationsNotCovered,
                                "linesNotCovered" -> locations.result().toJson ).toJson
                            // end result looks like so:
                            // {"path" : "src/classes/AccountController.cls", "linesNotCovered" : [1, 2, 3,  4, 15, 16,...]}
                            writer.println(coverageJSON.compactPrint)
                        case None =>
                    }
                case None =>
            }
        }
        //dump coverage related MessageDetail-s into the response file, making sure that messages are sorted by file name
        val messageDetailsSortedByFileName = coverageRelatedMessages.result().toSeq.sortBy(_._1.toLowerCase).map(_._2).toList
        //responseWriter.println(messageDetailsSortedByFileName)
        actionResultBuilder.addDetails(messageDetailsSortedByFileName)

        val coverageMessage = WarnMessage("Code coverage warnings")
        if (runTestResult.getCodeCoverageWarnings.nonEmpty) {
            //responseWriter.println(coverageMessage)
            actionResultBuilder.addMessage(coverageMessage)
        }
        val reportedNamesSet = reportedNames.result()
        for ( coverageWarning <- runTestResult.getCodeCoverageWarnings) {
            if (null != coverageWarning.getName) {
                if (!reportedNamesSet.contains(coverageWarning.getName)) {
                    //responseWriter.println(MessageDetailMap(coverageMessage, Map("text" -> (coverageWarning.getName + ": " + coverageWarning.getMessage))))
                    actionResultBuilder.addDetail(MessageDetailMap(coverageMessage, Map("text" -> (coverageWarning.getName + ": " + coverageWarning.getMessage))))
                }
            } else {
                //responseWriter.println(MessageDetailMap(coverageMessage, Map("text" -> coverageWarning.getMessage)))
                actionResultBuilder.addDetail(MessageDetailMap(coverageMessage, Map("text" -> coverageWarning.getMessage)))
            }

        }

        coverageWriter match {
            case Some(writer) =>
                writer.close()
            case _ =>
        }
        coverageFile
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

    def processTestResult(runTestResult: com.neowit.apex.RunTestsResult, session: Session,
                          actionResultBuilder: ActionResultBuilder): Unit = {
        val metadataByXmlName = DescribeMetadata.getMap(session)


        val testFailureMessage = ErrorMessage("Test failures")
        if (null != runTestResult && runTestResult.getFailures.nonEmpty) {
            //responseWriter.println(testFailureMessage)
            actionResultBuilder.addMessage(testFailureMessage)
        }
        if (null == runTestResult || runTestResult.getFailures.isEmpty) {
            //responseWriter.println(new Message(ResponseWriter.INFO, "Tests PASSED"))
            actionResultBuilder.addMessage(InfoMessage("Tests PASSED"))
        }
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
                        case Some((typeName, fileName, methodName, line, column)) =>
                            val (_line, _column, filePath) = Deploy.getMessageData(session, problem, typeName, fileName, metadataByXmlName)
                            val inMethod = if (methodName.isEmpty) "" else " in method " +methodName
                            val _problem = if (showProblem) problem else "...continuing stack trace" +inMethod+ ". Details see above"
                            //responseWriter.println("ERROR", Map("line" -> line, "column" -> column, "filePath" -> filePath, "text" -> _problem))
                            if (showProblem) {
                                //responseWriter.println(new MessageDetail(testFailureMessage, Map("type" -> ResponseWriter.ERROR, "filePath" -> filePath, "text" -> problem)))
                                actionResultBuilder.addDetail(MessageDetailMap(testFailureMessage, Map("type" -> ResponseWriter.ERROR, "line" -> line, "column" -> column, "filePath" -> filePath, "text" -> _problem)))
                            }
                        case None => //failed to parse anything meaningful, fall back to simple message
                            //responseWriter.println("ERROR", Map("line" -> -1, "column" -> -1, "filePath" -> "", "text" -> problem))
                            if (showProblem) {
                                //responseWriter.println(new MessageDetail(testFailureMessage, Map("type" -> ResponseWriter.ERROR, "filePath" -> "", "text" -> problem)))
                                actionResultBuilder.addDetail(MessageDetailMap(testFailureMessage, Map("type" -> ResponseWriter.ERROR, "filePath" -> "", "text" -> problem)))
                            }
                    }
                    showProblem = false
                }
            } else { //no stack trace, try to parse cine/column/filePath from error message
            val typeName = failureMessage.getType
                val fileName = failureMessage.getName
                val (line, column, filePath) = Deploy.getMessageData(session, problem, typeName, fileName, metadataByXmlName)
                //responseWriter.println("ERROR", Map("line" -> line, "column" -> column, "filePath" -> filePath, "text" -> problem))
                //responseWriter.println(new MessageDetail(testFailureMessage, Map("type" -> ResponseWriter.ERROR, "filePath" -> filePath, "text" -> problem)))
                actionResultBuilder.addDetail(MessageDetailMap(testFailureMessage, Map("type" -> ResponseWriter.ERROR, "line" -> line, "column" -> column, "filePath" -> filePath, "text" -> problem)))
            }

            //responseWriter.println("ERROR", Map("line" -> line, "column" -> column, "filePath" -> filePath, "text" -> problem))
        }
        //responseWriter.endSection("ERROR LIST")

        Unit
    }
    /**
     *
     * @param traceLine -
     *                  Class.Test1.prepareData: line 13, column 1
     *                  Class.Test1: line 19, column 1
     * @return (typeName, fileName, methodName, line, column)
     */
    protected def parseStackTraceLine(traceLine: String): Option[(String, String, String, Int, Int )] = {

        //Class.Test1.prepareData: line 13, column 1
        //val (typeName, fileName, methodName, line, column) =
        try {
            val TypeFileMethodLineColumnRegex(_typeName, _fileName, _methodName, _line, _column) = traceLine
            Some((_typeName, _fileName, _methodName, _line.toInt, _column.toInt))
        } catch {
            case _:scala.MatchError =>
                //Class.Test1: line 19, column 1
                try {
                    val TypeFileLineColumnRegex(_typeName, _fileName, _line, _column) = traceLine
                    Some((_typeName, _fileName, "", _line.toInt, _column.toInt))
                } catch {
                    case _:scala.MatchError =>
                        //... line 155, column 41: ....
                        Deploy.parseLineColumn(traceLine)  match {
                            case Some((_line, _column)) => Some(("", "", "", _line, _column))
                            case None => None
                        }
                    case _:Throwable => None
                }
            case _:Throwable => None
        }
    }

}