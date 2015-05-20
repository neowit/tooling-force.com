package com.neowit.apex.actions.tooling

import com.neowit.apex._
import com.neowit.apex.actions.{ActionHelp, ActionError, DeployModified}
import com.neowit.utils.{FileUtils, ResponseWriter}
import com.neowit.utils.ResponseWriter.Message

object RunTests {

    class RunTestResultTooling(sourceTestResult: com.sforce.soap.tooling.RunTestsResult) extends RunTestsResult {
        override def getCodeCoverage: Array[CodeCoverageResult] = sourceTestResult.getCodeCoverage.map(new CodeCoverageResultTooling(_))

        override def getCodeCoverageWarnings: Array[CodeCoverageWarning] = sourceTestResult.getCodeCoverageWarnings.map(new CodeCoverageWarningTooling(_))
    }

    class CodeCoverageResultTooling(sourceCoverageResult: com.sforce.soap.tooling.CodeCoverageResult) extends CodeCoverageResult {
        override def getNumLocations: Int = sourceCoverageResult.getNumLocations

        override def getLocationsNotCovered: Array[CodeLocation] = sourceCoverageResult.getLocationsNotCovered.map(new CodeLocationTooling(_))

        override def getName: String = sourceCoverageResult.getName

        override def getNumLocationsNotCovered: Int = sourceCoverageResult.getNumLocationsNotCovered
    }

    class CodeLocationTooling(sourceCodeLocation: com.sforce.soap.tooling.CodeLocation) extends CodeLocation {
        override def getLine: Int = sourceCodeLocation.getLine
    }

    class CodeCoverageWarningTooling(source: com.sforce.soap.tooling.CodeCoverageWarning) extends CodeCoverageWarning {
        override def getName: String = source.getName

        override def getMessage: String = source.getMessage
    }

}
/**
 * --testsToRun=* OR "comma separated list of class names",
 *      e.g. "ControllerTest, ControllerTest, HandlerTest1, Test3"
 *
 *      if --testsToRun=* (star) then run all tests in all Local classes (excluding installed packages)
 * --reportCoverage=true|false (defaults to false) - if true then generate code coverage file
 */
class RunTests extends DeployModified{

    override def getHelp: ActionHelp = new ActionHelp {
        override def getExample: String = ""

        override def getParamDescription(paramName: String): String = paramName match {
            case "reportCoverage" =>
                """--reportCoverage=true|false (defaults to false) - if true then generate code coverage file
                  |Note: makes sence only when --testsToRun is also specified
                """.stripMargin
            case
                "testsToRun" =>
                """--testsToRun=* OR "comma separated list of class names",
                  |       e.g. "ControllerTest, ControllerTest, HandlerTest1, Test3"
                  |
                  |       if --testsToRun=* (star) then run all tests in all Local classes (excluding installed packages)
                """.stripMargin
        }

        override def getParamNames: List[String] = List("ignoreConflicts", "checkOnly", "testsToRun", "reportCoverage")

        override def getSummary: String = "Deploy modified files and (if requested) run tests"

        override def getName: String = "deployModified"
    }
    override def act(): Unit = {

        val modifiedFiles = getFiles
        if (modifiedFiles.nonEmpty ) {
            //check if we can save using tooling API
            if (ToolingUtils.canUseTooling(session, modifiedFiles)) {
                val saveModified = new SaveModified().load[SaveModified](session)
                if (!saveModified.deploy(modifiedFiles, updateSessionDataOnSuccess = true)) {
                    responseWriter.println("RESULT=FAILURE")
                    responseWriter.println("FILE_COUNT=" + modifiedFiles.size)
                    responseWriter.println(new Message(ResponseWriter.ERROR, "Modified files detected and can not be saved with Tooling API. Save/Deploy modified first"))
                    return
                }

            }
        }
        val runTestsRequest = new com.sforce.soap.tooling.RunTestsRequest()

        getTestClassNames match {
            case List("*") =>
                runTestsRequest.setAllTests(true)
            case head :: tail =>
                runTestsRequest.setClasses((head :: tail).toArray[String])
        }
        val traceId = ToolingUtils.setupTrace(session, logger)
        logger.debug("Run tests")
        val runTestsResult = session.runTestsTooling(runTestsRequest)
        if (0 == runTestsResult.getNumFailures) {
            responseWriter.println("RESULT=SUCCESS")
            responseWriter.println(new Message(ResponseWriter.INFO, "Tests PASSED"))
        } else {
            responseWriter.println("RESULT=FAILURE")
        }
        ApexTestUtils.processCodeCoverage(new RunTests.RunTestResultTooling(runTestsResult), session, responseWriter) match {
            case Some(coverageFile) =>
                responseWriter.println("COVERAGE_FILE=" + coverageFile.getAbsolutePath)
            case _ =>
        }

        if ("None" != session.getConfig.logLevel) {
            ToolingUtils.getLastLogId(session) match {
                case Some(logId) =>
                    val log = ToolingUtils.getLog(session, logId)
                    if (!log.isEmpty) {
                        val logFile = config.getLogFile
                        FileUtils.writeFile(log, logFile)
                        responseWriter.println("LOG_FILE=" + logFile.getAbsolutePath)
                    }
                case None =>
            }
        }
    }

    private def getTestClassNames: List[String] = {
        val classNames = Set.newBuilder[String]
        config.getProperty("testsToRun") match {
            case Some(x) if "*" == x =>
                classNames += "*"

            case Some(x) if !x.isEmpty =>
                for (classAndMethodStr <- x.split(","); if !classAndMethodStr.isEmpty) {
                    //do split (Class.method) just in case ,to make sure that if method names are passed like
                    // in DeployModified then we can safely ignore those
                    val classAndMethod = classAndMethodStr.split("\\.")
                    val className = classAndMethod(0).trim
                    if (className.isEmpty) {
                        throw new ActionError("invalid --testsToRun: " + x)
                    }
                    classNames += className
                }
            case _ => Map[String, Set[String]]()
        }
        classNames.result().toList
    }
}
