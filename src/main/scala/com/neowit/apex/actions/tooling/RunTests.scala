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

package com.neowit.apex.actions.tooling

import java.io.File

import com.neowit.apex._
import com.neowit.apex.actions.SoqlQuery.ResultRecord
import com.neowit.apex.actions.tooling.RunTests.TestResultWithJobId
import com.neowit.apex.actions._
import com.neowit.utils.FileUtils
import com.sforce.soap.tooling.sobject.{ApexTestQueueItem, AsyncApexJob}
import spray.json._

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

object RunTests {

    class TestResultWithJobId(val sourceTestResult: com.sforce.soap.tooling.RunTestsResult, val asyncJobId: Option[String], val useLastLog: Boolean = false )

    class RunTestResultTooling(sourceTestResult: com.sforce.soap.tooling.RunTestsResult) extends com.neowit.apex.RunTestsResult {
        override def getCodeCoverage: Array[CodeCoverageResult] = sourceTestResult.getCodeCoverage.map(new CodeCoverageResultTooling(_))
        override def getCodeCoverageWarnings: Array[CodeCoverageWarning] = sourceTestResult.getCodeCoverageWarnings.map(new CodeCoverageWarningTooling(_))
        override def getFailures: Array[com.neowit.apex.RunTestFailure] = sourceTestResult.getFailures.map(new RunTestFailure(_))
    }

    class CodeCoverageResultTooling(sourceCoverageResult: com.sforce.soap.tooling.CodeCoverageResult) extends com.neowit.apex.CodeCoverageResult {
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

    class RunTestFailure(source: com.sforce.soap.tooling.RunTestFailure) extends com.neowit.apex.RunTestFailure {
        override def getId: String = source.getId

        override def getType: String = source.getType

        override def getMessage: String = source.getMessage

        override def getStackTrace: String = source.getStackTrace

        override def getName: String = source.getName

        override def getMethodName: String = source.getMethodName
    }

}
/**
 * --testsToRun=* OR "comma separated list of class names",
 *      e.g. "ControllerTest.test1, ControllerTest.test2, HandlerTest1, Test3"
 *
 *      if --testsToRun=* (star) then run all tests in all Local classes (excluding installed packages)
 * --reportCoverage=true|false (defaults to false) - if true then generate code coverage file
 * --async=true|false (defaults to false) - if true then use runTestsAsynchronous
 */
class RunTests extends DeployModified with RunTestJsonSupport {

    override def getHelp: ActionHelp = new ActionHelp {
        override def getExample: String = ""

        override def getParamDescription(paramName: String): String = paramName match {
            case "reportCoverage" =>
                """--reportCoverage=true|false (defaults to false) - if true then generate code coverage file
                """.stripMargin
            case "testsToRun" =>
                """--testsToRun=* OR "comma separated list of class names",
                  |       e.g. "ControllerTest,ControllerTest,HandlerTest1,Test3"
                  |
                  |       if --testsToRun=* (star) then run all tests in all Local classes (excluding installed packages)
                  |       NOTE 1: if --async=false then all test methods in a synchronous test run must be in the same class.
                  |       NOTE 2: if --testLevel is anything other than RunSpecifiedTests then value of --testsToRun is ignored
                """.stripMargin
            case "testSuitesToRun" =>
                """--testSuitesToRun="comma separated list of test suite names",
                  |       e.g. "TestSuite1,TestSuite2"
                  |
                """.stripMargin
            case "async" =>
                """--async=true|false (defaults to false) - if true then use runTestsAsynchronous.
                   |      Running tests asynchronously allows methods runTestsAsynchronous() to process in parallel, cutting down your test run times.
                   |      in --async mode you can specify individual test methods
                   |      e.g. --testsToRun="ControllerTest.method1,ControllerTest.myTest2,Test3"
                   |      This flag is ignored if --testSuitesToRun=... is provided. Test Suites can only be run in "async" mode.
                """.stripMargin
            case "traceFlagConfig" =>
                """--traceFlagConfig=/full/path/to/traceflag.conf - OPTIONAL
                  |  traceflag.conf must contain a JSON object with "type":"value" pairs.
                  |  All log types [ApexCode, ApexProfiling, ... Workflow] must be specified.
                  |  File content format is as follows:
                  |  "{"ApexCode": "Debug", "ApexProfiling": "Error", "Callout": "Error", "Database": "Error",
                  |    "System": "Error", "Validation": "Error", "Visualforce": "Error", "Workflow": "Error"}"
                """.stripMargin
            case "maxFailedTests" =>
                """--maxFailedTests=0
                  |     Optional: if not provided - defaults to 0.
                  |     To allow all tests in your org to run, regardless of how many tests fail, set maxFailedTests to -1.
                  |     To stop the test run from executing new tests after a given number of tests fail, set maxFailedTests
                  |     to an integer value from 0 to 1,000,000. This integer value sets the maximum allowable test failures.
                  |     A value of 0 causes the test run to stop if any failure occurs.
                  |     A value of 1 causes the test run to stop on the second failure, and so on.
                """.stripMargin
            case "testLevel" =>
                """--testLevel=RunLocalTests
                  |     Optional: if not provided - defaults to RunSpecifiedTests.
                  |     Used only with --async=true
                  |     Permissible values include:
                  |     RunSpecifiedTests - Only the tests that you specify are run
                  |     RunLocalTests - All tests in your org are run, except the ones that originate from installed managed packages.
                  |                     value of --testsToRun parameter is ignored
                  |     RunAllTestsInOrg - All tests are run. The tests include all tests in your org, including tests of managed packages.
                  |                     value of --testsToRun parameter is ignored
                  |

                """.stripMargin
        }

        override def getParamNames: List[String] = List("async", "testsToRun", "testSuitesToRun", "reportCoverage", "traceFlagConfig", "maxFailedTests")

        override def getSummary: String = "Run tests using Tooling API"

        override def getName: String = "runTestsTooling"
    }
    protected override def act()(implicit ec: ExecutionContext): Future[ActionResult] = {

        val modifiedFiles = getFiles
        if (modifiedFiles.nonEmpty ) {
            //check if we can save using tooling API
            if (ToolingUtils.canUseTooling(session, modifiedFiles)) {

                val ignoreConflicts = config.getProperty("ignoreConflicts").getOrElse("false").toBoolean
                val conflictReport = if (ignoreConflicts) DeploymentConflictsReport(hasConflicts = false) else getConflicts(modifiedFiles)

                val deploymentResult =
                    if (conflictReport.hasConflicts) {
                        val checkOnly = getSessionConfig.isCheckOnly
                        DeploymentReport(
                            isSuccess = false,
                            isCheckOnly = checkOnly,
                            failureReportOpt = None,
                            conflictsReportOpt = Option(conflictReport)
                        )
                    } else {
                        val saveModified = new SaveModified().load[SaveModified](session)
                        saveModified.deploy(modifiedFiles, updateSessionDataOnSuccess = true)
                    }
                if (!deploymentResult.isSuccess) {
                    //responseWriter.println("RESULT=FAILURE")
                    //responseWriter.println("FILE_COUNT=" + modifiedFiles.size)
                    //responseWriter.println(new Message(ERROR, "Modified files detected and can not be saved with Tooling API. Fix-Problems/Save/Deploy modified first"))

                    return Future.successful(
                        ActionFailure(
                            /*
                            List(
                                KeyValueMessage(Map("FILE_COUNT" -> modifiedFiles.size)),
                                ErrorMessage("Modified files detected and can not be saved with Tooling API. Fix-Problems/Save/Deploy modified first")
                            )
                            */
                            com.neowit.response.DeployModifiedResult(
                                deploymentReport = deploymentResult
                            )
                        )
                    )
                }

            }
        }

        val traceIdOpt = config.getProperty("traceFlagConfig") match {
          case Some(filePath) =>
              ChangeLogLevels.setupTraceFlag(Some(filePath), session, logger, Some("DEVELOPER_LOG"), session.getUserId) match {
                  case Success(traceId) => Some(traceId)
                  case Failure(ex) =>
                      throw ex

              }
          case None => None
        }

        val actionResult =
            try {
                val runTestsResultWithJobId = runTests()
                val runTestsResult = runTestsResultWithJobId.sourceTestResult
                /*
                if (0 == runTestsResult.getNumFailures) {
                    //responseWriter.println("RESULT=SUCCESS")
                } else {
                    //responseWriter.println("RESULT=FAILURE")
                }
                */
                val toolingRunTestResult = new RunTests.RunTestResultTooling(runTestsResult)
                val coverageReportOpt = config.getProperty("reportCoverage") match {
                    case Some("true") =>
                        ApexTestUtils.processCodeCoverage(toolingRunTestResult, session)
                    case _ =>
                        None
                }
                /*
                ApexTestUtils.processCodeCoverage(toolingRunTestResult, session) match {
                    case Some(coverageFile) =>
                        //responseWriter.println("COVERAGE_FILE=" + coverageFile.getAbsolutePath)
                        actionResultBuilder.addMessage(KeyValueMessage(Map("COVERAGE_FILE" -> coverageFile.getAbsolutePath)))
                    case _ =>
                }
                */
                val testFailures = ApexTestUtils.processTestResult(toolingRunTestResult, session)

                val (logFilePathByClassName, logFileOpt) =
                    if (traceIdOpt.isDefined) {
                        //retrieve log files
                        runTestsResultWithJobId.asyncJobId match {
                            case Some(jobId) =>
                                val logIdByClassName = getLogIds(jobId)
                                val logFileByClassName = getLogFileByClassName(logIdByClassName)
                                val _logFilePathByClassName = logFileByClassName.mapValues(_.getAbsolutePath)
                                //responseWriter.println("LOG_FILE_BY_CLASS_NAME=" + logFilePathByClassName.toJson)
                                (_logFilePathByClassName, None)
                            case None => //test was run in Synchronous mode
                                val logOpt =
                                    if (runTestsResultWithJobId.useLastLog) {
                                        LogActions.getLastLogId(session) match {
                                            case Some(logId) =>
                                                val log = LogActions.getLog(session, logId)
                                                if (!log.isEmpty) {
                                                    val logFile = getProjectConfig.getLogFile
                                                    FileUtils.writeFile(log, logFile)
                                                    //responseWriter.println("LOG_FILE=" + logFile.getAbsolutePath)
                                                    Option(logFile)
                                                } else {
                                                    None
                                                }
                                            case None =>
                                                None
                                        }
                                    } else {
                                        None
                                    }
                                (Map.empty[String, String], logOpt)
                        }
                    } else {
                        (Map.empty[String, String], None)
                    }
                if (testFailures.isEmpty) {
                    ActionSuccess(com.neowit.response.RunTestsResult(testFailures, logFilePathByClassName, logFileOpt, coverageReportOpt))
                } else {
                    ActionFailure(com.neowit.response.RunTestsResult(testFailures, logFilePathByClassName, logFileOpt, coverageReportOpt))
                }
            } catch {
                case ex: Session.RestCallException =>
                    ex.getRestErrorCode match {
                        case Some("ALREADY_IN_PROCESS") =>
                            //responseWriter.println("RESULT=FAILURE")
                            //responseWriter.println(new Message(ERROR, "ALREADY_IN_PROCESS => " + ex.getRestMessage.getOrElse("")))
                            //actionResultBuilder.setResultType(FAILURE)
                            //actionResultBuilder.addMessage(ErrorMessage("ALREADY_IN_PROCESS => " + ex.getRestMessage.getOrElse("")))
                            ActionFailure("ALREADY_IN_PROCESS => " + ex.getRestMessage.getOrElse(""))
                        case Some("INVALID_INPUT") =>
                            ActionFailure("INVALID_INPUT => " + ex.getRestMessage.getOrElse(""))
                        case _ =>
                            throw ex
                    }
            }
        //clean-up, remove trace flag we set before the start of the test
        traceIdOpt match {
          case Some(traceId) =>
              ChangeLogLevels.deleteTraceFlag(traceId, session, logger)
          case None =>
        }
        Future.successful(actionResult)
    }

    private def runTests(): RunTests.TestResultWithJobId = {
        val isAsync = config.getProperty("async").getOrElse("false").toBoolean || config.getProperty("testSuitesToRun").nonEmpty
        if (isAsync) {
            runTestsAsynchronous()
        } else {
            //new RunTests.TestResultWithJobId(runTestsSynchronous2(), None, useLastLog = true)
            new RunTests.TestResultWithJobId(runTestsSynchronous(), None, useLastLog = true)
        }

    }
    /*
    private def runTestsSynchronous2(): com.sforce.soap.tooling.RunTestsResult = {
        val runTestsRequest = new com.sforce.soap.tooling.RunTestsRequest()

        getTestClassNames match {
            case List("*") =>
                runTestsRequest.setAllTests(true)
            case head :: tail =>
                val classData = generateTestRequestPayload("RunSpecifiedTests", isAsync = false)

                classData.get("tests") match {
                    case Some(JsArray(testsJson)) =>
                        val classes = testsJson.map(_.compactPrint)
                        //runTestsRequest.setClasses(classes.toArray)
                        //runTestsRequest.setClasses(classData)
                    case _ =>
                }
                //runTestsRequest.setClasses((head :: tail).toArray[String])
                //runTestsRequest.setClasses(classData)
            case _ => // no classes provided
        }
        //logger.info("Run tests Synchronous")
        //val runTestsResult = session.runTestsTooling(runTestsRequest)
        var runTestsResult: com.sforce.soap.tooling.RunTestsResult = null
        com.neowit.utils.Logging.repeatingInfo(logger,
            { runTestsResult = session.runTestsTooling(runTestsRequest) },
            "Run tests Synchronous ...", scala.Console.out )(com.neowit.TcpServer.system.scheduler)
        runTestsResult
    }
    */

    private def submitAsyncTestRunJob(resource: String, jsonRequest: String): RunTests.TestResultWithJobId = {
        session.postRestContentTooling(resource, jsonRequest) match {
            case Some(jsonAst) =>
                val asyncJobId = jsonAst.asInstanceOf[JsString].value
                waitAsyncJob(asyncJobId, reportTestProgress) match {
                    case Some(asyncApexJob) if Set("Completed", "Failed").contains(asyncApexJob.getStatus) =>
                        new TestResultWithJobId(generateRunTestResult(asyncApexJob), Some(asyncJobId))

                    case Some(asyncApexJob) =>
                        logger.debug("asyncApexJob=" + asyncApexJob)
                        //TODO - job is aborted or something else
                        new TestResultWithJobId(new com.sforce.soap.tooling.RunTestsResult(), None)
                    case None =>
                        new TestResultWithJobId(new com.sforce.soap.tooling.RunTestsResult(), None)
                }

            case None =>
                new TestResultWithJobId(new com.sforce.soap.tooling.RunTestsResult(), None)
        }
    }


    private def runTestsSynchronous(): com.sforce.soap.tooling.RunTestsResult = {
        logger.info("Run tests Synchronous")
        val maxFailedTests = session.config.getProperty("maxFailedTests").getOrElse("0")

        val specificTestsData = generateTestRequestPayload("RunSpecifiedTests", isAsync = false)
        val maxFailedTestsData = Map("maxFailedTests" -> maxFailedTests.toJson)

        val jsonRequest = (specificTestsData ++ maxFailedTestsData).toJson.compactPrint
        //submitTestRunJob("/runTestsSynchronous/", jsonRequest)
        session.postRestContentTooling("/runTestsSynchronous/", jsonRequest) match {
            case Some(jsonAst) =>
                generateRunTestResult(jsonAst)
            case None =>
                val res = new com.sforce.soap.tooling.RunTestsResult()
                val failure = new com.sforce.soap.tooling.RunTestFailure()
                failure.setMessage("Tests have not been executed, remote resource returned empty response")
                res.setFailures(Array(failure))
                res.setNumFailures(1)
                res
        }
    }
    /**
      * @param jsonAst response returned by runTestsSynchronous
      * @return
      */
    private def generateRunTestResult(jsonAst: JsValue): com.sforce.soap.tooling.RunTestsResult = {
        //logger.debug(jsonAst.prettyPrint)
        val internalResult = jsonAst.convertTo[RunTestJsonSupport.RunTestsSynchronousResult]
        RunTestConversions.toRunTestResult(internalResult)
    }

    /**
      * generate payload for specific Class[.method] and Test Suite Ids
      * @param testLevel - must be "RunSpecifiedTests"
      * @param isAsync - if true then test suites are taken into account, otherwise ignored
      * @return empty result if testLevel <> RunSpecifiedTests, otherwise returns json ready part for specific tests payload
      */
    private def generateTestRequestPayload(testLevel: String, isAsync: Boolean = true): Map[String, JsValue] = {
        // specific tests or suites can only be specified with testLevel=RunSpecifiedTests
        val canIncludeSpecificTests = "RunSpecifiedTests".equalsIgnoreCase(testLevel)

        if (canIncludeSpecificTests) {
            val records: List[SoqlQuery.ResultRecord] =
                getTestClassNames match {
                    case List("*") =>
                        SoqlQuery.getQueryIteratorTooling(session, "select Id, Name from ApexClass where Status = 'Active' and NamespacePrefix = ''")
                            .map(new ResultRecord(_))
                            .toList
                    case head :: tail =>
                        val classNames = (head :: tail).mkString("','")
                        SoqlQuery.getQueryIteratorTooling(session, s"select Id, Name from ApexClass where Name in ('$classNames')")
                            .map(new ResultRecord(_))
                            .toList
                    case Nil =>
                        List.empty

                }
            //Map("classids" -> "id1,id2,id3")
            val classIdData = config.getProperty("testsToRun") match {
                case Some(_) => // test class names provided

                    val classIds = records.map(_.getFieldAsString("Id").get).mkString(",")
                    if (classIds.nonEmpty) Map("classids" -> classIds.toJson) else Map.empty[String, JsValue]
                case _ => Map.empty[String, JsValue]
            }

            //Map(suiteids -> "id1,id2,id3")
            val (suiteIds: Map[String, JsValue], classesAndMethodsOpt: Option[Map[String, JsValue]]) =
                getTestSuiteNames match {
                    case names @ head :: tail if isAsync =>
                        // test suite names provided
                        val suiteIds = TestSuiteActions.getTestSuiteIdByName(names, session).values.mkString(",")
                        if (suiteIds.nonEmpty) {
                            (Map("suiteids" -> suiteIds.toJson), None)
                        } else {
                            (Map.empty, None)
                        }
                    case _ =>
                        // specific tests can be only run if no test suites provided
                        // {"tests":<tests array>}
                        val _clsAndMethods =
                            getTestClassMethodJson(
                                records.map(r => r.getFieldAsString("Name").get -> r.getFieldAsString("Id").get).toMap
                            )
                        if (_clsAndMethods.nonEmpty) {
                            (Map.empty, Option(_clsAndMethods))
                        } else {
                            (Map.empty, None)
                        }
                }
            if (isAsync) {
                classesAndMethodsOpt match {
                    case Some(classesAndMethods) => classesAndMethods
                    case None => classIdData ++ suiteIds
                }
            } else {
                classesAndMethodsOpt match {
                    case Some(classesAndMethods) => classesAndMethods
                    case None => classIdData
                }
            }

        } else {
            Map.empty[String, JsValue]
        }

    }
    /**
     * ID: 707g000000MRjHdAAL
     * SELECT ApexClassId,CompletedDate,CreatedById,CreatedDate,ExtendedStatus,Id,JobItemsProcessed,JobType,
     * LastProcessed,LastProcessedOffset,MethodName,NumberOfErrors,ParentJobId,Status,TotalJobItems FROM AsyncApexJob
     * @return
     */
    private def runTestsAsynchronous(): RunTests.TestResultWithJobId = {
        logger.info("Run tests Asynchronous")
        val testLevel = config.getProperty("testLevel").getOrElse("RunSpecifiedTests")
        val maxFailedTests = session.config.getProperty("maxFailedTests").getOrElse("0")

        val specificTestsData = generateTestRequestPayload(testLevel)
        val testsLevelData = Map("testLevel" -> testLevel.toJson)
        val maxFailedTestsData = Map("maxFailedTests" -> maxFailedTests.toJson)

        val jsonRequest = (specificTestsData ++ testsLevelData ++ maxFailedTestsData).toJson.compactPrint

        submitAsyncTestRunJob("/runTestsAsynchronous/", jsonRequest)
        /*
        session.postRestContentTooling("/runTestsAsynchronous/", jsonRequest) match {
          case Some(jsonAst) =>
              val asyncJobId = jsonAst.asInstanceOf[JsString].value
              waitAsyncJob(asyncJobId, reportTestProgress) match {
                  case Some(asyncApexJob) if Set("Completed", "Failed").contains(asyncApexJob.getStatus) =>
                      new TestResultWithJobId(generateRunTestResult(asyncApexJob), Some(asyncJobId))

                  case Some(asyncApexJob) =>
                      logger.debug("asyncApexJob=" + asyncApexJob)
                      //TODO - job is aborted or something else
                      new TestResultWithJobId(new com.sforce.soap.tooling.RunTestsResult(), None)
                  case None =>
                      new TestResultWithJobId(new com.sforce.soap.tooling.RunTestsResult(), None)
              }

          case None =>
              new TestResultWithJobId(new com.sforce.soap.tooling.RunTestsResult(), None)
        }
        */
        //val asyncJobId = session.postRestContentTooling("/runTestsAsynchronous/", Map("classids" -> classIds.mkString(","))).get
    }


    private def getLogIds(asyncApexJobId: String): Map[String, String] = {
        val query =
            s"""select ApexClass.Name, ApexClassId, ApexLogId, Message, MethodName, Outcome, QueueItemId, StackTrace, TestTimestamp
               |from ApexTestResult
               |where AsyncApexJobId = '$asyncApexJobId'
            """.stripMargin

        val queryIterator = SoqlQuery.getQueryIteratorTooling(session, query).map(new ResultRecord(_))
        val logIdByClassName = queryIterator.
                map(r => r.getFieldAsString("ApexClass.Name") -> r.getFieldAsString("ApexLogId")).
                filterNot(pair => pair._1.isEmpty || pair._2.isEmpty).
                map(pair => pair._1.get -> pair._2.get).toMap
        logIdByClassName
    }

    private def getLogFileByClassName(logIdByClassName: Map[String, String] ): Map[String, File] = {
        val x = logIdByClassName.par.map{
            case (className, logId) =>
                val logText = LogActions.getLog(session, logId)
                if (logText.nonEmpty) {
                    val tempFile = FileUtils.createTempFile(className, ".log")
                    FileUtils.writeFile(logText, tempFile)
                    className -> Some(tempFile)
                } else {
                    className -> None
                }
        }.filterNot(_._2.isEmpty).mapValues(_.get)

        //convert back to sequential
        x.seq.toMap
    }

    private def generateRunTestResult(asyncApexJob: AsyncApexJob): com.sforce.soap.tooling.RunTestsResult = {
        val runTestResult = new com.sforce.soap.tooling.RunTestsResult()
        //runTestResult.setTotalTime()
        val query =
            s"""select ApexClass.Name, ApexClassId, ApexLogId, Message, MethodName, Outcome, QueueItemId, StackTrace, TestTimestamp
                |from ApexTestResult
                |where AsyncApexJobId = '${asyncApexJob.getId}'
            """.stripMargin


        val queryIterator = SoqlQuery.getQueryIteratorTooling(session, query).map(new ResultRecord(_))

        if (queryIterator.isEmpty) {
            runTestResult.setNumFailures(0)
            runTestResult.setNumTestsRun(0)
            runTestResult.setSuccesses(Array[com.sforce.soap.tooling.RunTestSuccess]())
            return runTestResult
        }

        val iterator = queryIterator

        //val failures = Array.newBuilder[com.sforce.soap.tooling.RunTestFailure]
        val successes = Array.newBuilder[com.sforce.soap.tooling.RunTestSuccess]
        for(record <- iterator) {

            val className = record.getFieldAsObject("ApexClass").get.getFieldAsString("Name").get
            val methodName = record.getFieldAsString("MethodName").get
            val outcome = record.getFieldAsString("Outcome").get
            logger.info(s"$className.$methodName => $outcome")
            //Pass, Failed, CompileFail, Skip
            outcome match {
                case "Pass" =>
                    val success = new com.sforce.soap.tooling.RunTestSuccess()
                    success.setMethodName(methodName)
                    successes += success
                case x => logger.debug("outcome: " + x)
            }

        }
        runTestResult.setFailures(getTestRunFailures(asyncApexJob.getId))
        runTestResult.setNumFailures(runTestResult.getFailures.length)
        runTestResult.setSuccesses(successes.result())
        runTestResult.setNumTestsRun(runTestResult.getNumFailures + runTestResult.getSuccesses.length)
        runTestResult.setCodeCoverage(getCoverageResult)

        runTestResult
    }

    private def getTestRunFailures(asyncJobId: String): Array[com.sforce.soap.tooling.RunTestFailure] = {
        val failures = Array.newBuilder[com.sforce.soap.tooling.RunTestFailure]
        val soql =
            s"""select ApexClassId, ApexClass.Name, ApexLogId, AsyncApexJobId, Message, MethodName, Outcome, StackTrace, TestTimestamp
               | from ApexTestResult where AsyncApexJobId = '$asyncJobId' and Outcome in ('Fail', 'CompileFail')""".stripMargin
        for (result <- SoqlQuery.getQueryIteratorTyped[com.sforce.soap.tooling.sobject.ApexTestResult](session, session.queryTooling(soql))) {
            val failure = new com.sforce.soap.tooling.RunTestFailure
            failure.setId(result.getApexClassId)
            failure.setName(result.getApexClass.getFullName)
            failure.setMessage(result.getMessage)
            failure.setMethodName(result.getMethodName)
            //result.getSystemModstamp returns NPE
            //failure.setTime(result.getSystemModstamp.getTimeInMillis - result.getTestTimestamp.getTimeInMillis)
            failure.setStackTrace(result.getStackTrace)
            failure.setType("Class") //all tests are in Classes
            failures += failure
        }
        failures.result()
    }

    private def getCoverageResult: Array[com.sforce.soap.tooling.CodeCoverageResult] = {
        import com.sforce.soap.tooling._
        //load only results for classes covered by the test
        val query =
            s""" SELECT ApexClassorTriggerId, ApexClassorTrigger.Name, NumLinesCovered, NumLinesUncovered,
               | Coverage
               | FROM ApexCodeCoverageAggregate
               | where NumLinesCovered > 0""".stripMargin
        val coverageResultBuilder = Array.newBuilder[CodeCoverageResult]
        //have to use REST query instead of typed ApexCodeCoverageAggregate because wsc v34.0 has a bug and
        // can not deserialise coverage field value
        for (jsRecord <- SoqlQuery.getQueryIteratorTooling(session, query)) {
            val record = new ResultRecord(jsRecord)

            val res = new com.sforce.soap.tooling.CodeCoverageResult

            val coverage = record.getFieldAsObject("Coverage").get
            coverage.getFieldAsArray("uncoveredLines") match {
              case Some(x) =>
                  val locations = x.elements.map{l =>
                      val loc = new CodeLocation()
                      loc.setLine(l.asInstanceOf[JsNumber].value.toInt)
                      loc}.toArray
                  res.setLocationsNotCovered(locations)
              case None =>
            }
            val numLinesCovered = record.getFieldAsNumber("NumLinesCovered").getOrElse(BigDecimal(0)).intValue()
            val numLinesUncovered = record.getFieldAsNumber("NumLinesUncovered").getOrElse(BigDecimal(0)).intValue()
            res.setNumLocations(numLinesCovered + numLinesUncovered)
            res.setNumLocationsNotCovered(numLinesUncovered)

            res.setName(record.getFieldAsString("ApexClassorTrigger.Name").getOrElse(""))
            record.getFieldAsString("ApexClassorTriggerId") match {
              case Some(classId) => res.setId(classId)
              case None =>
            }

            coverageResultBuilder += res
        }
        coverageResultBuilder.result()
    }

    private val ONE_SECOND = 1000
    private def waitAsyncJob(asyncJobId: String, progressReporter: (String, Set[String]) => Set[String]): Option[AsyncApexJob] = {
        val startTimeMills = System.currentTimeMillis
        var lastReportTime = System.currentTimeMillis
        val waitTimeMilliSecs = config.getProperty("pollWaitMillis").getOrElse("" + (ONE_SECOND * 5)).toLong
        val FINAL_STATUSES = Set("Aborted", "Completed", "Failed")
        val query = s"SELECT ApexClassId,ExtendedStatus,Id,ParentJobId,Status FROM AsyncApexJob where Id = '$asyncJobId'"
        val doneClassIds = new collection.mutable.HashSet[String]()
        while (true) {
            val queryResult = session.queryTooling(query)
            if (queryResult.getSize < 1) {
                return None
            }
            val record = queryResult.getRecords.head.asInstanceOf[AsyncApexJob]
            val isDone = FINAL_STATUSES.contains(record.getStatus)
            if (isDone) {
                return Some(record)
            }
            doneClassIds ++= progressReporter(asyncJobId, doneClassIds.toSet)
            val reportAttempt = (System.currentTimeMillis() - lastReportTime) > (ONE_SECOND * 3)
            if (reportAttempt) {
                val timeElapsed = (System.currentTimeMillis - startTimeMills) / 1000
                logger.info(s"Time elapsed: $timeElapsed sec")
                lastReportTime = System.currentTimeMillis
            }
            Thread.sleep(waitTimeMilliSecs)
        }
        None

    }
    /**
     *
     * @param asyncJobId - Id of AsyncApexJob
     * @param ignoreClassIds - Id of classes for which we do not need to report status
     * @return set of Done class Ids
     */
    private def reportTestProgress(asyncJobId: String, ignoreClassIds: Set[String]): Set[String] = {
        val FINAL_STATUSES = Set("Aborted", "Completed", "Failed")
        val query =
            s"""SELECT ApexClassId, ApexClass.Name, ExtendedStatus, Id, ParentJobId, Status
               |FROM ApexTestQueueItem
               |where ParentJobId = '$asyncJobId'
             """.stripMargin
        val queryFilter = if (ignoreClassIds.nonEmpty) " and ApexClassId not in ('" + ignoreClassIds.mkString("','") + "')" else ""
        val doneClassIds = Set.newBuilder[String]
        val queryResult = session.queryTooling(query + queryFilter)

        for (record <- queryResult.getRecords) {
            val queueItem = record.asInstanceOf[ApexTestQueueItem]
            val classId = queueItem.getApexClassId
            val className = queueItem.getApexClass.getName
            val extendedStatus = queueItem.getExtendedStatus
            val status = queueItem.getStatus

            if (null != extendedStatus) {
                logger.info(s"$className => $extendedStatus")
            } else {
                logger.info(s"$className => $status")
            }
            if (FINAL_STATUSES.contains(status)) {
                doneClassIds += classId
            }
        }
        doneClassIds.result()

    }

    private def getTestClassNames: List[String] = {
        ApexTestUtils.getTestMethodsByClassName(config.getProperty("testsToRun")).keys.toList
    }

    private def getTestSuiteNames: List[String] = {
        config.getProperty("testSuitesToRun") match {
            case Some(value) if value.nonEmpty => value.split(",").toList
            case None => List.empty
        }
    }

    /**
     *
     * @param classIdByName map of Class Ids by Class name
     * @return JSON for runTestsAsynchronous
      *         {"tests": [{
     *              "classId" : "01pi0000004xPzu",
     *              "testMethods" : ["testMethod1","testMethod2","testMethod3"] },{
     *              "classId" : "01pi0000004xPzv",
     *              "testMethods" : ["testMethod1","testMethod2"]
     *              }]
     *          }
     *        OR
     *        {"tests":[{"classId":"01pi0000004xPzuAAE"}]}
     */
    private def getTestClassMethodJson(classIdByName: Map[String, String]): Map[String, JsValue] = {

        val methodsByClassNameMap = ApexTestUtils.getTestMethodsByClassName(config.getProperty("testsToRun"))

        val methodsByClass = classIdByName.map {
            case (className, classId) =>
                val _methodsByClass =
                    methodsByClassNameMap.get(className) match {
                        case Some(methodSet) if methodSet.nonEmpty =>
                            Map("classId" -> classId.toJson, "testMethods" -> methodSet.toList.toJson)
                        case _ => Map("classId" -> classId.toJson)
                    }
                _methodsByClass
        }
        if (methodsByClass.nonEmpty) {
            Map("tests" -> methodsByClass.toJson)
        } else {
            Map.empty
        }
    }


}
