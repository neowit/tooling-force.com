package com.neowit.apex.actions.tooling

import com.neowit.apex._
import com.neowit.apex.actions.SoqlQuery.ResultRecord
import com.neowit.apex.actions.{SoqlQuery, ActionHelp, DeployModified}
import com.neowit.utils.{FileUtils, ResponseWriter}
import com.neowit.utils.ResponseWriter.Message
import com.sforce.soap.tooling.{RunTestFailure, ApexTestQueueItem, AsyncApexJob}
import spray.json._
import spray.json.DefaultJsonProtocol._

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
 *      e.g. "ControllerTest.test1, ControllerTest.test2, HandlerTest1, Test3"
 *
 *      if --testsToRun=* (star) then run all tests in all Local classes (excluding installed packages)
 * --reportCoverage=true|false (defaults to false) - if true then generate code coverage file
 * --async=true|false (defaults to false) - if true then use runTestsAsynchronous
 */
class RunTests extends DeployModified{

    override def getHelp: ActionHelp = new ActionHelp {
        override def getExample: String = ""

        override def getParamDescription(paramName: String): String = paramName match {
            case "reportCoverage" =>
                """--reportCoverage=true|false (defaults to false) - if true then generate code coverage file
                """.stripMargin
            case
                "testsToRun" =>
                """--testsToRun=* OR "comma separated list of class names",
                  |       e.g. "ControllerTest, ControllerTest, HandlerTest1, Test3"
                  |
                  |       if --testsToRun=* (star) then run all tests in all Local classes (excluding installed packages)
                """.stripMargin
            case "async" =>
                """--async=true|false (defaults to false) - if true then use runTestsAsynchronous.
                   |      Running tests asynchronously allows methods runTestsAsynchronous() to process in parallel, cutting down your test run times.
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
        //val traceId = ToolingUtils.setupTrace(session, logger)

        /*
        val runTestsRequest = new com.sforce.soap.tooling.RunTestsRequest()

        getTestClassNames match {
            case List("*") =>
                runTestsRequest.setAllTests(true)
            case head :: tail =>
                runTestsRequest.setClasses((head :: tail).toArray[String])
        }
        logger.debug("Run tests")
        val runTestsResult = session.runTestsTooling(runTestsRequest)
        */

        val runTestsResult = runTests()
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

    private def runTests(): com.sforce.soap.tooling.RunTestsResult = {
        //TODO uncomment
        //val isAsync = config.getProperty("async").getOrElse("false").toBoolean
        val isAsync = true
        if (isAsync) {
            runTestsAsynchronous()
        } else {
           runTestsSynchronous()
        }

    }
    private def runTestsSynchronous(): com.sforce.soap.tooling.RunTestsResult = {
        val runTestsRequest = new com.sforce.soap.tooling.RunTestsRequest()

        getTestClassNames match {
            case List("*") =>
                runTestsRequest.setAllTests(true)
            case head :: tail =>
                runTestsRequest.setClasses((head :: tail).toArray[String])
        }
        logger.info("Run tests Synchronous")
        val runTestsResult = session.runTestsTooling(runTestsRequest)
        runTestsResult
    }

    /**
     * ID: 707g000000MRjHdAAL
     * SELECT ApexClassId,CompletedDate,CreatedById,CreatedDate,ExtendedStatus,Id,JobItemsProcessed,JobType,
     * LastProcessed,LastProcessedOffset,MethodName,NumberOfErrors,ParentJobId,Status,TotalJobItems FROM AsyncApexJob
     * @return
     */
    private def runTestsAsynchronous(): com.sforce.soap.tooling.RunTestsResult = {
        logger.info("Run tests Asynchronous")
        val records = getTestClassNames match {
            case List("*") =>
                SoqlQuery.getQueryIteratorTooling(session, "select Id, Name from ApexClass where Status = 'Active' and NamespacePrefix = ''").map(new ResultRecord(_))
            case head :: tail =>
                val classNames = (head :: tail).mkString("','")
                SoqlQuery.getQueryIteratorTooling(session, s"select Id, Name from ApexClass where Name in ('$classNames')").map(new ResultRecord(_))
        }
        val classIds = records.map(_.getFieldAsString("Id").get)
        //val asyncJobId = session.runTestsAsyncTooling(classIds.toList)
        val methodsToClassJson = getTestClassMethodJson(records.map(r => r.getFieldAsString("Name").get -> r.getFieldAsString("Id").get).toMap)
        session.postRestContentTooling("/runTestsAsynchronous/", methodsToClassJson) match {
          case Some(jsonAst) =>
              val asyncJobId = jsonAst.asInstanceOf[JsString].value
              waitAsyncJob(asyncJobId, reportTestProgress) match {
                  case Some(asyncApexJob) if "Completed" == asyncApexJob.getStatus=>
                      generateRunTestResult(asyncApexJob)

                  case Some(asyncApexJob) =>
                      //TODO - job is aborted or failed
                      new com.sforce.soap.tooling.RunTestsResult()
                  case None =>
                      new com.sforce.soap.tooling.RunTestsResult()
              }

          case None =>
              new com.sforce.soap.tooling.RunTestsResult()
        }
        //val asyncJobId = session.postRestContentTooling("/runTestsAsynchronous/", Map("classids" -> classIds.mkString(","))).get
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

        val failures = Array.newBuilder[com.sforce.soap.tooling.RunTestFailure]
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
                case "Fail" =>
                    val failure = new RunTestFailure()
                    failure.setMethodName(methodName)
                    failure.setMessage(record.getFieldAsString("Message").get)
                    failures += failure
                case "CompileFail" =>
                    val failure = new RunTestFailure()
                    failure.setMethodName(methodName)
                    failure.setMessage(record.getFieldAsString("Message").get)
                    failures += failure
                case x => logger.debug("outcome: " + x)
            }

        }
        runTestResult.setFailures(failures.result())
        runTestResult.setNumFailures(runTestResult.getFailures.length)
        runTestResult.setSuccesses(successes.result())
        runTestResult.setNumTestsRun(runTestResult.getNumFailures + runTestResult.getSuccesses.length)
        runTestResult.setCodeCoverage(getCoverageResult)
        //runTestResult.set
        runTestResult
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
        val waitTimeMilliSecs = config.getProperty("pollWaitMillis").getOrElse("" + (ONE_SECOND * 5)).toInt
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
    private def getTestClassMethodJson(classIdByName: Map[String, String]): String = {

        val methodsByClassMap = ApexTestUtils.getTestMethodsByClassName(config.getProperty("testsToRun"))

        val methodsByClass = methodsByClassMap.map{
            case (name, methods) =>
                classIdByName.get(name) match {
                  case Some(classId) =>
                      val _methodsByClass = if (methods.isEmpty) Map("classId" -> classId.toJson) else Map("classId" -> classId.toJson, "testMethods" -> methods.toList.toJson)
                      _methodsByClass
                  case None => Map[String, JsValue]()
                }
        }.filterNot(_.isEmpty)
        val jsonStr = Map("tests" -> methodsByClass).toJson.compactPrint
        jsonStr
    }


}
