package com.neowit.apex.actions

import com.neowit.apex.{StubFileGenerator, MetadataType, MetaXml}
import com.neowit.utils.ResponseWriter.{MessageDetail, Message}
import com.neowit.utils.{FileUtils, ZipUtils, ResponseWriter}
import java.io.{PrintWriter, FileWriter, File}
import com.sforce.soap.metadata.{DescribeMetadataObject, DeployProblemType, DeployOptions}
import scala.collection.mutable
import scala.util.matching.Regex
import scala.util.parsing.json.{JSONArray, JSONObject}

abstract class Deploy extends ApexAction {

    //* ... line 155, column 41 ....
    private val LineColumnRegex = """.*line (\d+), column (\d+).*""".r
    //Class.Test1: line 19, column 1
    val TypeFileLineColumnRegex = """.*(Class|Trigger)\.(\w*).*line (\d+), column (\d+).*""".r
    //Class.Test1.prepareData: line 13, column 1
    val TypeFileMethodLineColumnRegex = """.*(Class|Trigger)\.(\w*)\.(\w*).*line (\d+), column (\d+).*""".r

    /**
     * try to parse line and column number from error message looking like so
     * ... line 155, column 41: ....
     * @param errorMessage - message returned by deploy operation
     * @return
     */
    protected def parseLineColumn(errorMessage: String): Option[(Int, Int)] = {

        val pair = try {
            val LineColumnRegex(line, column) = errorMessage
            Some((line.toInt, column.toInt))
        } catch {
            case _:Throwable => None
        }

        pair
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
                        parseLineColumn(traceLine)  match {
                          case Some((_line, _column)) => Some(("", "", "", _line, _column))
                          case None => None
                        }
                    case _:Throwable => None
                }
            case _:Throwable => None
        }
    }

    /**
     * depending on the target of deployment and flags like "checkOnly" & "updateSessionDataOnSuccess" we may or may not
     * need to update session data after successful deployment
     @return
     */
    def isUpdateSessionDataOnSuccess: Boolean = {
        val callingAnotherOrg = session.callingAnotherOrg
        val updateSessionDataOnSuccess = !config.isCheckOnly && !callingAnotherOrg || config.getProperty("updateSessionDataOnSuccess").getOrElse("false").toBoolean
        updateSessionDataOnSuccess
    }
}


/**
 * 'deployModified' action grabs all modified files and sends deploy() File-Based call
 * Extra command line params:
 * --ignoreConflicts=true|false (defaults to false) - if true then skip ListConflicting check
 * --checkOnly=true|false (defaults to false) - if true then do a dry-run without modifying SFDC
 * --testsToRun=* OR "comma separated list of class.method names",
 *      e.g. "ControllerTest.myTest1, ControllerTest.myTest2, HandlerTest1.someTest, Test3.anotherTest1"
 *
 *      class/method can be specified in two forms
 *      - ClassName[.methodName] -  means specific method of specific class
 *      - ClassName -  means *all* test methodsToKeep of specific class
 *
 *      if --testsToRun=* (star) then run all tests in all classes (containing testMethod or @isTest ) in
 *                              the *current* deployment package
 * --reportCoverage=true|false (defaults to false) - if true then generate code coverage file
 *
 */
class DeployModified extends Deploy {
    override def getHelp: ActionHelp = new ActionHelp {
        override def getExample: String = ""

        override def getParamDescription(paramName: String): String = paramName match {
            case "ignoreConflicts" => "--ignoreConflicts=true|false (defaults to false) - if true then skip ListConflicting check"
            case "checkOnly" => "--checkOnly=true|false (defaults to false) - if true then test deployment but do not make actual changes in the Org"
            case "reportCoverage" =>
                """--reportCoverage=true|false (defaults to false) - if true then generate code coverage file
                  |Note: makes sence only when --testsToRun is also specified
                """.stripMargin
            case
                "testsToRun" =>
                """--testsToRun=* OR "comma separated list of class.method names",
                  |       e.g. "ControllerTest.myTest1, ControllerTest.myTest2, HandlerTest1.someTest, Test3.anotherTest1"
                  |
                  |       class/method can be specified in two forms
                  |       - ClassName[.methodName] -  means specific method of specific class
                  |       - ClassName -  means *all* test methodsToKeep of specific class
                  |
                  |       if --testsToRun=* (star) then run all tests in all classes (containing testMethod or @isTest ) in
                  |                               the *current* deployment package
                """.stripMargin
        }

        override def getParamNames: List[String] = List("ignoreConflicts", "checkOnly", "testsToRun", "reportCoverage")

        override def getSummary: String = "Deploy modified files and (if requested) run tests"

        override def getName: String = "deployModified"
    }

    def act() {
        val hasTestsToRun = None != config.getProperty("testsToRun")
        val modifiedFiles = new ListModified().load[ListModified](session.basicConfig).getModifiedFiles
        val filesWithoutPackageXml = modifiedFiles.filterNot(_.getName == "package.xml").toList
        if (!hasTestsToRun && filesWithoutPackageXml.isEmpty) {
            responseWriter.println("RESULT=SUCCESS")
            responseWriter.println("FILE_COUNT=" + modifiedFiles.size)
            responseWriter.println(new Message(ResponseWriter.INFO, "no modified files detected."))
        } else {
            //first check if SFDC has newer version of files we are about to deploy
            val ignoreConflicts = config.getProperty("ignoreConflicts").getOrElse("false").toBoolean

            val canDeploy = ignoreConflicts || !hasConflicts(modifiedFiles) || hasTestsToRun

            if (canDeploy) {
                deploy(modifiedFiles, isUpdateSessionDataOnSuccess)
            }
        }
    }

    /**
     * @return - true if deployment is successful
     */
    def deploy(files: List[File], updateSessionDataOnSuccess: Boolean): Boolean = {
        deploy(files, updateSessionDataOnSuccess, None)
    }
    def deploy(files: List[File], updateSessionDataOnSuccess: Boolean, alternativeSrcDir: Option[File] = None): Boolean = {

        var success = false //assume failure by default

        //for every modified file add its -meta.xml if exists
        val metaXmlFiles = for (file <- files;
                                metaXml = new File(file.getAbsolutePath + "-meta.xml")
                                if metaXml.exists()) yield metaXml
        //for every -meta.xml file make sure that its source file is also included
        val extraSourceFiles = for (file <- files.filter(_.getName.endsWith("-meta.xml"));
                                    sourceFile = new File(file.getAbsolutePath.replace("-meta.xml", ""))
                                    if sourceFile.exists()) yield sourceFile


        var allFilesToDeploySet = (files ++ metaXmlFiles ++ extraSourceFiles).toSet

        val packageXml = new MetaXml(config)
        val packageXmlFile = packageXml.getPackageXml
        if (!allFilesToDeploySet.contains(packageXmlFile)) {
            //deployment always must contain packageXml file
            allFilesToDeploySet += packageXmlFile
        }
        /*
        //for debug purpose only, to check what is put in the archive
        //get temp file name
        val destZip = FileUtils.createTempFile("deploy", ".zip")
        destZip.delete()

        ZipUtils.zipDir(session.getConfig.srcPath, destZip.getAbsolutePath, excludeFileFromZip(allFilesToDeploySet, _))
        */
        val checkOnly = config.isCheckOnly
        val testMethodsByClassName: Map[String, Set[String]] = getTestMethodsByClassName(allFilesToDeploySet)
        val isRunningTests = testMethodsByClassName.nonEmpty

        //make sure that test class is part of deployment if user set specific methods to run
        if (checkOnly && isRunningTests ) {
            for (className <- testMethodsByClassName.keySet) {
                testMethodsByClassName.get(className) match {
                    case Some(x) if x.nonEmpty =>
                        session.findFile(className, "ApexClass") match {
                            case Some(f) =>
                                allFilesToDeploySet += f
                                allFilesToDeploySet += new File(f.getAbsolutePath + "-meta.xml")
                            case None =>
                        }
                    case _ =>
                }
            }
        }

        val deployOptions = new DeployOptions()
        deployOptions.setPerformRetrieve(false)
        deployOptions.setAllowMissingFiles(true)
        deployOptions.setRollbackOnError(true)
        deployOptions.setRunTests(testMethodsByClassName.keys.toArray)

        deployOptions.setCheckOnly(checkOnly)

        logger.info("Deploying...")
        val srcDir = alternativeSrcDir.getOrElse(session.getConfig.srcDir)
        val (deployResult, log) = session.deploy(ZipUtils.zipDirToBytes(srcDir, excludeFileFromZip(allFilesToDeploySet, _),
            disableNotNeededTests(_, testMethodsByClassName)), deployOptions)

        val deployDetails = deployResult.getDetails
        if (!deployResult.isSuccess) {
            responseWriter.println("RESULT=FAILURE")
            //dump details of failures into a response file
            writeDeploymentFailureReport(deployDetails, isRunningTests)
        } else { //deployResult.isSuccess = true

            val runTestResult = deployDetails.getRunTestResult
            if (isRunningTests && (null == runTestResult || runTestResult.getFailures.isEmpty)) {
                responseWriter.println(new Message(ResponseWriter.INFO, "Tests PASSED"))
            }
            processCodeCoverage(runTestResult) match {
                case Some(coverageFile) =>
                    responseWriter.println("COVERAGE_FILE=" + coverageFile.getAbsolutePath)
                case _ =>
            }
            //update session data for successful files
            if (updateSessionDataOnSuccess) {
                updateSessionDataForSuccessfulFiles(deployResult)
            }
            session.storeSessionData()
            responseWriter.println("RESULT=SUCCESS")
            responseWriter.println("FILE_COUNT=" + files.size)
            if (!checkOnly) {
                responseWriter.startSection("DEPLOYED FILES")
                files.foreach(f => responseWriter.println(f.getName))
                responseWriter.endSection("DEPLOYED FILES")
            }
            success = true
        }
        if (!log.isEmpty) {
            val logFile = config.getLogFile
            FileUtils.writeFile(log, logFile)
            responseWriter.println("LOG_FILE=" + logFile.getAbsolutePath)
        }

        success
    }

    //update session data for successful files
    private def updateSessionDataForSuccessfulFiles (deployResult: com.sforce.soap.metadata.DeployResult): Unit = {
        val deployDetails = deployResult.getDetails

        val calculateMD5 = config.useMD5Hash
        val calculateCRC32 = !calculateMD5  //by default use only CRC32

        val describeByDir = DescribeMetadata.getDescribeByDirNameMap(session)
        for ( successMessage <- deployDetails.getComponentSuccesses) {
            val relativePath = successMessage.getFileName
            val key = session.getKeyByRelativeFilePath(relativePath)
            val f = new File(config.projectDir, relativePath)
            if (f.exists()) {
                val xmlType = describeByDir.get(f.getParentFile.getName) match {
                    case Some(describeMetadataObject) => describeMetadataObject.getXmlName
                    case None => "" //package.xml and -meta.xml do not have xmlType
                }
                val localMills = f.lastModified()

                val md5Hash = if (calculateMD5) FileUtils.getMD5Hash(f) else ""
                val crc32Hash = if (calculateCRC32) FileUtils.getCRC32Hash(f) else -1L

                val fMeta = new File(f.getAbsolutePath + "-meta.xml")
                val (metaLocalMills: Long, metaMD5Hash: String, metaCRC32Hash: Long) = if (fMeta.canRead) {
                    (fMeta.lastModified(),
                        if (calculateMD5) FileUtils.getMD5Hash(fMeta) else "",
                        if (calculateCRC32) FileUtils.getCRC32Hash(fMeta) else -1L)
                } else {
                    (-1L, "", -1L)
                }

                val newData = MetadataType.getValueMap(deployResult, successMessage, xmlType, localMills, md5Hash, crc32Hash, metaLocalMills, metaMD5Hash, metaCRC32Hash)
                val oldData = session.getData(key)
                session.setData(key, oldData ++ newData)
            }
        }

    }

    private def writeDeploymentFailureReport(deployDetails: com.sforce.soap.metadata.DeployDetails, isRunningTests: Boolean): Unit = {
        /**
         * @return (line, column, relativeFilePath)
         */
        def getMessageData(problem: String, typeName: String, fileName: String,
                           metadataByXmlName: Map[String, DescribeMetadataObject]): (Int, Int, String) = {
            val suffix = typeName match {
                case "Class" => "cls"
                case "Trigger" => "trigger"
                case _ => ""
            }
            val (line, column) = parseLineColumn(problem) match {
                case Some((_line, _column)) => (_line, _column)
                case None => (-1, -1)
            }
            val filePath =  if (!suffix.isEmpty) session.getRelativeFilePath(fileName, suffix) match {
                case Some(_filePath) => _filePath
                case _ => ""
            }
            else ""

            (line, column, filePath)

        }

        if (null != deployDetails) {
            responseWriter.startSection("ERROR LIST")

            //display errors both as messages and as ERROR: lines
            val componentFailureMessage = new Message(ResponseWriter.WARN, "Component failures")
            if (deployDetails.getComponentFailures.nonEmpty) {
                responseWriter.println(componentFailureMessage)
            }
            for ( failureMessage <- deployDetails.getComponentFailures) {
                val line = failureMessage.getLineNumber
                val column = failureMessage.getColumnNumber
                val filePath = failureMessage.getFileName
                val problem = failureMessage.getProblem
                val problemType = failureMessage.getProblemType match {
                    case DeployProblemType.Warning => ResponseWriter.WARN
                    case DeployProblemType.Error => ResponseWriter.ERROR
                    case _ => ResponseWriter.ERROR
                }
                responseWriter.println("ERROR", Map("type" -> problemType, "line" -> line, "column" -> column, "filePath" -> filePath, "text" -> problem))
                responseWriter.println(new MessageDetail(componentFailureMessage, Map("type" -> problemType, "filePath" -> filePath, "text" -> problem)))
            }
            //process test successes and failures
            val runTestResult = deployDetails.getRunTestResult
            val metadataByXmlName = DescribeMetadata.getMap(session)


            val testFailureMessage = new Message(ResponseWriter.ERROR, "Test failures")
            if (null != runTestResult && runTestResult.getFailures.nonEmpty) {
                responseWriter.println(testFailureMessage)
            }
            if (isRunningTests && (null == runTestResult || runTestResult.getFailures.isEmpty)) {
                responseWriter.println(new Message(ResponseWriter.INFO, "Tests PASSED"))
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
                                val (_line, _column, filePath) = getMessageData(problem, typeName, fileName, metadataByXmlName)
                                val inMethod = if (methodName.isEmpty) "" else " in method " +methodName
                                val _problem = if (showProblem) problem else "...continuing stack trace" +inMethod+ ". Details see above"
                                responseWriter.println("ERROR", Map("line" -> line, "column" -> column, "filePath" -> filePath, "text" -> _problem))
                                if (showProblem) {
                                    responseWriter.println(new MessageDetail(testFailureMessage, Map("type" -> ResponseWriter.ERROR, "filePath" -> filePath, "text" -> problem)))
                                }
                            case None => //failed to parse anything meaningful, fall back to simple message
                                responseWriter.println("ERROR", Map("line" -> -1, "column" -> -1, "filePath" -> "", "text" -> problem))
                                if (showProblem) {
                                    responseWriter.println(new MessageDetail(testFailureMessage, Map("type" -> ResponseWriter.ERROR, "filePath" -> "", "text" -> problem)))
                                }
                        }
                        showProblem = false
                    }
                } else { //no stack trace, try to parse cine/column/filePath from error message
                val typeName = failureMessage.getType
                    val fileName = failureMessage.getName
                    val (line, column, filePath) = getMessageData(problem, typeName, fileName, metadataByXmlName)
                    responseWriter.println("ERROR", Map("line" -> line, "column" -> column, "filePath" -> filePath, "text" -> problem))
                    responseWriter.println(new MessageDetail(testFailureMessage, Map("type" -> ResponseWriter.ERROR, "filePath" -> filePath, "text" -> problem)))
                }

                //responseWriter.println("ERROR", Map("line" -> line, "column" -> column, "filePath" -> filePath, "text" -> problem))
            }
            responseWriter.endSection("ERROR LIST")

            val coverageReportFile = processCodeCoverage(runTestResult)
            coverageReportFile match {
                case Some(coverageFile) =>
                    responseWriter.println("COVERAGE_FILE=" + coverageFile.getAbsolutePath)
                case _ =>
            }
        }

    }

    /**
     * process code coverage results
     * @param runTestResult - deployDetails.getRunTestResult
     * @return set of file names (e.g. MyClass) for which we had coverage
     */
    private def processCodeCoverage(runTestResult: com.sforce.soap.metadata.RunTestsResult): Option[File] = {
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
        val coverageDetails = new Message(ResponseWriter.WARN, "Code coverage details")
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
            responseWriter.println(coverageDetails)
        }

        val reportedNames = mutable.Set[String]()
        for ( coverageResult <- runTestResult.getCodeCoverage) {
            reportedNames += coverageResult.getName
            val linesCovered = coverageResult.getNumLocations - coverageResult.getNumLocationsNotCovered
            val coveragePercent = if (coverageResult.getNumLocations > 0) linesCovered * 100 / coverageResult.getNumLocations else 0
            responseWriter.println(new MessageDetail(coverageDetails,
                Map("text" ->
                    (coverageResult.getName +
                        ": lines total " + coverageResult.getNumLocations +
                        "; lines not covered " + coverageResult.getNumLocationsNotCovered +
                        "; covered " + coveragePercent + "%"),
                    "type" -> (if (coveragePercent >= 75) ResponseWriter.INFO else ResponseWriter.WARN)
                )
            ))
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
                            val locations = mutable.MutableList[Int]()
                            for (codeLocation <- coverageResult.getLocationsNotCovered) {
                                locations += codeLocation.getLine
                            }
                            val coverageJSON = JSONObject(Map("path" -> relPath, "linesTotalNum" -> coverageResult.getNumLocations,
                                                    "linesNotCoveredNum" -> coverageResult.getNumLocationsNotCovered,
                                                    "linesNotCovered" -> JSONArray(locations.toList)))
                            // end result looks like so:
                            // {"path" : "src/classes/AccountController.cls", "linesNotCovered" : [1, 2, 3,  4, 15, 16,...]}
                            writer.println(coverageJSON.toString(ResponseWriter.defaultFormatter))
                        case None =>
                    }
                case None =>
            }
        }

        val coverageMessage = new Message(ResponseWriter.WARN, "Code coverage warnings")
        if (runTestResult.getCodeCoverageWarnings.nonEmpty) {
            responseWriter.println(coverageMessage)
        }
        for ( coverageWarning <- runTestResult.getCodeCoverageWarnings) {
            if (null != coverageWarning.getName) {
                if (!reportedNames.contains(coverageWarning.getName)) {
                    responseWriter.println(new MessageDetail(coverageMessage, Map("text" -> (coverageWarning.getName + ": " + coverageWarning.getMessage))))
                }
            } else {
                responseWriter.println(new MessageDetail(coverageMessage, Map("text" -> coverageWarning.getMessage)))
            }

        }

        coverageWriter match {
            case Some(writer) =>
                writer.close()
            case _ =>
        }
        coverageFile
    }

    private val alwaysIncludeNames = Set("src", "package.xml")

    private def excludeFileFromZip(modifiedFiles: Set[File], file: File) = {
        val exclude = !modifiedFiles.contains(file) && !alwaysIncludeNames.contains(file.getName)
        logger.trace(file.getName + " include=" + !exclude)
        exclude
    }

    def getFilesNewerOnRemote(files: List[File]): Option[List[Map[String, Any]]] = {
        val checker = new ListConflicting().load[ListConflicting](session.basicConfig)
        checker.getFilesNewerOnRemote(files)
    }

    protected def hasConflicts(files: List[File]): Boolean = {
        if (files.nonEmpty) {
            logger.info("Check Conflicts with Remote")
            val checker = new ListConflicting().load[ListConflicting](session.basicConfig)
            getFilesNewerOnRemote(files) match {
                case Some(conflictingFiles) =>
                    if (conflictingFiles.nonEmpty) {
                        responseWriter.println("RESULT=FAILURE")

                        val msg = new Message(ResponseWriter.WARN, "Outdated file(s) detected.")
                        responseWriter.println(msg)
                        checker.generateConflictMessageDetails(conflictingFiles, msg).
                            foreach{detail => responseWriter.println(detail)}
                        responseWriter.println(new Message(ResponseWriter.WARN, "Use 'refresh' before 'deploy'."))
                    }
                    conflictingFiles.nonEmpty
                case None => false
            }
        } else {
            logger.debug("File list is empty, nothing to check for Conflicts with Remote")
            false
        }
    }

    /**
     * --testsToRun="comma separated list of class.method names",
     *      e.g. "ControllerTest.myTest1, ControllerTest.myTest2, HandlerTest1.someTest, Test3.anotherTest1"
     *
     *      class/method can be specified in two forms
     *      - ClassName.<methodName> -  means specific method of specific class
     *      - ClassName -  means *all* test methodsToKeep of specific class
     *      Special case: *  - means "run all test in all classes belonging to current deployment package"
     * @return if user passed any classes to run tests via "testsToRun" the return them here
     */
    private def getTestMethodsByClassName(files: Set[File]): Map[String, Set[String]] = {
        var methodsByClassName = Map[String, Set[String]]()
        config.getProperty("testsToRun") match {
            case Some(x) if "*" == x =>
                //include all eligible classes
                val classFiles = files.filter(_.getName.endsWith(".cls"))
                for (classFile <- classFiles) {
                    methodsByClassName += FileUtils.removeExtension(classFile) -> Set[String]()
                }

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

    /**
     * if testsToRun contains names of specific methodsToKeep then we need to disable all other test methods in deployment package
     */
    private def disableNotNeededTests(classFile: File, testMethodsByClassName: Map[String, Set[String]]): File = {
        testMethodsByClassName.get(FileUtils.removeExtension(classFile)) match {
            case Some(methodsSet) if methodsSet.nonEmpty =>
                if (!config.isCheckOnly) {
                    throw new ActionError("Single method test is experimental and only supported in --checkOnly=true mode.")
                }
                disableNotNeededTests(classFile, methodsSet)
            case _ => classFile
        }

    }

    /**
     * this version is quite slow because it loads whole class code in memory before processing it.
     * when/if SFDC Tooling API provides native tools to run selected methodsToKeep, consider switching
     *
     * parse given file and comment out all test methodsToKeep which do not belong to provided methodsToKeep set
     * @param classFile - file to transform
     * @param methodsToKeep - test methodsToKeep to keep
     * @return new file with unnecessary test methodsToKeep removed
     */
    private def disableNotNeededTests(classFile: File, methodsToKeep: Set[String]): File = {
        //find testMethod or @isTest and then find first { after ( ... ) brackets
        //[^\{]+   [^\)]*  [^\{]* - used to make it non-greedy and allow no more than one (, ) and {
        val regexLeftCurly = """(?i)(\btestMethod|@\bisTest)\b[^\{]+\([^\)]*\)[^\{]*\{""".r

        //using names of all methods generate pattern to find them, something like this:
        //(\btest1|\btest3)\s*\(\s*\)\s*\{""".r
        val patternMethodNames = "(" + methodsToKeep.map("\\b" + _).mkString("|") + ")\\s*\\(\\s*\\)\\s*\\{"
        val regexMethodNames = new Regex(patternMethodNames)

        val source = scala.io.Source.fromFile(classFile)
        val textIn = source.getLines().mkString("\n")
        source.close()

        val textOut = new mutable.StringBuilder(textIn)

        var offset = 0
        for (currentMatch <- regexLeftCurly.findAllMatchIn(textIn)) {

            val curlyPos = currentMatch.end //current position of { after 'testMethod' keyword
            //check if this is one of methodsToKeep we need to disable
            //search only between 'testMethod' and '{'
            val methodDefinitionStr = textIn.substring(currentMatch.start, currentMatch.end)
            regexMethodNames.findFirstIn(methodDefinitionStr) match {
                case Some(x) => //yes, this string contains method we need to keep
                case None => //this string contains method we need to disable
                    textOut.insert(offset + curlyPos, "return; ")
                    offset += "return; ".length
            }
            logger.trace("========================================================")
            logger.trace(textOut.toString())
            logger.trace("=============== END ====================================")

        }

        val preparedFile = FileUtils.createTempFile(classFile.getName, ".cls")
        val writer = new FileWriter(preparedFile)
        try {
            writer.write(textOut.toString())
        } finally {
            writer.close()
        }
        preparedFile
    }
}

/**
 * 'deployAll' action grabs all project files and sends deploy() File-Based call
 * Extra command line params:
 * --updateSessionDataOnSuccess=true|false (defaults to false) - if true then update session data if deployment is successful
 */
class DeployAll extends DeployModified {
    private val superActionHelp = super.getHelp
    override def getHelp: ActionHelp = new AbstractActionHelp(superActionHelp) {

        override def getExample: String = ""

        override def getName: String = "deployAll"

        override def getSummary: String = "action grabs all project files and sends deploy() File-Based call"


        override def getParamNames: List[String] = "updateSessionDataOnSuccess" :: getParentActionHelp.getParamNames

        override def getParamDescription(paramName: String): String = paramName match {
            case "updateSessionDataOnSuccess" => "--updateSessionDataOnSuccess=true|false (defaults to false) - if true then update session data if deployment is successful"
            case _ => getParentActionHelp.getParamDescription(paramName)
        }

    }

    override def act() {
        val allFiles = getAllFiles

        deploy(allFiles, isUpdateSessionDataOnSuccess)
    }

    /**
     * list locally modified files using data from session.properties
     */
    def getAllFiles:List[File] = {
        val config = session.getConfig
        val allFiles  = FileUtils.listFiles(config.srcDir).filter(
            //remove all non apex files
            file => DescribeMetadata.isValidApexFile(session, file)
        ).toSet

        allFiles.toList
    }
}

/**
 * 'deployAllDestructive' action deploys all project files and removes all files listed in local package.xml
 * and existing on Remote but no longer available locally
 * Extra command line params:
 * --updateSessionDataOnSuccess=true|false (defaults to false) - if true then update session data if deployment is successful
 */
class DeployAllDestructive extends DeployAll {
    private val superActionHelp = super.getHelp
    override def getHelp: ActionHelp = new AbstractActionHelp(superActionHelp) {

        override def getExample: String = ""

        override def getName: String = "deployAllDestructive"

        override def getSummary: String = "action deploys all project files and removes all files listed in local package.xml and existing on Remote but no longer available locally"

        override def getParamNames: List[String] = "updateSessionDataOnSuccess" :: getParentActionHelp.getParamNames

        override def getParamDescription(paramName: String): String = paramName match {
            case "updateSessionDataOnSuccess" => "--updateSessionDataOnSuccess=true|false (defaults to false) - if true then update session data if deployment is successful"
            case _ => getParentActionHelp.getParamDescription(paramName)
        }

    }

    /**
     *   # DeployAllDestructive
     *   - get diff report using DiffWithRemote
     *   - create blank stubs for all files marked as “missing locally” in Diff Report
     *   ——— we can reuse files downloaded by DiffWithRemote
     *   - create temp project folder with all files reported by DeployAll + blanks
     *   - execute DeployAll against this temp folder
     *   - if previous step is successful then execute DeployDestructive using list of blank files
     */
    override def act() {

        val diffWithRemote = new DiffWithRemote {
            /**
             * path to folder where remote version of current project will be saved
             * @return - Option(/path/to/folder)
             */
            override def getTargetFolder: Option[String] = Some(FileUtils.createTempDir("remote_files").getAbsolutePath)
        }
        diffWithRemote.load[DiffWithRemote](session.basicConfig)

        diffWithRemote.getDiffReport match {
            case None =>
                responseWriter.println("RESULT=FAILURE")
                responseWriter.println(new Message(ResponseWriter.ERROR, "Failed to load remote version of current project"))

            case Some(diffReport) =>
                val dummyFilesBuilder = Map.newBuilder[String, File]
                val dummyMetaFilesBuilder = List.newBuilder[File]
                val dummyFilesDir = FileUtils.createTempDir("dummyFiles")
                val meta = new MetaXml(session.getConfig)
                for (file <- diffReport.getRemoteFilesMissingLocally.values) {
                    val relativePath = DescribeMetadata.getApexFolderNameByFile(session, file).getOrElse("") + "/" + file.getName
                    StubFileGenerator.generateStub(meta.getPackage.getVersion, dummyFilesDir, file, withMetaXml = true) match {
                        case (dummy, Some(metaXml)) =>
                            dummyFilesBuilder += relativePath -> dummy
                            dummyMetaFilesBuilder += metaXml
                        case (dummy, None) =>
                            dummyFilesBuilder += relativePath -> dummy
                    }
                }
                val allLocalFiles = getAllFiles
                val dummyFileByRelativePath = dummyFilesBuilder.result()
                val allFiles = allLocalFiles ++ dummyFileByRelativePath.values ++ dummyMetaFilesBuilder.result()
                //at this point we have two types of files
                //1 - files under the current project folder
                //2 - dummy/blank files somewhere in temp directory

                val (srcDir, filesToDeploy) = moveAllFilesUnderOneSrc(allFiles)
                val isDeploySuccessful = deploy(filesToDeploy, isUpdateSessionDataOnSuccess, Some(srcDir))
                if (isDeploySuccessful) {
                    //save response file
                    //execute DeployDestructive using list of blank files
                    deleteFiles(dummyFileByRelativePath.keys)
                    responseWriter.println(new Message(ResponseWriter.INFO, "REMOTE_VERSION_BACKUP_PATH=" + diffReport.remoteSrcFolderPath))
                }

        }
    }

    /**
     *
     * @param relativePathsToDelete
     *   list of relative file paths that need to be deleted from Remote
     *   note - these files do not have to belong to local project
     *   the only important thing is file name and extension
     *   e.g.
     *   classes/MyClass.cls
     *   Objects/MyObject.object
     *   classes/MyClass.cls-meta.xml
     *
     */
    private def deleteFiles(relativePathsToDelete: Iterable[String]): Unit = {
        if (relativePathsToDelete.nonEmpty) {
            //get temp file name
            val componentsToDeleteFile = FileUtils.createTempFile("COMPONENTS_TO_DELETE", ".txt")
            val pathsToDelete = relativePathsToDelete
                .filterNot(path => path.endsWith("-meta.xml") || path.endsWith("package.xml") )

            val writer = new PrintWriter(componentsToDeleteFile)
            pathsToDelete.foreach(writer.println(_))
            writer.close()


            //override DeployDestructive and add list of files to delete and tell it to update session if not in test mode
            val deployDestructiveAction = new DeployDestructive {
                override def getSpecificComponentsFilePath: String = componentsToDeleteFile.getAbsolutePath
                override def isUpdateSessionDataOnSuccess: Boolean = {
                    val updateSessionDataOnSuccess = !config.isCheckOnly && !session.callingAnotherOrg
                    updateSessionDataOnSuccess
                }
            }
            deployDestructiveAction.load[DeployDestructive](session.basicConfig)

            //make sure that Delete operation appends to response file, rather than overwrites it
            deployDestructiveAction.setResponseWriter(this.responseWriter)

            deployDestructiveAction.act()
            //get rid of temp file
            componentsToDeleteFile.delete()

        }

    }
    /**
     * using list of apex files which belong to different projects - merge them all under one temp project
     * @param filesInDifferentFolders files from several projects
     * @return - list of files under one (temp) project
     */
    private def moveAllFilesUnderOneSrc(filesInDifferentFolders: List[File]): (File, List[File]) = {
        val tempDir = FileUtils.createTempDir("real_and_dummies")
        val srcDir = new File(tempDir, "src")
        srcDir.mkdir()

        val createdDirs = collection.mutable.Map[String, File]()
        val filesUnderOneProject = List.newBuilder[File]

        for (file <- filesInDifferentFolders) {
            DescribeMetadata.getApexFolderNameByFile(session, file) match {
              case Some(dirName) if dirName.nonEmpty =>
                  val dir = createdDirs.get(dirName) match {
                    case Some(existingDir) => existingDir
                    case None =>
                        val dir = new File(srcDir, dirName)
                        dir.mkdir()
                        createdDirs += dirName -> dir
                        dir
                  }
                  val movedFile = new File(dir, file.getName)
                  FileUtils.copy(file, movedFile)
                  filesUnderOneProject += movedFile
              case Some(dirName) if dirName.isEmpty =>
                  //file belongs to src/ folder
                  val movedFile = new File(srcDir, file.getName)
                  FileUtils.copy(file, movedFile)
                  filesUnderOneProject += movedFile
              case _ =>
            }

        }
        (srcDir, filesUnderOneProject.result())
    }

}

/**
 * 'deploySpecificFiles' action uses file list specified in a file and sends deploy() File-Based call
 * Extra command line params:
 * --specificFiles=/path/to/file with file list
 * --updateSessionDataOnSuccess=true|false (defaults to false) - if true then update session data if deployment is successful
 */
class DeploySpecificFiles extends DeployModified {
    private val superActionHelp = super.getHelp
    override def getHelp: ActionHelp = new AbstractActionHelp(superActionHelp) {
        override def getExample: String =
            """
          |Suppose we want to deploy Account.trigger and AccountHandler.cls, content of file passed to --specificFiles
          |may look like so
          |-------------------------------
          |src/classes/AccountHandler.cls
          |src/triggers/Account.trigger
          |-------------------------------
          |
          |then in addition to all normal command line parameters you add
          |... --specificFiles=/tmp/file_list.txt
        """.stripMargin

        override def getName: String = "deploySpecificFiles"

        override def getSummary: String = "action uses file list specified in a file and sends deploy() File-Based call"

        override def getParamNames: List[String] = List("updateSessionDataOnSuccess", "specificFiles") ++ getParentActionHelp.getParamNames

        override def getParamDescription(paramName: String): String = paramName match {
            case "updateSessionDataOnSuccess" => "--updateSessionDataOnSuccess=true|false (defaults to false) - if true then update session data if deployment is successful"
            case "specificFiles" => "--specificFiles=/path/to/file with file list"
            case _ => getParentActionHelp.getParamDescription(paramName)
        }
    }
    override def act() {
        val files = getFiles
        if (files.isEmpty) {
            responseWriter.println("RESULT=FAILURE")
            val fileListFile = new File(config.getRequiredProperty("specificFiles").get)
            responseWriter.println(new Message(ResponseWriter.ERROR, "no valid files in " + fileListFile))
        } else {

            //first check if SFDC has newer version of files we are about to deploy
            val ignoreConflicts = config.getProperty("ignoreConflicts").getOrElse("false").toBoolean
            val callingAnotherOrg = session.callingAnotherOrg

            val canDeploy = callingAnotherOrg || ignoreConflicts || !hasConflicts(files)
            if (canDeploy) {
                deploy(files, isUpdateSessionDataOnSuccess)
            }
        }


    }

    /**
     * list locally modified files using data from session.properties
     */
    def getFiles:List[File] = {
        val config = session.getConfig

        //logger.debug("packageXmlData=" + packageXmlData)
        val projectDir = config.projectDir

        //load file list from specified file
        val fileListFile = new File(config.getRequiredProperty("specificFiles").get)
        val files:List[File] = scala.io.Source.fromFile(fileListFile).getLines().map(relativeFilePath => new File(projectDir, relativeFilePath)).toList

        //for each file check that it exists
        files.find(!_.canRead) match {
            case Some(f) =>
                throw new ActionError("Can not read file: " + f.getAbsolutePath)
            case None =>
        }

        val allFiles  = files.filter(
            //remove all non apex files
            file => DescribeMetadata.isValidApexFile(session, file)
        ).toSet

        allFiles.toList
    }
}

/**
 * list modified files (compared to their stored session data)
 */
class ListModified extends ApexAction {
    override def getHelp: ActionHelp = new ActionHelp {
        override def getExample: String = ""

        override def getParamDescription(paramName: String): String = ""

        override def getParamNames: List[String] = Nil

        override def getSummary: String = "list names of files that have changed since last refresh/deploy operation"

        override def getName: String = "listModified"
    }
    /**
     * list locally modified files using data from session.properties
     */
    def getModifiedFiles:List[File] = {
        val config = session.getConfig

        //logger.debug("packageXmlData=" + packageXmlData)
        //val allFiles  = (packageXmlFile :: FileUtils.listFiles(config.srcDir)).toSet
        val allFiles  = FileUtils.listFiles(config.srcDir).filter(
            //remove all non apex files
            file => DescribeMetadata.isValidApexFile(session, file)
        ).toSet

        val modifiedFiles = allFiles.filter(session.isModified(_))
        modifiedFiles.toList
    }

    def reportModifiedFiles(modifiedFiles: List[File], messageType: ResponseWriter.MessageType = ResponseWriter.INFO) {
        val msg = new Message(messageType, "Modified file(s) detected.", Map("code" -> "HAS_MODIFIED_FILES"))
        responseWriter.println(msg)
        for(f <- modifiedFiles) {
            responseWriter.println(new MessageDetail(msg, Map("filePath" -> f.getAbsolutePath, "text" -> session.getRelativePath(f))))
        }
        responseWriter.println("HAS_MODIFIED_FILES=true")
        responseWriter.startSection("MODIFIED FILE LIST")
        for(f <- modifiedFiles) {
            responseWriter.println("MODIFIED_FILE=" + session.getRelativePath(f))
        }
        responseWriter.endSection("MODIFIED FILE LIST")

    }
    
    def reportDeletedFiles(deletedFiles: List[File], messageType: ResponseWriter.MessageType = ResponseWriter.INFO) {
        val msg = new Message(messageType, "Deleted file(s) detected.", Map("code" -> "HAS_DELETED_FILES"))
        responseWriter.println(msg)
        for(f <- deletedFiles) {
            responseWriter.println(new MessageDetail(msg, Map("filePath" -> f.getAbsolutePath, "text" -> session.getRelativePath(f))))
        }
        responseWriter.println("HAS_DELETED_FILES=true")
        responseWriter.startSection("DELETED FILE LIST")
        for(f <- deletedFiles) {
            responseWriter.println("DELETED_FILE=" + session.getRelativePath(f))
        }
        responseWriter.endSection("DELETED FILE LIST")

    }

    def act() {
        val modifiedFiles = getModifiedFiles
        val deletedFiles = session.getDeletedLocalFilePaths.map(new File(config.projectDir, _))

        responseWriter.println("RESULT=SUCCESS")
        responseWriter.println("FILE_COUNT=" + modifiedFiles.size + deletedFiles.size)
        if (modifiedFiles.nonEmpty) {
            reportModifiedFiles(modifiedFiles)
        } else {
            responseWriter.println(new Message(ResponseWriter.INFO, "No Modified file(s) detected."))
        }

        if (deletedFiles.nonEmpty) {
            reportDeletedFiles(deletedFiles)
        } else {
            //responseWriter.println(new Message(ResponseWriter.INFO, "No Deleted file(s) detected."))
        }
    }

}

/**
 * 'deleteMetadata' action attempts to remove specified metadata components from SFDC
 * using deploy() call with delete manifest file named destructiveChanges.xml
 * Extra command line params:
 * --checkOnly=true|false (defaults to false) - if true then do a dry-run without modifying SFDC
 * --specificComponents=/path/to/file with metadata components list, each component on its own line
 * --updateSessionDataOnSuccess=true|false (defaults to false) - if true then update session data if delete is successful
 */
class DeployDestructive extends Deploy {
    override def getHelp: ActionHelp = new ActionHelp {
        override def getExample: String =
            """
          |Suppose we want to delete Account.trigger, AccountHandler.cls and MyCustomObject__c.MyCustomField__c,
          |content of file passed to --specificComponents may look like so
          |-------------------------------
          |classes/AccountHandler.cls
          |triggers/Account.trigger
          |objects/MyCustomObject__c.object
          |pages/Hello.page
          |-------------------------------
          |
          |then in addition to all normal command line parameters you add
          |... --specificComponents=/tmp/list.txt
        """.stripMargin

        override def getParamDescription( paramName: String): String = paramName match {
            case "checkOnly" =>
                "--checkOnly=true|false (defaults to false) - if true then test deployment but do not make actual changes in the Org"
            case
                "specificComponents" => "--specificComponents=/path/to/file with component names"
            case
                "updateSessionDataOnSuccess" => "--updateSessionDataOnSuccess=true|false (defaults to false) - if true then update session data if delete is successful"
        }

        override def getParamNames: List[String] = List("checkOnly", "specificComponents", "updateSessionDataOnSuccess")

        override def getSummary: String =
            s"""action attempts to remove specified metadata components from SFDC
              |Note: removal of individual members (e.g. custom field) is not supported by this command.
              |With $getName you can delete a Class or Custom Object, but not specific field of custom object.
            """.stripMargin

        override def getName: String = "deleteMetadata"
    }

    def getSpecificComponentsFilePath: String = config.getRequiredProperty("specificComponents").get

    override def act() {
        val components = getComponentPaths
        if (components.isEmpty) {
            responseWriter.println("RESULT=FAILURE")
            val componentListFile = new File(config.getRequiredProperty("specificComponents").get)
            responseWriter.println(new Message(ResponseWriter.ERROR, "no valid components in " + componentListFile))
        } else {

            val deployOptions = new DeployOptions()
            deployOptions.setPerformRetrieve(false)
            deployOptions.setAllowMissingFiles(false)
            deployOptions.setRollbackOnError(true)
            val checkOnly = config.isCheckOnly
            deployOptions.setCheckOnly(checkOnly)

            val tempDir = generateDeploymentDir(components)
            logger.info("Deleting...")
            val (deployResult, log) = session.deploy(ZipUtils.zipDirToBytes(tempDir), deployOptions)

            val deployDetails = deployResult.getDetails
            if (!deployResult.isSuccess) {
                responseWriter.println("RESULT=FAILURE")
                if (null != deployDetails) {
                    responseWriter.startSection("ERROR LIST")

                    //display errors both as messages and as ERROR: lines
                    val componentFailureMessage = new Message(ResponseWriter.WARN, "Component failures")
                    if (deployDetails.getComponentFailures.nonEmpty) {
                        responseWriter.println(componentFailureMessage)
                    }

                    for ( failureMessage <- deployDetails.getComponentFailures) {
                        //first part of the file usually looks like deleteMetadata/classes/AccountController.cls
                        //make it src/classes/AccountController.cls
                        val filePath = failureMessage.getFileName match {
                            case null => ""
                            case "" => ""
                            case str =>
                                val pathSplit = str.split('/')
                                if (3 == pathSplit.size) {
                                    //this is a standard 3 component path, make first part src/
                                    "src" + str.dropWhile(_ != '/')
                                } else {
                                    str
                                }
                        }
                        val problemWithFilePath = if (filePath.isEmpty) failureMessage.getProblem else filePath + ": " + failureMessage.getProblem
                        val problemType = failureMessage.getProblemType match {
                            case DeployProblemType.Warning => ResponseWriter.WARN
                            case DeployProblemType.Error => ResponseWriter.ERROR
                            case _ => ResponseWriter.ERROR
                        }
                        responseWriter.println("ERROR", Map("type" -> problemType, "text" -> failureMessage.getProblem, "filePath" -> filePath))
                        responseWriter.println(new MessageDetail(componentFailureMessage, Map("type" -> problemType, "text" -> problemWithFilePath, "filePath" -> filePath)))
                    }
                    responseWriter.endSection("ERROR LIST")
                }
            } else {
                responseWriter.println("RESULT=SUCCESS")
                if (isUpdateSessionDataOnSuccess) {
                    responseWriter.debug("Updating session data")
                    for (componentPath <- components) {
                        val pair = componentPath.split('/')
                        session.findKey(pair(0), pair(1)) match {
                            case Some(key) =>
                                session.removeData(key)
                                responseWriter.debug("Removed session data for key: " + key)
                            case None =>
                        }
                    }
                    session.storeSessionData()
                }
            }
        }

    }
    def generateDeploymentDir(componentPaths: List[String]): File = {
        val tempDir = FileUtils.createTempDir(config)
        val metaXml = new MetaXml(config)
        //place destructiveChanges.xml and package.xml in this temp folder
        val destructivePackage = getDestructiveChangesPackage(componentPaths)
        val destructiveXml = metaXml.packageToXml(destructivePackage)
        scala.xml.XML.save(new File(tempDir, "destructiveChanges.xml").getAbsolutePath, destructiveXml, enc = "UTF-8", xmlDecl = true )

        val packageXml = metaXml.packageToXml(getEmptyPackage)
        scala.xml.XML.save(new File(tempDir, "package.xml").getAbsolutePath, packageXml, enc = "UTF-8" )
        tempDir
    }
    /**
     * list locally modified files using data from session.properties
     */
    def getComponentPaths: List[String] = {

        //load file list from specified file
        val componentsPath = getSpecificComponentsFilePath
        val componentListFile = new File(componentsPath)
        val components:List[String] = scala.io.Source.fromFile(componentListFile).getLines().filter(!_.trim.isEmpty).toList
        components
    }

    def getDestructiveChangesPackage(componentsPaths: List[String]): com.sforce.soap.metadata.Package = {
        val objectDescribeByXmlTypeName = DescribeMetadata.getMap(session)
        val xmlTypeNameByDirName = objectDescribeByXmlTypeName.filter(pair => !pair._2.getDirectoryName.isEmpty).map(
            pair => pair._2.getDirectoryName -> pair._1
        )
        val _package = new com.sforce.soap.metadata.Package()
        _package.setVersion(config.apiVersion.toString)

        val namesByDir = componentsPaths.groupBy(_.takeWhile(_ != '/'))

        val members = for (dirName <- namesByDir.keySet) yield {
            val ptm = new com.sforce.soap.metadata.PackageTypeMembers()
            xmlTypeNameByDirName.get(dirName) match {
              case Some(xmlTypeName) =>
                  ptm.setName(xmlTypeName)
                  var extension = objectDescribeByXmlTypeName(xmlTypeName).getSuffix
                  if (null == extension) {
                      extension = ""
                  } else {
                      extension = "." + extension
                  }

                  val objNames = namesByDir.getOrElse(dirName, Nil) match {
                      case _objNames if _objNames.nonEmpty && List("*") != _objNames=>
                          _objNames.map(_.drop(dirName.size + 1)).map(
                              name => if (name.endsWith(extension)) name.dropRight(extension.size) else name
                          ).toArray
                      case _ =>
                          throw new ActionError("Did not recognise directory: " + dirName)
                  }

                  ptm.setMembers(objNames)
              case None =>
            }
            ptm
        }
        _package.setTypes(members.toArray)
        _package

    }
    def getEmptyPackage: com.sforce.soap.metadata.Package = {
        val _package = new com.sforce.soap.metadata.Package()
        _package.setVersion(config.apiVersion.toString)
        _package
    }
}

/**
 * 'deployModifiedDestructive' action grabs all modified files and sends two File-Based calls
 * 1. deploy() - to deploy all modified files (which still exist locally)
 * 2. deleteMetadata() - to delete all files listed in session but no longer exist locally
 *
 * Extra command line params:
 * --ignoreConflicts=true|false (defaults to false) - if true then skip ListConflicting check
 * --checkOnly=true|false (defaults to false) - if true then do a dry-run without modifying SFDC
 * --testsToRun=* OR "comma separated list of class.method names",
 *      e.g. "ControllerTest.myTest1, ControllerTest.myTest2, HandlerTest1.someTest, Test3.anotherTest1"
 *
 *      class/method can be specified in two forms
 *      - ClassName[.methodName] -  means specific method of specific class
 *      - ClassName -  means *all* test methodsToKeep of specific class
 *
 *      if --testsToRun=* (star) then run all tests in all classes (containing testMethod or @isTest ) in
 *                              the *current* deployment package
 * --reportCoverage=true|false (defaults to false) - if true then generate code coverage file
 *
 */
class DeployModifiedDestructive extends DeployModified {

    override def getHelp: ActionHelp = new ActionHelp {
        override def getExample: String =
            """
            """.stripMargin

        override def getParamDescription( paramName: String): String = paramName match {
            case "checkOnly" =>
                "--checkOnly=true|false (defaults to false) - if true then test deployment but do not make actual changes in the Org"
            case
                "updateSessionDataOnSuccess" => "--updateSessionDataOnSuccess=true|false (defaults to false) - if true then update session data if delete is successful"
        }

        override def getParamNames: List[String] = List("checkOnly", "updateSessionDataOnSuccess")

        override def getSummary: String =
            s"""action is a combination of deployModified & deleteMetadata
              |First it tries to deploy existing modified files.
              |If deploy part is successful then it checks what files are present in the session properties but not present locally as files
              |Such file names are passed to deleteMetadata command
              |
              |Note: removal of individual members (e.g. custom field) is not supported by this command.
              |With $getName you can delete a Class or Custom Object, but not specific field of custom object.
            """.stripMargin

        override def getName: String = "deployModifiedDestructive"
    }

    override def act() {
        val hasTestsToRun = None != config.getProperty("testsToRun")
        val listModified = new ListModified().load[ListModified](session.basicConfig)
        val modifiedFiles = listModified.getModifiedFiles
        val filesWithoutPackageXml = modifiedFiles.filterNot(_.getName == "package.xml").toList
        if (!hasTestsToRun && filesWithoutPackageXml.isEmpty) {
            if (session.getDeletedLocalFilePaths.isEmpty) {
                responseWriter.println("RESULT=SUCCESS")
                responseWriter.println("FILE_COUNT=" + modifiedFiles.size)
                responseWriter.println(new Message(ResponseWriter.INFO, "no modified files detected."))
            } else {
                //process files that need to be deleted
                deleteFiles()
            }
        } else {
            //first check if SFDC has newer version of files we are about to deploy
            val ignoreConflicts = config.getProperty("ignoreConflicts").getOrElse("false").toBoolean
            //val hasConflicts = hasConflicts(deletedFiles)

            val canDeploy = ignoreConflicts || !hasConflicts(modifiedFiles) || hasTestsToRun

            if (canDeploy && deploy(modifiedFiles, isUpdateSessionDataOnSuccess)) {
                //process files that need to be deleted
                deleteFiles()
            }
        }
    }
    private def deleteFiles(): Unit = {
        val deletedLocalFilePaths = session.getDeletedLocalFilePaths.map(_.substring("src/".length))
        if (deletedLocalFilePaths.nonEmpty) {
            //get temp file name
            val componentsToDeleteFile = FileUtils.createTempFile("COMPONENTS_TO_DELETE", ".txt")

            val writer = new PrintWriter(componentsToDeleteFile)
            deletedLocalFilePaths.foreach(writer.println(_))
            writer.close()

            //override DeployDestructive and add list of files to delete and tell it to update session if not in test mode
            val deployDestructiveAction = new DeployDestructive {
                override def getSpecificComponentsFilePath: String = componentsToDeleteFile.getAbsolutePath
                override def isUpdateSessionDataOnSuccess: Boolean = {
                    val updateSessionDataOnSuccess = !config.isCheckOnly && !session.callingAnotherOrg
                    updateSessionDataOnSuccess
                }
            }
            deployDestructiveAction.load[DeployDestructive](session.basicConfig)

            deployDestructiveAction.act()
            //get rid of temp file
            componentsToDeleteFile.delete()
        }

    }

}
