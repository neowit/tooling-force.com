package com.neowit.apex.actions

import com.neowit.apex.actions.Deploy.RunTestResultMetadata
import com.neowit.apex.actions.tooling.AuraMember
import com.neowit.apex._
import com.neowit.utils.{FileUtils, ZipUtils}
import java.io.{File, FileWriter, PrintWriter}

import com.neowit.response._
import com.sforce.soap.metadata._

import scala.collection.mutable
import scala.concurrent.{ExecutionContext, Future}
import scala.util.matching.Regex

object Deploy {

    class RunTestResultMetadata(sourceTestResult: com.sforce.soap.metadata.RunTestsResult) extends com.neowit.apex.RunTestsResult {
        override def getCodeCoverage: Array[com.neowit.apex.CodeCoverageResult] = sourceTestResult.getCodeCoverage.map(new CodeCoverageResultMetadata(_))

        override def getCodeCoverageWarnings: Array[com.neowit.apex.CodeCoverageWarning] = sourceTestResult.getCodeCoverageWarnings.map(new CodeCoverageWarningMetadata(_))

        override def getFailures: Array[com.neowit.apex.RunTestFailure] = sourceTestResult.getFailures.map(new RunTestFailure(_))
    }

    class CodeCoverageResultMetadata(sourceCoverageResult: com.sforce.soap.metadata.CodeCoverageResult) extends com.neowit.apex.CodeCoverageResult {
        override def getNumLocations: Int = sourceCoverageResult.getNumLocations

        override def getLocationsNotCovered: Array[com.neowit.apex.CodeLocation] = sourceCoverageResult.getLocationsNotCovered.map(new CodeLocationMetadata(_))

        override def getName: String = sourceCoverageResult.getName

        override def getNumLocationsNotCovered: Int = sourceCoverageResult.getNumLocationsNotCovered
    }

    class CodeLocationMetadata(sourceCodeLocation: com.sforce.soap.metadata.CodeLocation) extends com.neowit.apex.CodeLocation {
        override def getLine: Int = sourceCodeLocation.getLine
    }

    class CodeCoverageWarningMetadata(source: com.sforce.soap.metadata.CodeCoverageWarning) extends com.neowit.apex.CodeCoverageWarning {
        override def getName: String = source.getName

        override def getMessage: String = source.getMessage
    }

    class RunTestFailure(source: com.sforce.soap.metadata.RunTestFailure) extends com.neowit.apex.RunTestFailure {
        override def getId: String = source.getId

        override def getType: String = source.getType

        override def getMessage: String = source.getMessage

        override def getStackTrace: String = source.getStackTrace

        override def getName: String = source.getName

        override def getMethodName: String = source.getMethodName
    }

    //* ... line 155, column 41 ....
    private val LineColumnRegex = """.*line (\d+), column (\d+).*""".r

    /**
     * try to parse line and column number from error message looking like so
     * ... line 155, column 41: ....
     * @param ErrorMessage - message returned by deploy operation
     * @return
     */
    def parseLineColumn(ErrorMessage: String): Option[(Int, Int)] = {

        val pair = try {
            val LineColumnRegex(line, column) = ErrorMessage
            Some((line.toInt, column.toInt))
        } catch {
            case _:Throwable => None
        }

        pair
    }
    /**
     * @return (line, column, relativeFilePath)
     */
    def getMessageData(session: Session, problem: String, typeName: String, fileName: String,
                       metadataByXmlName: Map[String, DescribeMetadataObject]): (Int, Int, String) = {
        val suffix = typeName match {
            case "Class" => "cls"
            case "Trigger" => "trigger"
            case _ => ""
        }
        val (line, column) = Deploy.parseLineColumn(problem) match {
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

}

abstract class Deploy extends ApexActionWithWritableSession {

    /**
     * depending on the target of deployment and flags like "checkOnly" & "updateSessionDataOnSuccess" we may or may not
     * need to update session data after successful deployment
     @return
     */
    def isUpdateSessionDataOnSuccess: Boolean = {
        val callingAnotherOrg = session.callingAnotherOrg
        val updateSessionDataOnSuccess = !getSessionConfig.isCheckOnly && !callingAnotherOrg || config.getProperty("updateSessionDataOnSuccess").getOrElse("false").toBoolean
        updateSessionDataOnSuccess
    }

    /**
     * load deploy options from user defined configuration
     * as usual configuration params can be set in one of 3 ways:
     * 1. command line: ... --deployOptions.allowMissingFiles=true --deployOptions.autoUpdatePackage=false ...
     * 2. java command line: java -DdeployOptions.allowMissingFiles=true -DdeployOptions.autoUpdatePackage=false ...
     * 3. config.properties file:
     *
     *      # this is a config file with SFDC Org access details
     *      sf.username = ...
     *      sf.password = ...
     *      deployOptions.allowMissingFiles = true
     *      deployOptions.autoUpdatePackage = true
     *
     * @param deployOptions - instance of Metadata DeployOptions
     */
    protected def loadDeployOptionsFromConfig(deployOptions: DeployOptions): Unit = {
        config.getProperty("deployOptions.allowMissingFiles").foreach(opt => deployOptions.setAllowMissingFiles(opt.toBoolean))
        config.getProperty("deployOptions.autoUpdatePackage").foreach(opt => deployOptions.setAutoUpdatePackage(opt.toBoolean))
        config.getProperty("deployOptions.checkOnly").foreach(opt => deployOptions.setCheckOnly(opt.toBoolean))
        config.getProperty("deployOptions.ignoreWarnings").foreach(opt => deployOptions.setIgnoreWarnings(opt.toBoolean))
        config.getProperty("deployOptions.performRetrieve").foreach(opt => deployOptions.setPerformRetrieve(opt.toBoolean))
        config.getProperty("deployOptions.purgeOnDelete").foreach(opt => deployOptions.setPurgeOnDelete(opt.toBoolean))
        config.getProperty("deployOptions.rollbackOnError").foreach(opt => deployOptions.setRollbackOnError(opt.toBoolean))
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
 *      if --testsToRun=* (star) then run all tests in all Local classes (excluding managed packages)
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
                  |Note: makes sense only when --testsToRun is also specified
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

        override def getSummary: String =
            """Deploy modified files and (if requested) run tests
              |
              |Note - this command supports user defined Metadata API DeployOptions
              |     As usual configuration params can be set in one of 3 ways:
              |     1. command line: ... --deployOptions.allowMissingFiles=true --deployOptions.autoUpdatePackage=false ...
              |     2. java command line: java -DdeployOptions.allowMissingFiles=true -DdeployOptions.autoUpdatePackage=false ...
              |     3. config.properties file:
              |
              |          # this is a config file with SFDC Org access details
              |          sf.username = ...
              |          sf.password = ...
              |          deployOptions.allowMissingFiles = true
              |          deployOptions.autoUpdatePackage = true
              |
            """.stripMargin

        override def getName: String = "deployModified"
    }

    protected def act()(implicit ec: ExecutionContext): Future[ActionResult] = {
        val hasTestsToRun = config.getProperty("testsToRun").isDefined
        val modifiedFiles = getFiles
        val filesWithoutPackageXml = modifiedFiles.filterNot(_.getName == "package.xml")
        val actionResult =
            if (!hasTestsToRun && filesWithoutPackageXml.isEmpty) {
                reportEmptyFileList(modifiedFiles)
            } else {
                //first check if SFDC has newer version of files we are about to deploy
                val ignoreConflicts = config.getProperty("ignoreConflicts").getOrElse("false").toBoolean

                val resultBuilder = new ActionResultBuilder()
                val canDeploy = ignoreConflicts || !hasConflicts(modifiedFiles, resultBuilder) || hasTestsToRun

                if (canDeploy) {
                    deploy(modifiedFiles, isUpdateSessionDataOnSuccess, resultBuilder)
                }
                resultBuilder.result()
            }
        Future.successful(actionResult)
    }

    /**
     * list files specified in --specificFiles parameter
     */
    protected def getFiles:List[File] = {
        val modifiedFiles = new ListModified().load[ListModified](session).getModifiedFiles
        modifiedFiles
    }
    protected def reportEmptyFileList(files: List[File]): ActionResult = {
        //responseWriter.println("RESULT=SUCCESS")
        //responseWriter.println("FILE_COUNT=" + files.size)
        //responseWriter.println(InfoMessage("no modified files detected."))
        ActionSuccess(
            List(
                KeyValueMessage(Map("FILE_COUNT" -> files.size)),
                InfoMessage("no modified files detected.")
            )
        )
    }
    /**
     * @return - true if deployment is successful
     */
    def deploy(files: List[File], updateSessionDataOnSuccess: Boolean, resultBuilder: ActionResultBuilder): Boolean = {
        deploy(files, updateSessionDataOnSuccess, resultBuilder, None)
    }
    //TODO
    def deploy(files: List[File], updateSessionDataOnSuccess: Boolean, resultBuilder: ActionResultBuilder, alternativeSrcDir: Option[File] = None): Boolean = {

        var success = false //assume failure by default

        //for every modified file add its -meta.xml if exists
        val metaXmlFiles = for (file <- files;
                                metaXml = new File(file.getAbsolutePath + "-meta.xml")
                                if metaXml.exists()) yield metaXml
        //for every -meta.xml file make sure that its source file is also included
        val extraSourceFiles = for (file <- files.filter(_.getName.endsWith("-meta.xml"));
                                    sourceFile = new File(file.getAbsolutePath.replace("-meta.xml", ""))
                                    if sourceFile.exists()) yield sourceFile

        //for every file that is part of aura bundle, include all files in that bundle
        val auraFiles = files.flatMap(getAllFilesInAuraBundle(_)).distinct

        var allFilesToDeploySet = (files ++ metaXmlFiles ++ extraSourceFiles ++ auraFiles).toSet

        val packageXml = new MetaXml(getProjectConfig)
        val packageXmlFile = packageXml.getPackageXml
        if (!allFilesToDeploySet.contains(packageXmlFile)) {
            //deployment always must contain packageXml file
            allFilesToDeploySet += packageXmlFile
        }
        /*
        //DEBUG
        //for debug purpose only, to check what is put in the archive
        //get temp file name
        val destZip = FileUtils.createTempFile("deploy", ".zip")
        destZip.delete()

        ZipUtils.zipDir(session.getConfig.srcPath, destZip.getAbsolutePath, excludeFileFromZip(allFilesToDeploySet, _))
        //end DEBUG
        */

        val checkOnly = getSessionConfig.isCheckOnly
        val testMethodsByClassName: Map[String, Set[String]] = ApexTestUtils.getTestMethodsByClassName(config.getProperty("testsToRun"))
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
        if (testMethodsByClassName.nonEmpty) {
            if ("*" == testMethodsByClassName.keys.head) {
                deployOptions.setTestLevel(TestLevel.RunLocalTests)
            } else {
                deployOptions.setTestLevel(TestLevel.RunSpecifiedTests)
                deployOptions.setRunTests(testMethodsByClassName.keys.toArray)
            }
        } else {
            //this fails in Prod, so if no test classes to run provided then assume nothing needs to be done
            //deployOptions.setTestLevel(TestLevel.NoTestRun)
        }

        //top-up DeployOptions from user defined configuration
        loadDeployOptionsFromConfig(deployOptions)

        deployOptions.setCheckOnly(checkOnly)

        logger.info("Deploying...")
        if (alternativeSrcDir.isEmpty && session.getConfig.srcDirOpt.isEmpty) {
            //responseWriter.println(FAILURE)
            //responseWriter.println(ErrorMessage("src folder not found"))
            resultBuilder.setResultType(FAILURE)
            resultBuilder.addMessage(ErrorMessage("src folder not found"))
            return false
        }
        val srcDir = alternativeSrcDir.getOrElse(session.getConfig.srcDirOpt.get)
        val (deployResult, log) = session.deploy(ZipUtils.zipDirToBytes(srcDir, excludeFileFromZip(allFilesToDeploySet, _),
            disableNotNeededTests(_, testMethodsByClassName)), deployOptions)

        val deployDetails = deployResult.getDetails
        if (!deployResult.isSuccess) {
            //responseWriter.println(FAILURE)
            resultBuilder.setResultType(FAILURE)
            //dump details of failures into a response file
            writeDeploymentFailureReport(deployDetails, isRunningTests, resultBuilder)
        } else { //deployResult.isSuccess = true

            val runTestResult = deployDetails.getRunTestResult
            if (isRunningTests && (null == runTestResult || runTestResult.getFailures.isEmpty)) {
                //responseWriter.println(InfoMessage("Tests PASSED"))
                resultBuilder.addMessage(InfoMessage("Tests PASSED"))
            }
            if (isRunningTests && (null != runTestResult) && runTestResult.getTotalTime > 0 ) {
                logger.info(s"total cumulative time spent running tests: ${runTestResult.getTotalTime}")
            }
            ApexTestUtils.processCodeCoverage(new Deploy.RunTestResultMetadata(runTestResult), session, resultBuilder) match {
                case Some(coverageFile) =>
                    //responseWriter.println("COVERAGE_FILE=" + coverageFile.getAbsolutePath)
                    resultBuilder.addMessage( KeyValueMessage(Map("COVERAGE_FILE" -> coverageFile.getAbsolutePath)) )
                case _ =>
            }
            //update session data for successful files
            if (updateSessionDataOnSuccess) {
                updateSessionDataForSuccessfulFiles(deployResult, auraFiles)
            }
            session.storeSessionData()
            //responseWriter.println(SUCCESS)
            //responseWriter.println("FILE_COUNT=" + files.size)
            resultBuilder.setResultType(SUCCESS)
            resultBuilder.addMessage( KeyValueMessage(Map("FILE_COUNT" -> files.size)) )

            if (!checkOnly) {
                //responseWriter.startSection("DEPLOYED FILES")
                //files.foreach(f => responseWriter.println(f.getName))
                //responseWriter.endSection("DEPLOYED FILES")
                val deployedFilesMsg = InfoMessage("DEPLOYED FILES")
                resultBuilder.addMessage(deployedFilesMsg)
                files.foreach(f => resultBuilder.addDetail(MessageDetailText(deployedFilesMsg, f.getName)))


            }
            success = true
        }
        if (!log.isEmpty) {
            val logFile = getProjectConfig.getLogFile
            FileUtils.writeFile(log, logFile)
            //responseWriter.println("LOG_FILE=" + logFile.getAbsolutePath)
            resultBuilder.addMessage(KeyValueMessage(Map("LOG_FILE" -> logFile.getAbsolutePath)))
        }

        success
    }

    //update session data for successful files
    private def updateSessionDataForSuccessfulFiles (deployResult: com.sforce.soap.metadata.DeployResult, auraFiles: List[File]): Unit = {
        val describeByDir = DescribeMetadata.getDescribeByDirNameMap(session)
        val calculateMD5 = getSessionConfig.useMD5Hash
        val calculateCRC32 = !calculateMD5  //by default use only CRC32

        def processOneFile(f: File, successMessage: com.sforce.soap.metadata.DeployMessage, xmlType: String): Unit = {
            val relativePath = session.getRelativePath(f) //successMessage.getFileName
            val key = session.getKeyByRelativeFilePath(relativePath)
            val localMills = f.lastModified()

            val md5Hash = if (calculateMD5 && !f.isDirectory) FileUtils.getMD5Hash(f) else ""
            val crc32Hash = if (calculateCRC32 && !f.isDirectory) FileUtils.getCRC32Hash(f) else -1L

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

            ()
        }

        val deployDetails = deployResult.getDetails

        for ( successMessage <- deployDetails.getComponentSuccesses) {
            val relativePath = successMessage.getFileName
            getProjectConfig.projectDirOpt match {
                case Some(projectDir) =>
                    val f = new File(projectDir, relativePath)
                    if (f.isDirectory && AuraMember.BUNDLE_XML_TYPE == successMessage.getComponentType) {
                        //process bundle definition
                        //for aura bundles Metadata API deploy() reports only bundle name, not individual files
                        // process bundle dir
                        processOneFile(f, successMessage, AuraMember.BUNDLE_XML_TYPE)
                        // process individual files
                        FileUtils.listFiles(dir = f, descentIntoFolders = true, includeFolders = false)
                            .filter(AuraMember.isSupportedType)
                            .foreach(file =>
                                processOneFile(file, successMessage, AuraMember.XML_TYPE)
                            )
                    } else {
                        if (f.exists() && !f.isDirectory) {
                            val xmlType = describeByDir.get(f.getParentFile.getName) match {
                                case Some(describeMetadataObject) => describeMetadataObject.getXmlName
                                case None => "" //package.xml and -meta.xml do not have xmlType
                            }
                            processOneFile(f, successMessage, xmlType)
                        }
                    }
                case None =>
            }
        }
        //if there were aura files then we have to fetch their ids using Retrieve because Metadata deploy() does not return them
        AuraMember.updateAuraDefinitionData(session, auraFiles, idsOnly = false)
        //dump session data to disk
        session.storeSessionData()
    }


    /**
     * current version (v32.0) of metadata API fails to deploy packages if they contain incomplete Aura Bundle
     * i.e. if any single file from aura bundle is included (e.g. <app-name>.css ) then *all* files in this bunde must be
     * part of deployment package, otherwise SFDC returns: UNKNOWN_EXCEPTION: An unexpected error occurred.
     * @param fileInBundle - file which belongs to Aura bundle
     * @return
     */
    private def getAllFilesInAuraBundle(fileInBundle: File): Set[File] = {
        AuraMember.getAuraBundleDir(fileInBundle) match {
          case Some(bundleDir) => FileUtils.listFiles(bundleDir, descentIntoFolders = true, includeFolders = false).toSet
          case None => Set()
        }
    }

    private def writeDeploymentFailureReport(deployDetails: com.sforce.soap.metadata.DeployDetails, isRunningTests: Boolean, resultBuilder: ActionResultBuilder): Unit = {

        if (null != deployDetails) {
            //responseWriter.startSection("ERROR LIST")

            //display errors both as messages and as ERROR: lines
            val componentFailureMessage = WarnMessage("Component failures")
            if (deployDetails.getComponentFailures.nonEmpty) {
                //responseWriter.println(componentFailureMessage)
                resultBuilder.addMessage(componentFailureMessage)
            }
            for ( failureMessage <- deployDetails.getComponentFailures) {
                val line = failureMessage.getLineNumber
                val column = failureMessage.getColumnNumber
                val filePath = failureMessage.getFileName
                val problem = failureMessage.getProblem
                val problemType = failureMessage.getProblemType match {
                    case DeployProblemType.Warning => WARN
                    case DeployProblemType.Error => ERROR
                    case _ => ERROR
                }
                //responseWriter.println("ERROR", Map("type" -> problemType, "line" -> line, "column" -> column, "filePath" -> filePath, "text" -> problem))
                //responseWriter.println( MessageDetailMap(componentFailureMessage, Map("type" -> problemType, "filePath" -> filePath, "text" -> problem)))
                resultBuilder.addDetail( MessageDetailMap(componentFailureMessage, Map("type" -> problemType, "line" -> line, "column" -> column, "filePath" -> filePath, "text" -> problem)) )
            }
            //process test successes and failures
            val runTestResult = new RunTestResultMetadata(deployDetails.getRunTestResult)
            if (isRunningTests || (null != deployDetails.getRunTestResult && deployDetails.getRunTestResult.getFailures.nonEmpty) ) {
                ApexTestUtils.processTestResult(runTestResult, session, resultBuilder)
            }
            val coverageReportFile = ApexTestUtils.processCodeCoverage(runTestResult, session, resultBuilder)
            coverageReportFile match {
                case Some(coverageFile) =>
                    //responseWriter.println("COVERAGE_FILE=" + coverageFile.getAbsolutePath)
                    resultBuilder.addMessage(KeyValueMessage(Map("COVERAGE_FILE" -> coverageFile.getAbsolutePath)))
                case _ =>
            }
        }

    }

    private val alwaysIncludeNames = Set("src", "package.xml")

    private def excludeFileFromZip(modifiedFiles: Set[File], file: File) = {
        val exclude = !modifiedFiles.contains(file) && !alwaysIncludeNames.contains(file.getName)
        logger.trace(file.getName + " include=" + !exclude)
        exclude
    }

    def getFilesNewerOnRemote(files: List[File]): Option[List[Map[String, Any]]] = {
        val checker = new ListConflicting().load[ListConflicting](session)
        checker.getFilesNewerOnRemote(files)
    }

    protected def hasConflicts(files: List[File], actionResultBuilder: ActionResultBuilder): Boolean = {
        if (files.nonEmpty) {
            logger.info("Check Conflicts with Remote")
            getFilesNewerOnRemote(files) match {
                case Some(conflictingFiles) =>
                    if (conflictingFiles.nonEmpty) {
                        //responseWriter.println(FAILURE)
                        actionResultBuilder.setResultType(FAILURE)

                        val msg = WarnMessage("Outdated file(s) detected.")
                        //responseWriter.println(msg)
                        actionResultBuilder.addMessage(msg)
                        val checker = new ListConflicting().load[ListConflicting](session)
                        checker.generateConflictMessageDetails(conflictingFiles, msg).
                            foreach{detail => actionResultBuilder.addDetail(detail)}
                        //responseWriter.println(WarnMessage("Use 'refresh' before 'deploy'."))
                        actionResultBuilder.addMessage(WarnMessage("Use 'refresh' before 'deploy'."))
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
     * if testsToRun contains names of specific methodsToKeep then we need to disable all other test methods in deployment package
     */
    private def disableNotNeededTests(classFile: File, testMethodsByClassName: Map[String, Set[String]]): File = {
        testMethodsByClassName.get(FileUtils.removeExtension(classFile)) match {
            case Some(methodsSet) if methodsSet.nonEmpty =>
                if (!getSessionConfig.isCheckOnly) {
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

        val source = FileUtils.readFile(classFile)
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

    protected override def act()(implicit ec: ExecutionContext): Future[ActionResult] = {
        val allFiles = getAllFiles
        val resultBuilder = new ActionResultBuilder()
        deploy(allFiles, isUpdateSessionDataOnSuccess, resultBuilder)

        Future.successful(resultBuilder.result())
    }

    /**
     * list locally modified files using data from session.properties
     */
    def getAllFiles:List[File] = {
        val config = session.getConfig
        config.srcDirOpt match {
            case Some(srcDir) =>
                val allFiles  = FileUtils.listFiles(srcDir).filter(
                    //remove all non apex files
                    file => DescribeMetadata.isValidApexFile(session, file)
                ).toSet

                allFiles.toList
            case None => Nil
        }
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
    protected override def act()(implicit ec: ExecutionContext): Future[ActionResult] = {

        val diffWithRemote = new DiffWithRemote {
            /**
             * path to folder where remote version of current project will be saved
             * @return - Option(/path/to/folder)
             */
            override def getTargetFolder: Option[String] = Some(FileUtils.createTempDir("remote_files").getAbsolutePath)
        }
        diffWithRemote.load[DiffWithRemote](session)

        val resultBuilder = new ActionResultBuilder()
        val actionResult =
            diffWithRemote.getDiffReport(resultBuilder) match {
                case None =>
                    //responseWriter.println(FAILURE)
                    //responseWriter.println(ErrorMessage("Failed to load remote version of current project"))
                    ActionFailure(ErrorMessage("Failed to load remote version of current project"))

                case Some(diffReport) =>
                    val dummyFilesBuilder = Map.newBuilder[String, File]
                    val keysToDeleteWithoutDummy = List.newBuilder[String]
                    val dummyMetaFilesBuilder = List.newBuilder[File]
                    val dummyFilesDir = FileUtils.createTempDir("dummyFiles")
                    val meta = new MetaXml(session.getConfig)
                    for (file <- diffReport.getRemoteFilesMissingLocally.values) {
                        val relativePath = DescribeMetadata.getApexFolderNameByFile(session, file).getOrElse("") + "/" + file.getName
                        StubFileGenerator.generateStub(meta.getPackage.getVersion, dummyFilesDir, file, withMetaXml = true) match {
                            case Some((dummy, Some(metaXml))) =>
                                dummyFilesBuilder += relativePath -> dummy
                                dummyMetaFilesBuilder += metaXml
                            case Some((dummy, None)) =>
                                dummyFilesBuilder += relativePath -> dummy
                            case None =>
                                //stub for this file type is not needed
                                keysToDeleteWithoutDummy += relativePath
                        }
                    }
                    val allLocalFiles = getAllFiles
                    val dummyFileByRelativePath = dummyFilesBuilder.result()
                    val allFiles = allLocalFiles ++ dummyFileByRelativePath.values ++ dummyMetaFilesBuilder.result()
                    //at this point we have two types of files
                    //1 - files under the current project folder
                    //2 - dummy/blank files somewhere in temp directory

                    val (srcDir, filesToDeploy) = moveAllFilesUnderOneSrc(allFiles)

                    val isDeploySuccessful = deploy(filesToDeploy, isUpdateSessionDataOnSuccess, resultBuilder, Some(srcDir))
                    if (isDeploySuccessful) {
                        //save response file
                        //execute DeployDestructive using list of blank files
                        deleteFiles(dummyFileByRelativePath.keys ++ keysToDeleteWithoutDummy.result())
                        //responseWriter.println(new Message(INFO, "REMOTE_VERSION_BACKUP_PATH=" + diffReport.remoteSrcFolderPath))
                        resultBuilder.addMessage(InfoMessage("REMOTE_VERSION_BACKUP_PATH=" + diffReport.remoteSrcFolderPath))
                    }
                    resultBuilder.result()

            }
        Future.successful(actionResult)
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
    private def deleteFiles(relativePathsToDelete: Iterable[String])(implicit ec: ExecutionContext): Future[ActionResult] = {
        val actionResultFuture =
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
                        val updateSessionDataOnSuccess = !getSessionConfig.isCheckOnly && !session.callingAnotherOrg
                        updateSessionDataOnSuccess
                    }
                }
                deployDestructiveAction.load[DeployDestructive](session)

                //make sure that Delete operation appends to response file, rather than overwrites it
                //deployDestructiveAction.setResponseWriter(this.responseWriter)

                deployDestructiveAction.act().map{
                    case result @ ActionSuccess(_, _) =>
                        //get rid of temp file
                        componentsToDeleteFile.delete()
                        result
                    case result @ ActionFailure(_) =>
                        //get rid of temp file
                        componentsToDeleteFile.delete()
                        result

                }

            } else {
                Future.successful(ActionSuccess())
            }
        actionResultFuture
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

    /**
     * list files specified in --specificFiles parameter
     */
    protected override def getFiles:List[File] = {
        val config = session.getConfig
        //load file list from specified file
        val fileListFile = new File(config.getRequiredProperty("specificFiles").get)
        session.listApexFilesFromFile(fileListFile)
    }


    protected override def reportEmptyFileList(files: List[File]): ActionResult = {
        //responseWriter.println(FAILURE)
        val fileListFile = new File(config.getRequiredProperty("specificFiles").get)
        //responseWriter.println(ErrorMessage("no valid files in " + fileListFile))
        ActionFailure("no valid files in " + fileListFile)
    }
}

/**
 * list modified files (compared to their stored session data)
 */
class ListModified extends ApexActionWithReadOnlySession {
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
        //logger.debug("packageXmlData=" + packageXmlData)
        //val allFiles  = (packageXmlFile :: FileUtils.listFiles(config.srcDir)).toSet
        val allFiles = getProjectConfig.srcDirOpt match {
            case Some(srcDir) =>
                FileUtils.listFiles(srcDir).filter(
                    //remove all non apex files
                    file => DescribeMetadata.isValidApexFile(session, file)
                ).toSet
            case None => Set()
        }

        val modifiedFiles = allFiles.filter(session.isModified(_))
        modifiedFiles.toList
    }

    /*
    def reportModifiedFiles(modifiedFiles: List[File],
                            messageType: MessageType = INFO,
                            resultBuilder: ActionResultBuilder): ActionResultBuilder = {
        val msg = ArbitraryTypeMessage(messageType, "Modified file(s) detected.", Map("code" -> "HAS_MODIFIED_FILES"))
        //responseWriter.println(msg)
        resultBuilder.addMessage(msg)

        for(f <- modifiedFiles) {
            //responseWriter.println(MessageDetail(msg, Map("filePath" -> f.getAbsolutePath, "text" -> session.getRelativePath(f))))
            resultBuilder.addDetail(MessageDetailMap(msg, Map("filePath" -> f.getAbsolutePath, "text" -> session.getRelativePath(f))))
        }
        //responseWriter.println("HAS_MODIFIED_FILES=true")
        resultBuilder.addMessage(KeyValueMessage(Map("HAS_MODIFIED_FILES" -> "true")))

        //responseWriter.startSection("MODIFIED FILE LIST")
        val modifiedFileListMsg = resultBuilder.addMessage(InfoMessage("MODIFIED FILE LIST"))
        for(f <- modifiedFiles) {
            //responseWriter.println("MODIFIED_FILE=" + session.getRelativePath(f))
            resultBuilder.addDetail(
                MessageDetailMap(
                    modifiedFileListMsg,
                    Map("MODIFIED_FILE" -> session.getRelativePath(f))
                )
            )
        }
        //responseWriter.endSection("MODIFIED FILE LIST")
        resultBuilder

    }
    
    def reportDeletedFiles(deletedFiles: List[File], resultBuilder: ActionResultBuilder): ActionResultBuilder = {
        val msg = InfoMessage("Deleted file(s) detected.", Map("code" -> "HAS_DELETED_FILES"))
        //responseWriter.println(msg)
        resultBuilder.addMessage(msg)
        for(f <- deletedFiles) {
            //responseWriter.println(MessageDetail(msg, Map("filePath" -> f.getAbsolutePath, "text" -> session.getRelativePath(f))))
            resultBuilder.addDetail(MessageDetailMap(msg, Map("filePath" -> f.getAbsolutePath, "text" -> session.getRelativePath(f))))
        }
        //responseWriter.println("HAS_DELETED_FILES=true")
        resultBuilder.addMessage(KeyValueMessage(Map("HAS_DELETED_FILES" -> "true")))

        //responseWriter.startSection("DELETED FILE LIST")
        val deletedFileListMsg = InfoMessage("DELETED FILE LIST")
        for(f <- deletedFiles) {
            //responseWriter.println("DELETED_FILE=" + session.getRelativePath(f))

            resultBuilder.addDetail(
                MessageDetailMap(
                    deletedFileListMsg,
                    Map("DELETED_FILE" -> session.getRelativePath(f))
                )
            )
        }
        //responseWriter.endSection("DELETED FILE LIST")
        resultBuilder
    }
    */

    protected def act()(implicit ec: ExecutionContext): Future[ActionResult] = {
        getProjectConfig.projectDirOpt match {
            case Some(projectDir) =>
                val modifiedFiles = getModifiedFiles
                val deletedFiles = session.getDeletedLocalFilePaths(extraSrcFoldersToLookIn = Nil).map(new File(projectDir, _))

                /*
                //responseWriter.println(SUCCESS)
                val successBuilder = new ActionResultBuilder(SUCCESS)
                //responseWriter.println("FILE_COUNT=" + modifiedFiles.size + deletedFiles.size)
                successBuilder.addMessage(KeyValueMessage(Map("FILE_COUNT" -> (modifiedFiles.size + deletedFiles.size) )))
                if (modifiedFiles.nonEmpty) {
                    reportModifiedFiles(modifiedFiles, INFO, successBuilder)
                } else {
                    //responseWriter.println(new Message(INFO, "No Modified file(s) detected."))
                    successBuilder.addMessage(InfoMessage("No Modified file(s) detected."))
                }

                if (deletedFiles.nonEmpty) {
                    reportDeletedFiles(deletedFiles, successBuilder)
                } else {
                    //responseWriter.println(new Message(INFO, "No Deleted file(s) detected."))
                }
                Future.successful(successBuilder.result())
                */
                Future.successful(ActionSuccess(ListModifiedResult(modifiedFiles, deletedFiles)))
            case None =>
                //responseWriter.println(FAILURE)
                //responseWriter.println(ErrorMessage("Invalid or Missing Project Path"))
                Future.successful(ActionFailure("Invalid or Missing Project Path"))
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
              |
              |Note - this command supports user defined Metadata API DeployOptions
              |     As usual configuration params can be set in one of 3 ways:
              |     1. command line: ... --deployOptions.allowMissingFiles=true --deployOptions.autoUpdatePackage=false ...
              |     2. java command line: java -DdeployOptions.allowMissingFiles=true -DdeployOptions.autoUpdatePackage=false ...
              |     3. config.properties file:
              |
              |          # this is a config file with SFDC Org access details
              |          sf.username = ...
              |          sf.password = ...
              |          deployOptions.allowMissingFiles = true
              |          deployOptions.autoUpdatePackage = true
              |
            """.stripMargin

        override def getName: String = "deleteMetadata"
    }

    def getSpecificComponentsFilePath: String = config.getRequiredProperty("specificComponents").get

    override def act()(implicit ec: ExecutionContext): Future[ActionResult] = {
        val components = getComponentPaths
        val actionResult =
            if (components.isEmpty) {
                //responseWriter.println("RESULT=FAILURE")
                val componentListFile = new File(config.getRequiredProperty("specificComponents").get)
                //responseWriter.println(new Message(ERROR, "no valid components in " + componentListFile))
                ActionFailure("no valid components in " + componentListFile)
            } else {

                val deployOptions = new DeployOptions()
                deployOptions.setPerformRetrieve(false)
                deployOptions.setAllowMissingFiles(false)
                deployOptions.setRollbackOnError(true)

                //top-up DeployOptions from user defined configuration
                loadDeployOptionsFromConfig(deployOptions)

                val checkOnly = getSessionConfig.isCheckOnly
                deployOptions.setCheckOnly(checkOnly)

                val tempDir = generateDeploymentDir(components)
                logger.info("Deleting...")
                val (deployResult, log) = session.deploy(ZipUtils.zipDirToBytes(tempDir), deployOptions)

                val deployDetails = deployResult.getDetails
                val resultBuilder = new ActionResultBuilder()
                if (!deployResult.isSuccess) {
                    //responseWriter.println("RESULT=FAILURE")
                    resultBuilder.setResultType(FAILURE)
                    if (null != deployDetails) {
                        //responseWriter.startSection("ERROR LIST")

                        //display errors both as messages and as ERROR: lines
                        val componentFailureMessage = WarnMessage("Component failures")
                        if (deployDetails.getComponentFailures.nonEmpty) {
                            //responseWriter.println(componentFailureMessage)
                            resultBuilder.addMessage(componentFailureMessage)
                        }

                        for ( failureMessage <- deployDetails.getComponentFailures) {
                            //first part of the file usually looks like deleteMetadata/classes/AccountController.cls
                            //make it src/classes/AccountController.cls
                            val filePath = failureMessage.getFileName match {
                                case null => ""
                                case "" => ""
                                case str =>
                                    val pathSplit = str.split('/')
                                    if (3 == pathSplit.length) {
                                        //this is a standard 3 component path, make first part src/
                                        "src" + str.dropWhile(_ != '/')
                                    } else {
                                        str
                                    }
                            }
                            val problemWithFilePath = if (filePath.isEmpty) failureMessage.getProblem else filePath + ": " + failureMessage.getProblem
                            val problemType = failureMessage.getProblemType match {
                                case DeployProblemType.Warning => WARN
                                case DeployProblemType.Error => ERROR
                                case _ => ERROR
                            }
                            //responseWriter.println("ERROR", Map("type" -> problemType, "text" -> failureMessage.getProblem, "filePath" -> filePath))
                            //responseWriter.println(new MessageDetailMap(componentFailureMessage, Map("type" -> problemType, "text" -> problemWithFilePath, "filePath" -> filePath)))
                            resultBuilder.addDetail(MessageDetailMap(componentFailureMessage, Map("type" -> problemType, "text" -> problemWithFilePath, "filePath" -> filePath)))
                        }
                        //responseWriter.endSection("ERROR LIST")
                    }
                } else {
                    //responseWriter.println("RESULT=SUCCESS")
                    resultBuilder.setResultType(SUCCESS)
                    if (isUpdateSessionDataOnSuccess) {
                        //responseWriter.debug("Updating session data")
                        resultBuilder.addMessage(DebugMessage("Updating session data"))
                        for (componentPath <- components) {
                            val pair = componentPath.split('/')
                            session.findKey(pair(0), pair(1)) match {
                                case Some(key) =>
                                    session.removeData(key)
                                    //responseWriter.debug("Removed session data for key: " + key)
                                    resultBuilder.addMessage(DebugMessage("Removed session data for key: " + key))
                                case None =>
                            }
                        }
                        session.storeSessionData()
                    }
                }
                resultBuilder.result()
            }

        Future.successful(actionResult)
    }

    def generateDeploymentDir(componentPaths: List[String]): File = {
        val tempDir = FileUtils.createTempDir(config)
        val metaXml = new MetaXml(getProjectConfig)
        //place destructiveChanges.xml and package.xml in this temp folder
        val destructivePackage = getDestructiveChangesPackage(componentPaths)
        val destructiveXml = metaXml.packageToXml(destructivePackage)
        FileUtils.writeFile(destructiveXml, new File(tempDir, "destructiveChanges.xml"))
        //scala.xml.XML.save(new File(tempDir, "destructiveChanges.xml").getAbsolutePath, destructiveXml, enc = "UTF-8", xmlDecl = true )

        val packageXml = metaXml.packageToXml(getEmptyPackage)
        FileUtils.writeFile(packageXml, new File(tempDir, "package.xml"))
        //scala.xml.XML.save(new File(tempDir, "package.xml").getAbsolutePath, packageXml, enc = "UTF-8" )
        tempDir
    }
    /**
     * list locally modified files using data from session.properties
     */
    def getComponentPaths: List[String] = {

        //load file list from specified file
        val componentsPath = getSpecificComponentsFilePath
        val componentListFile = new File(componentsPath)
        val components:List[String] = FileUtils.readFile(componentListFile).getLines().filter(!_.trim.isEmpty).toList
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
                          _objNames.map(_.drop(dirName.length + 1)).map(
                              name => if (name.endsWith(extension)) name.dropRight(extension.length) else name
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

    protected override def act()(implicit ec: ExecutionContext): Future[ActionResult] = {
        val hasTestsToRun = config.getProperty("testsToRun").nonEmpty
        val listModified = new ListModified().load[ListModified](session)
        val modifiedFiles = listModified.getModifiedFiles
        val filesWithoutPackageXml = modifiedFiles.filterNot(_.getName == "package.xml")

        val actionResultBuilder = new ActionResultBuilder()

        val isOkToDelete =
        if (!hasTestsToRun && filesWithoutPackageXml.isEmpty) {
            if (session.getDeletedLocalFilePaths(extraSrcFoldersToLookIn = Nil).isEmpty) {
                //responseWriter.println("RESULT=SUCCESS")
                actionResultBuilder.setResultType(SUCCESS)
                //responseWriter.println("FILE_COUNT=" + modifiedFiles.size)
                actionResultBuilder.addMessage(KeyValueMessage(Map("FILE_COUNT" -> modifiedFiles.size)))
                //responseWriter.println(new Message(INFO, "no modified files detected."))
                actionResultBuilder.addMessage(InfoMessage("no modified files detected."))
                false
            } else {
                //process files that need to be deleted
                //deleteFiles()
                true
            }
        } else {
            //first check if SFDC has newer version of files we are about to deploy
            val ignoreConflicts = config.getProperty("ignoreConflicts").getOrElse("false").toBoolean
            //val hasConflicts = hasConflicts(deletedFiles)

            val canDeploy = ignoreConflicts || !hasConflicts(modifiedFiles, actionResultBuilder) || hasTestsToRun

            if (canDeploy && deploy(modifiedFiles, isUpdateSessionDataOnSuccess, actionResultBuilder)) {
                //process files that need to be deleted
                //deleteFiles()
                true
            } else {
                false
            }
        }
        if (isOkToDelete) {
            deleteFiles().map{ deleteResult =>
                actionResultBuilder.add(deleteResult)
                actionResultBuilder.result()
            }
        } else {
            Future.successful(actionResultBuilder.result())
        }
    }
    private def deleteFiles()(implicit ec: ExecutionContext): Future[ActionResult] = {
        val deletedLocalFilePaths = session.getDeletedLocalFilePaths(extraSrcFoldersToLookIn = Nil).map(_.substring("src/".length))
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
                    val updateSessionDataOnSuccess = !getSessionConfig.isCheckOnly && !session.callingAnotherOrg
                    updateSessionDataOnSuccess
                }
            }
            deployDestructiveAction.load[DeployDestructive](session)

            deployDestructiveAction.act()
                .map{actionResult =>
                    //get rid of temp file
                    componentsToDeleteFile.delete()
                    actionResult
                }
        } else {
            Future.successful(ActionSuccess("Nothing to delete - File list is empty or does not include valid files"))
        }

    }

}
