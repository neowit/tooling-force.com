package com.neowit.apex.actions

import java.io.{File, FileOutputStream}

import com.neowit.apex.{MetaXml, MetadataType}
import com.neowit.utils._
import com.sforce.soap.metadata.{FileProperties, RetrieveMessage, RetrieveRequest, RetrieveResult}

import scala.collection.mutable
import scala.util.{Failure, Success, Try}
import spray.json._
import com.neowit.response._

import scala.concurrent.{ExecutionContext, Future}

case class RetrieveError(retrieveResult: RetrieveResult) extends Error

abstract class RetrieveMetadata extends ApexActionWithWritableSession {

    def setUpackaged(retrieveRequest: RetrieveRequest): Unit = {
        val metaXml = new MetaXml(session.getConfig)
        val unpackagedManifest = metaXml.getPackageXml
        logger.debug("Manifest file: " + unpackagedManifest.getAbsolutePath)

        retrieveRequest.setUnpackaged(metaXml.getPackage)
    }

    /**
     * retrieve information about specific files
     * used mainly to check conflicts between remote versions of local files
     * @param files - list of files to check status on Remote
     */
    def retrieveFiles(files: List[File], reportMissing: Boolean = true): RetrieveResult = {
        val retrieveRequest = new RetrieveRequest()
        retrieveRequest.setApiVersion(config.apiVersion)
        //setSpecificFiles requires file names that look like: unpackaged/classes/MyClass.cls
        retrieveRequest.setSpecificFiles(files.map(session.getRelativePath(_).replaceFirst("src/", "unpackaged/")).toArray)
        //retrieveRequest.setSinglePackage(true) //do NOT use setSinglePackage(), it causes fileNames to lose "unpackaged/"
        setUpackaged(retrieveRequest)

        val retrieveResult = session.retrieve(retrieveRequest)
        retrieveResult.getMessages match {
            case messages if null != messages && messages.nonEmpty=>
                //check if all errors we have are about missing files
                val messagesOtherThanMissingFiles = messages.filterNot(isMissingFileError(_))
                if (reportMissing || messagesOtherThanMissingFiles.nonEmpty) {
                    throw RetrieveError(retrieveResult)
                }
            case _ =>
        }
        retrieveResult

    }

    /**
     * filter errors like
     * "In field: specific ids - no ApexTrigger named Contact found"
     * "In field: specific ids - no ApexClass named ContactController found"
     */
    private def isMissingFileError(message: RetrieveMessage): Boolean = {
        message.getProblem.matches("""In field: specific ids - no .* named .* found""")
    }

    /**
     * remove all file keys from session which do not correspond existing files
     */
    private def cleanSessionKeys(extraSrcFoldersToLookIn: List[File]): Unit = {
        val keysToDelete = session.getDeletedLocalFilePaths(extraSrcFoldersToLookIn).map(session.getKeyByRelativeFilePath(_))
        keysToDelete.foreach(session.removeData(_))
    }

    /**
     * using ZIP file produced, for example, as a result of Retrieve operation
     * extract content and generate response file
     */
    def updateFromRetrieve(retrieveResult: com.sforce.soap.metadata.RetrieveResult,
                           tempFolder: File,
                           updateSessionData: Boolean = true): Map[String, FileProperties] = {

        //val outputPath = appConfig.srcDir.getParentFile.getAbsolutePath
        //extract in temp area first
        val resultsFile = FileUtils.createTempFile("retrieveResult", ".zip")
        val out = new FileOutputStream(resultsFile)
        try {
            out.write(retrieveResult.getZipFile)
        } finally {
            out.close()
        }
        val calculateMD5 = getSessionConfig.useMD5Hash
        val calculateCRC32 = !calculateMD5  //by default use only CRC32

        val propertyByFilePath = new collection.mutable.HashMap[String,  com.sforce.soap.metadata.FileProperties]()
        try {
            val localDateAndHashByFName = ZipUtils.extract(resultsFile, tempFolder, calculateMd5 = calculateMD5, calculateCRC32 = calculateCRC32)
            //update session with file properties
            for (fileProp <- retrieveResult.getFileProperties) {
                val key = MetadataType.getKey(fileProp)
                val (lastModifiedLocally, md5Hash, crc32Hash) = localDateAndHashByFName(fileProp.getFileName)
                //check if we have -meta.xml data
                val metaFileName = fileProp.getFileName + "-meta.xml"
                val (metaLastModifiedLocally:Long, metaMD5Hash: String, metaCRC32Hash: Long) =
                    if (localDateAndHashByFName.contains(metaFileName)) {
                        localDateAndHashByFName(metaFileName)
                    } else {
                        (-1L, "", -1L)
                    }

                val valueMap = MetadataType.getValueMap(fileProp, lastModifiedLocally, md5Hash, crc32Hash, metaLastModifiedLocally, metaMD5Hash, metaCRC32Hash )
                if (updateSessionData) {
                    session.setData(key, valueMap)
                }

                propertyByFilePath.put(fileProp.getFileName, fileProp)
            }

            //finally make sure that session does not contain keys for files which no longer exist
            if (updateSessionData) {

                FileUtils.findSrcFolder(tempFolder) match {
                    case Some(srcFolder) =>
                        cleanSessionKeys(List(srcFolder))
                    case None =>
                        cleanSessionKeys(Nil)
                }
            }
        } finally {
            session.storeSessionData()
            resultsFile.delete()
        }
        propertyByFilePath.toMap
    }
}

/**
 * 'refresh' action is 'retrieve' for all elements specified in package.xml into project folder
 */
class RefreshMetadata extends RetrieveMetadata {

    override def getHelp: ActionHelp = new ActionHelp {
        override def getParamDescription(paramName: String): String = paramName match {
            case "config" => "--config - full path to config.properties file"
            case "projectPath" => "--projectPath - full path to folder which contains ./src/ of apex project."
            case "responseFilePath" => "--responseFilePath - full path to file where result of the operation will be documented."
            case "skipModifiedFilesCheck" => "--skipModifiedFilesCheck - unsafe retrieval, do not check for modified files before retrieval."
            case "modifiedFilesResultCode" => "--modifiedFilesResultCode - If provided (and project has modified files) then instead of normal 'RESULT=FAILURE' 'RESULT=' will be reported with given code."
        }

        override def getParamNames: List[String] = List("config", "projectPath", "responseFilePath", "skipModifiedFilesCheck", "modifiedFilesResultCode")

        override def getExample: String = ""

        override def getSummary: String = "allows to 'retrieve' all elements specified in package.xml"

        override def getName: String = "refresh"
    }

    protected def act()(implicit ec: ExecutionContext): Future[ActionResult] = {
        val actionResultBuilder = new ActionResultBuilder()
        try {
            //first check if we have modified files
            val skipModifiedFilesCheck = config.getProperty("skipModifiedFilesCheck").getOrElse("false").toBoolean
            val modifiedFileChecker = new ListModified().load[ListModified](session)
            val modifiedFiles = if (skipModifiedFilesCheck) Nil else modifiedFileChecker.getModifiedFiles

            if (modifiedFiles.isEmpty) {
                val retrieveRequest = new RetrieveRequest()
                retrieveRequest.setApiVersion(config.apiVersion)
                setUpackaged(retrieveRequest)
                Try(session.retrieve(retrieveRequest)) match {
                    case Success(retrieveResult) =>
                        updateFromRetrieve(retrieveResult, actionResultBuilder)
                    case Failure(err) =>
                        err match {
                            case e: RetrieveError =>
                                err.asInstanceOf[RetrieveError].retrieveResult.getMessages match {
                                    case messages if null != messages && messages.nonEmpty=>
                                        //responseWriter.println("RESULT=FAILURE")
                                        actionResultBuilder.setResultType(FAILURE)
                                        for(msg <- messages) {
                                            //responseWriter.println(msg.getFileName + ": " + msg.getProblem)
                                            actionResultBuilder.addMessage(ErrorMessage(msg.getFileName + ": " + msg.getProblem))
                                        }
                                    case _ =>
                                }
                            case _ =>
                                throw err
                        }
                }
            } else {
                //responseWriter.println("RESULT=FAILURE")
                // in some cases reporting RESULT=FAILURE (if modified files detected) may not be desirable
                // check if user requested alternative result code
                config.getProperty("modifiedFilesResultCode") match {
                    case Some("SUCCESS") =>
                        actionResultBuilder.setResultType(SUCCESS)
                    case _ =>
                        actionResultBuilder.setResultType(FAILURE)
                }
                //responseWriter.println("RESULT=" + modifiedFilesResultCode)
                //responseWriter.println(new Message(ResponseWriter.DEBUG, "Use --skipModifiedFilesCheck=true command line option to force Refresh"))
                actionResultBuilder.addMessage(DebugMessage("Use --skipModifiedFilesCheck=true command line option to force Refresh"))

                modifiedFileChecker.reportModifiedFiles(modifiedFiles, WARN, actionResultBuilder)
            }
        } catch {
            case ex:Throwable =>
                println(ex)
                //responseWriter.println("RESULT=FAILURE")
                //responseWriter.println(new Message(ERROR, ex.toString))
                actionResultBuilder.setResultType(FAILURE)
                actionResultBuilder.addMessage(ErrorMessage(ex.toString))
        }
        Future.successful(actionResultBuilder.result())
    }
    /**
     * using ZIP file produced, for example, as a result of Retrieve operation
     * extract content and generate response file
     */
    def updateFromRetrieve(retrieveResult: com.sforce.soap.metadata.RetrieveResult, actionResultBuilder: ActionResultBuilder): Unit = {
        val tempFolder = FileUtils.createTempDir(config)
        val filePropsMap = updateFromRetrieve(retrieveResult, tempFolder)
        //clear Ids of all files not loaded from the Org
        session.resetData(filePropsMap.keySet)

        //config.responseWriter.println("RESULT=SUCCESS")
        //config.responseWriter.println("RESULT_FOLDER=" + tempFolder.getAbsolutePath)
        //config.responseWriter.println("FILE_COUNT=" + filePropsMap.values.count(props => !props.getFullName.endsWith("-meta.xml") && props.getFullName != "package.xml"))
        actionResultBuilder.setResultType(SUCCESS)
        actionResultBuilder.addMessage(KeyValueMessage(Map("RESULT_FOLDER" -> tempFolder.getAbsolutePath)))
        actionResultBuilder.addMessage(KeyValueMessage(Map("FILE_COUNT" -> filePropsMap.values.count(props => !props.getFullName.endsWith("-meta.xml") && props.getFullName != "package.xml"))))

    }
}
/**
 * check local modified files against their Remote versions to see if remote is newer
 */
class ListConflicting extends RetrieveMetadata {

    override def isSessionReadOnly: Boolean = false

    override def getHelp: ActionHelp = new ActionHelp {
        override def getExample: String = ""

        override def getParamDescription(paramName: String): String = paramName match {
            case "config" => "--config - full path to config.properties file"
            case "projectPath" => "--projectPath - full path to folder which contains ./src/ of apex project."
            case "responseFilePath" => "--responseFilePath - full path to file where result of the operation will be documented."
        }

        override def getParamNames: List[String] = List("config", "projectPath", "responseFilePath")

        override def getSummary: String =
            """ Check if there have been updates of remote (SFDC) versions of local files since last 'refresh' or 'deploy' operation
          | """

        override def getName: String = "listConflicts"
    }

    def getFilesNewerOnRemote(files: List[File]): Option[List[Map[String, Any]]] = {
        val filesWithoutPackageXml = files.filterNot(_.getName == "package.xml")
        if (filesWithoutPackageXml.isEmpty) {
            Option(filesWithoutPackageXml.map(f => Map("file" -> f)))
        } else {
            val fileMap = filesWithoutPackageXml.map(f => (session.getRelativePath(f).replaceFirst("src/", "unpackaged/"), f) ).toMap

            Try(retrieveFiles(filesWithoutPackageXml, reportMissing = false)) match {
                case Success(retrieveResult) =>
                    val newerProps = retrieveResult.getFileProperties.filter(
                        props => {
                            val key = props.getFileName

                            val millsLocal = session.getData(key).getOrElse("LastModifiedDateMills", 0).toString.toLong
                            val millsRemote = MetadataType.getLastModifiedDateMills(props)
                            millsLocal < millsRemote
                        }
                    )
                    val res = newerProps.map(
                        props => {
                            val key = props.getFileName
                            val millsLocal = session.getData(key).getOrElse("LastModifiedDateMills", 0).toString.toLong
                            Map(
                                "file" -> fileMap(props.getFileName),
                                "LastModifiedByName" -> props.getLastModifiedByName,
                                "LastModifiedById" -> props.getLastModifiedById,
                                "Remote-LastModifiedDateStr" -> ZuluTime.formatDateGMT(props.getLastModifiedDate),
                                "Local-LastModifiedDateStr" -> ZuluTime.formatDateGMT(ZuluTime.toCalendar(millsLocal))
                            )
                    })
                    Some(res.toList)

                case Failure(err) =>
                    throw err
            }
        }

    }

    /**
     * using list of conflict data returned by getFilesNewerOnRemote() generate MessageDetail for given Message
     * @param conflictingFiles - list of info maps about each file
     *   each entry in the lust looks like so:
     *   Map(
     *        "file" -> fileMap(props.getFileName),
     *        "LastModifiedByName" -> props.getLastModifiedByName,
     *        "LastModifiedById" -> props.getLastModifiedById,
     *        "Remote-LastModifiedDateStr" -> ZuluTime.formatDateGMT(props.getLastModifiedDate),
     *        "Local-LastModifiedDateStr" -> ZuluTime.formatDateGMT(ZuluTime.toCalendar(millsLocal))
     *    )
     * @param msg - parent message
     * @return
     */
    def generateConflictMessageDetails(conflictingFiles: List[Map[String, Any]], msg: Message):List[MessageDetail] = {
        conflictingFiles.map(
            prop => {
                var text = ""
                val filePath = prop.get("file")  match {
                    case Some(x) =>
                        val f = x.asInstanceOf[File]
                        text = f.getName
                        text += " => Modified By: " + prop.getOrElse("LastModifiedByName", "")
                        text += "; at: " + prop.getOrElse("Remote-LastModifiedDateStr", "")
                        text += "; Local version saved at: " + prop.getOrElse("Local-LastModifiedDateStr", "")
                        f.getAbsolutePath
                    case None => ""
                }
                //config.responseWriter.println(new MessageDetail(msg, Map("filePath" -> filePath, "text" -> text)))
                MessageDetailMap(msg, Map("filePath" -> filePath, "text" -> text))
            }
        )
    }

    protected def act()(implicit ec: ExecutionContext): Future[ActionResult] = {
        val checker = new ListModified().load[ListModified](session)
        val modifiedFiles = checker.getModifiedFiles

        val actionResultBuilder = new ActionResultBuilder(SUCCESS)

        getFilesNewerOnRemote(modifiedFiles) match {
            case Some(fileProps) =>
                //config.responseWriter.println("RESULT=SUCCESS")
                actionResultBuilder.setResultType(SUCCESS)
                if (fileProps.nonEmpty) {
                    val msg = InfoMessage("Outdated file(s) detected.")
                    //config.responseWriter.println(msg)
                    actionResultBuilder.addMessage(msg)
                    //generateConflictMessageDetails(fileProps, msg).foreach{detail => config.responseWriter.println(detail)}
                    generateConflictMessageDetails(fileProps, msg).foreach{detail => actionResultBuilder.addDetail(detail)}
                } else {
                    //config.responseWriter.println(new Message(INFO, "No outdated files detected."))
                    actionResultBuilder.addMessage(InfoMessage("No outdated files detected."))
                }
                //fileProps.isEmpty
            case None =>
        }
        Future.successful(actionResultBuilder.result())
    }
}

/**
 * result returned by BulkRetrieve.doRetrieve(...) call
 * @param errors - list of errors in ResponseWriter.Message format
 * @param fileCountByType - map that looks like so: "ApexClass" -> 10, "ApexPage" -> 1
 * @param filePropsMapByXmlType - map that looks like so: "ApexClass" -> Map[String, FileProperties], "ApexPage" -> Map[String, FileProperties]
 *                              Map[String, FileProperties] is resolved like so: unpackaged/classes/MyClass.cls -> FileProperties
 */
class BulkRetrieveResult(val errors: List[Message], val fileCountByType: Map[String, Int],
                         val filePropsMapByXmlType: Map[String, Map[String, FileProperties]]) {

    private val filePropsByRelativePath = filePropsMapByXmlType.values.flatten.toMap
    /**
     *
     * @param relativeFilePath - e.g. src/classes/MyClass.cls
     * @return - FileProperties for file MyClass
     */
    def getFileProps(relativeFilePath: String): Option[FileProperties] = {

        val fPath = if (relativeFilePath.startsWith("src")) relativeFilePath.replaceFirst("src", "unpackaged") else relativeFilePath
        filePropsByRelativePath.get(fPath)
    }
}

/**
 * 'bulkRetrieve' action uses type list specified in a file and sends retrieve() call for each type
 * Extra command line params:
 * --targetFolder=/path/to/dir (optional), if specified then extract will be done in this folder
 * --specificTypes=/path/to/file.js with extraction details
 * --typesFileFormat=json|file-paths|package.xml, default is 'json'
 *  if format is set to "file-paths" then types list should look like so:
 *  -------------------------------
 *  objects/Account.object
 *  classes/MyClass.cls
 *  classes/A_Class.cls
 *  -------------------------------
 *  if format is set to "packageXml" then --specificTypes parameter must contain path to package.xml
 *
 * --updatePackageXMLOnSuccess=true|false (defaults to false) - if true then update package.xml to add missing types (if any)
 * --updateSessionDataOnSuccess=true|false (defaults to false) - if true then update session data if deployment is successful
 */
class BulkRetrieve extends RetrieveMetadata with JsonSupport {

    override def getHelp: ActionHelp = new ActionHelp {
        override def getExample: String =
            """Suppose we want to retrieve all members of ApexClass, ApprovalProcess, ApexComponent
              |and selected members of ApexPage type.
              |We can create a file (/home/test/types.js) with following content
              |---------------------------
              |{"XMLName": "ApexClass", "members": ["*"]}
              |{"XMLName": "ApprovalProcess", "members": ["*"]}
              |{"XMLName": "ApexComponent", "members": ["*"]}
              |{"XMLName": "ApexPage", "members": ["AccountEdit", "ContactEdit"]}
              |---------------------------
              | and add to the command line:
              |--specificTypes=/home/test/types.js
            """.stripMargin

        override def

        getParamDescription(paramName: String): String = {
            paramName match {
                case
                    "specificTypes" =>
                    """---specificTypes=/path/to/file
                      |full path to file with the list of metadata types to retrieve
                      |each metadata type must be on its own line in the file (unless --typesFileFormat=packageXml)
                    """.stripMargin
                case
                    "updatePackageXMLOnSuccess" =>
                    """--updatePackageXMLOnSuccess=true|false
                      |if --updatePackageXMLOnSuccess=true then package.xml will be updated when types file contains
                      |types missing in package.xml
                    """.stripMargin
                case
                    "typesFileFormat" =>
                    """format of the file with components list, can be either 'json' (default) or 'file-paths'
                      |if format is set to "file-paths" then types list should look like so:
                      |-------------------------------
                      |objects/My_Object__c
                      |classes/MyClass.cls
                      |classes/A_Class.cls
                      |-------------------------------
                      |
                      |if format is not set or set to "json" then types list should look like so:
                      |---------------------------
                      |{"XMLName": "ApexClass", "members": ["*"]}
                      |{"XMLName": "ApprovalProcess", "members": ["*"]}
                      |{"XMLName": "ApexComponent", "members": ["*"]}
                      |{"XMLName": "ApexPage", "members": ["AccountEdit", "ContactEdit"]}
                      |---------------------------
                      |
                      |if format is set to "packageXml" then --specificTypes parameter must contain path to package.xml
                    """.stripMargin
                case "targetFolder" =>
                    "/path/to/dir (optional), if specified then retrieved files will be saved in this folder"

                case "updateSessionDataOnSuccess" => "--updateSessionDataOnSuccess=true|false (defaults to false) - if true then update session data if deployment is successful"
                case _ => ""
            }
        }

        override def getParamNames: List[String] = List(
            "specificTypes", "updatePackageXMLOnSuccess", "typesFileFormat", "targetFolder", "updateSessionDataOnSuccess")

        override def getSummary: String =
            """using type list specified in a given file send retrieve() call for each type
              |and load retrieve results into a temporary folder
            """.
                stripMargin

        override def getName: String =  "bulkRetrieve"
    }

    //some types require special treatment and need other object types included in the package in order to return meaningful result
    private val complexTypes = Map("Profile" -> Set("CustomObject"), "PermissionSet" -> Set("CustomObject"))
    /**
     * retrieve single type and its members
     * @param metadataTypeName, e.g. ApexClass
     */
    def retrieveOne(metadataTypeName: String, membersByXmlName: Map[String, List[String]]): RetrieveResult = {
        logger.info("retrieve: " + metadataTypeName)
        val members = membersByXmlName(metadataTypeName)
        val retrieveRequest = new RetrieveRequest()
        retrieveRequest.setApiVersion(config.apiVersion)
        //retrieveRequest.setSinglePackage(true) //do NOT use setSinglePackage(), it causes fileNames to lose "unpackaged/"

        val metaXml = new MetaXml(session.getConfig)
        val typesMap = complexTypes.get(metadataTypeName)  match {
            case Some(extraTypeNames) =>
                //special treatment for types like Profile, PermissionSet
                val resMap = mutable.HashMap(metadataTypeName -> members)
                //add specified members of extra type, e.g. if main type is Profile then secondary type = CustomObject
                for(secondaryTypeName <- extraTypeNames) {
                    val extraMembers = membersByXmlName.get(secondaryTypeName) match {
                        case Some(_members) => _members
                        case None => List("*") //user did not specify members of secondary type, assume all
                    }
                    resMap += secondaryTypeName -> extraMembers
                }
                resMap.toMap
            case None => Map(metadataTypeName -> members)
        }
        val unpackagedManifest = metaXml.createPackage(config.apiVersion, typesMap)

        retrieveRequest.setUnpackaged(unpackagedManifest)

        val retrieveResult = session.retrieve(retrieveRequest)
        retrieveResult.getMessages match {
            case messages if null != messages && messages.nonEmpty=>
                //check if there are errors
                throw RetrieveError(retrieveResult)
            case _ =>
        }
        retrieveResult
    }

    /**
     * list locally modified files using data from session.properties
     */
    def getComponentPaths: List[String] = {

        //load file list from specified file
        val componentListFile = new File(config.getRequiredProperty("specificTypes").get)
        val components:List[String] = FileUtils.readFile(componentListFile).getLines().filter(!_.trim.isEmpty).toList
        components.map(FileUtils.normalizePath(_))
    }

    protected def isUpdateSessionDataOnSuccess: Boolean = config.getProperty("updateSessionDataOnSuccess").getOrElse("false").toBoolean
    protected def getTypesFileFormat: String = config.getProperty("typesFileFormat").getOrElse("json")
    protected def getSpecificTypesFile: File = new File(config.getRequiredProperty("specificTypes").get)

    protected def act()(implicit ec: ExecutionContext): Future[ActionResult] = {
        val tempFolder = config.getProperty("targetFolder")  match {
            case Some(x) => new File(x)
            case None =>
                FileUtils.createTempDir(config)
        }

        val bulkRetrieveResult = doRetrieve(tempFolder)
        val errors = bulkRetrieveResult.errors
        val fileCountByType = bulkRetrieveResult.fileCountByType

        if (errors.isEmpty) {
            //config.responseWriter.println("RESULT=SUCCESS")
            //config.responseWriter.println("RESULT_FOLDER=" + tempFolder.getAbsolutePath)
            //config.responseWriter.println("FILE_COUNT_BY_TYPE=" + fileCountByType.toJson.compactPrint)
            val actionResultBuilder = new ActionResultBuilder(SUCCESS)
            actionResultBuilder.addMessage(KeyValueMessage(Map("RESULT_FOLDER" -> tempFolder.getAbsolutePath)))
            actionResultBuilder.addMessage(KeyValueMessage(Map("FILE_COUNT_BY_TYPE" -> fileCountByType.toJson.compactPrint)))

            Future.successful(actionResultBuilder.result())
        } else {
            //config.responseWriter.println("RESULT=FAILURE")
            //errors.foreach(responseWriter.println(_))
            val actionResultBuilder = new ActionResultBuilder(FAILURE)
            actionResultBuilder.addMessages(errors)

            Future.successful(actionResultBuilder.result())
        }

    }


    def doRetrieve(tempFolder: File): BulkRetrieveResult = {


        //load file list from specified file
        val typesFile = getSpecificTypesFile
        val isPackageXmlFormat = getTypesFileFormat == "packageXml"
        val isJSONFormat = !isPackageXmlFormat && config.getProperty("typesFileFormat").getOrElse("json") == "json"

        val fileCountByType = Map.newBuilder[String, Int]
        val filePropsMapByXmlType = Map.newBuilder[String, Map[String, FileProperties]]
        var errors = List[Message]()
        val membersByXmlName = Map.newBuilder[String, List[String]]


        if (isPackageXmlFormat) {
            //get members list directly from provided package.xml
            membersByXmlName ++= MetaXml.getMembersByType(typesFile)

        } else if (isJSONFormat) {
            for (line <- FileUtils.readFile(typesFile).getLines().filter(!_.trim.isEmpty)) {
                Try(line.parseJson) match {
                    case Success(jsonAst) =>
                        val data = jsonAst.asJsObject.fields
                        val typeName = AnyJsonFormat.read(data.getOrElse("XMLName", JsString(""))).asInstanceOf[String]
                        val members = AnyJsonFormat.read(data.getOrElse("members", JsArray())).asInstanceOf[Vector[String]].toList

                        if (typeName.nonEmpty) {
                            membersByXmlName += typeName -> members
                        }
                        membersByXmlName
                    case Failure(e) =>
                        errors ::= ErrorMessage("failed to parse line: '" + line + "'. Make sure you specified correct '--typesFileFormat' value")
                }
            }
        } else {
            //folder/file format
            val metadataByDirName = DescribeMetadata.getDescribeByDirNameMap(session)
            val componentsPaths = getComponentPaths

            //classes -> List("MyClass.cls", "OtherClass.cls", ...), pages -> List("Page1.page")
            val namesByDir = componentsPaths.groupBy(_.takeWhile(_ != '/'))
            for (dirName <- namesByDir.keySet) {
                metadataByDirName.get(dirName) match {
                    case Some(describeObject) =>

                        var extension = describeObject.getSuffix
                        if (null == extension) {
                            extension = ""
                        } else {
                            extension = "." + extension
                        }
                        namesByDir.getOrElse(dirName, Nil) match {
                            case _fileNames if _fileNames.nonEmpty && Nil != _fileNames =>

                                val objNames = _fileNames.map(_.drop(dirName.length + 1)).map(
                                    name => if (name.endsWith(extension)) name.dropRight(extension.length) else name
                                )
                                membersByXmlName += describeObject.getXmlName -> objNames

                            case _ =>
                                throw new ActionError("Did not recognise values in directory: " + dirName)
                        }
                    case None =>
                }
            }
        }

        if (errors.isEmpty) {
            val membersByXmlNameMap = membersByXmlName.result()


            for (typeName <- membersByXmlNameMap.keySet) {
                Try(retrieveOne(typeName, membersByXmlNameMap)) match {
                    case Success(retrieveResult) =>
                        val filePropsMap = updateFromRetrieve(retrieveResult, tempFolder, updateSessionData = isUpdateSessionDataOnSuccess)
                        val fileCount = filePropsMap.values.count(props => !props.getFullName.endsWith("-meta.xml") && !props.getFullName.endsWith("package.xml"))
                        fileCountByType += typeName -> fileCount
                        filePropsMapByXmlType += typeName -> filePropsMap
                    case Failure(err) =>
                        err match {
                            case e: RetrieveError =>
                                e.retrieveResult.getMessages match {
                                    case messages if null != messages && messages.nonEmpty=>
                                        for(msg <- messages) {
                                            errors ::= ErrorMessage(msg.getFileName + ": " + msg.getProblem)
                                        }
                                    case _ =>
                                }
                            case _ => throw err
                        }
                }

            }
        }
        new BulkRetrieveResult(errors, fileCountByType.result(), filePropsMapByXmlType.result())
    }
}


/**
 * this is a helper class which serves as a container of information collected by DiffWithRemote command
 * @param remoteSrcFolderPath - path to src/ folder where remote version of current project is saved/dumped by BulkRetrieve
 */
class DiffWithRemoteReport(val bulkRetrieveResult: BulkRetrieveResult, val remoteSrcFolderPath: String) {
    private var _hasSomethingToReport = false
    private val localFilesMissingOnRemoteBuilder = Map.newBuilder[String, File]
    private val remoteFilesMissingLocallyBuilder = Map.newBuilder[String, File]
    private val conflictingFileMapBuilder = Map.newBuilder[String, ConflictingFile]

    def addLocalFileMissingOnRemote(relativePath: String, file: File): Unit = {
        localFilesMissingOnRemoteBuilder += relativePath -> file
        _hasSomethingToReport = true
    }
    def addRemoteFileMissingLocally(relativePath: String, file: File, prop: FileProperties): Unit = {
        remoteFilesMissingLocallyBuilder += relativePath -> file
        _hasSomethingToReport = true
    }

    class ConflictingFile(val fileLocal: File, val fileRemote: File, val remoteProp: FileProperties)

    def addConflictingFiles(relativePath: String, fileLocal: File, fileRemote: File, remoteProp: FileProperties): Unit = {
        conflictingFileMapBuilder += relativePath -> new ConflictingFile(fileLocal, fileRemote, remoteProp)
        _hasSomethingToReport = true
    }

    def hasSomethingToReport: Boolean = _hasSomethingToReport

    def getLocalFilesMissingOnRemote: Map[String, File] = localFilesMissingOnRemoteBuilder.result()

    def getRemoteFilesMissingLocally: Map[String, File] = remoteFilesMissingLocallyBuilder.result()

    def getConflictingFiles = conflictingFileMapBuilder.result()
}
/**
 * 'diffWithRemote' action - using package.xml or list of specific files extract files from SFDC and list
 * - files with different size
 * - files missing locally
 * - files missing on remote (but present locally)
 * Extra command line params:
 * --projectPath=/full/path/to/project/folder - the one which contains /src subfolder in it
 * --responseFilePath=full path to file where result of the operation will be documented.
 * --targetFolder=/path/to/dir (optional), if specified then extract will be done in this folder
 * --specificTypes=/path/to/file with details about resources to extract - (optional), if not provided then assume local project's package.xml
 * --typesFileFormat=json|file-paths|package.xml, default is 'json'
 *  if format is set to "file-paths" then types list should look like so:
 *  -------------------------------
 *  objects/Account.object
 *  classes/MyClass.cls
 *  classes/A_Class.cls
 *  -------------------------------
 */
class DiffWithRemote extends RetrieveMetadata {

    override def isSessionReadOnly: Boolean = false

    private val superActionHelp = new BulkRetrieve().getHelp
    override def getHelp: ActionHelp = new AbstractActionHelp(superActionHelp) {
        override def getExample: String =
            """
              |In order to get list of differences between local project and remote SFDC Org 'diffWithRemote' can be called like so
              |... --action=diffWithRemote --projectPath=/home/users/tester/MyApexProject  --responseFilePath=/path/to/response.txt
              |--specificTypes=/temp/file-paths.txt --typesFileFormat=file-paths
            """.stripMargin

        override def getParamDescription(paramName: String): String = {
            paramName match {
                case "config" => "--config - full path to config.properties file"
                case "projectPath" => "--projectPath - full path to folder which contains ./src/ of apex project."
                case "responseFilePath" => "--responseFilePath - full path to file where result of the operation will be documented."
                case _ => superActionHelp.getParamDescription(paramName)
            }
        }

        override def getParamNames: List[String] = List("config", "projectPath", "responseFilePath", "targetFolder", "specificTypes", "typesFileFormat")

        override def getSummary: String = "return differences between local and remote file sets using metadata types specified in local package.xml or file specified by --specificTypes parameter"

        override def getName: String = "diffWithRemote"
    }

    protected def act()(implicit ec: ExecutionContext): Future[ActionResult] = {
        val actionResultBuilder = new ActionResultBuilder(SUCCESS)
        getDiffReport(actionResultBuilder) match {
          case Some(diffReport) =>
              //responseWriter.println("RESULT=SUCCESS")
              writeReportToResponseFile(diffReport, actionResultBuilder)
          case None =>
        }
        Future.successful(actionResultBuilder.result())

    }

    /**
     * path to folder where remote version of current project will be saved
     * @return - Option(/path/to/folder)
     */
    def getTargetFolder: Option[String] = config.getProperty("targetFolder")

    def getDiffReport(actionResultBuilder: ActionResultBuilder): Option[DiffWithRemoteReport] = {
        val tempFolder = getTargetFolder match {
            case Some(x) => new File(x)
            case None => FileUtils.createTempDir(config)
        }
        val bulkRetrieve = new BulkRetrieve {
            override protected def isUpdateSessionDataOnSuccess: Boolean = false

            override protected def getSpecificTypesFile: File = {
                super.getTypesFileFormat match {
                    case "packageXml" =>
                        val metaXml = new MetaXml(session.getConfig)
                        val packageXmlFile = metaXml.getPackageXml
                        packageXmlFile
                    case _ => super.getSpecificTypesFile
                }
            }
        }
        bulkRetrieve.load[BulkRetrieve](session)

        val bulkRetrieveResult = bulkRetrieve.doRetrieve(tempFolder)
        processRetrieveResult(tempFolder, bulkRetrieveResult, actionResultBuilder)
    }

    protected def processRetrieveResult(tempFolder: File, bulkRetrieveResult: BulkRetrieveResult, actionResultBuilder: ActionResultBuilder): Option[DiffWithRemoteReport] = {
        val errors = bulkRetrieveResult.errors

        if (errors.isEmpty) {
            //by default retrieve result is unpacked in .../unpackaged/... folder
            val remoteProjectDir = new File(tempFolder, "unpackaged")
            //rename destination folder to be .../src/... instead of .../unpackaged/...
            val destinationSrcFolder = new File(tempFolder, "src")
            if (destinationSrcFolder.exists()) {
                FileUtils.delete(destinationSrcFolder)
            }
            if (remoteProjectDir.renameTo(new File(tempFolder, "src"))) {
                val report = generateDiffReport(bulkRetrieveResult, destinationSrcFolder.getAbsolutePath)
                Some(report)

            } else {
                //failed to rename unpackaged/ into src/
                //responseWriter.println("RESULT=FAILURE")
                //responseWriter.println(new Message(ERROR,
                //    s"Failed to rename $remoteProjectDir + into $destinationSrcFolder"))
                actionResultBuilder.setResultType(FAILURE)
                actionResultBuilder.addMessage(ErrorMessage(s"Failed to rename $remoteProjectDir + into $destinationSrcFolder"))
                None
            }
        } else {
            //config.responseWriter.println("RESULT=FAILURE")
            //errors.foreach(responseWriter.println(_))
            actionResultBuilder.setResultType(FAILURE)
            actionResultBuilder.addMessages(errors)
            None
        }
    }

    private def getFilePaths(srcDir: File): List[File] = {
        config.getRequiredProperty("typesFileFormat").get match {
            case "packageXml" =>
                FileUtils.listFiles(srcDir)
            case "file-paths" =>
                FileUtils.readFile(new File(config.getRequiredProperty("specificTypes").get)).
                                                getLines().filter(!_.trim.isEmpty).
                                                map(new File(srcDir, _)).toList
            case x => throw new IllegalArgumentException("support for --typesFileFormat=" + x + " is not implemented")
        }
    }
    protected def getLocalFiles: List[File] = {
        getSessionConfig.srcDirOpt match {
            case Some(srcDir) => getFilePaths(srcDir)
            case None => Nil
        }
    }

    protected def getRemoteFiles(remoteSrcFolder: File): List[File] = getFilePaths(remoteSrcFolder)
    /**
     *
     * @param bulkRetrieveResult - result of
     * @param remoteSrcFolderPath - src/ folder where results of retrieve command dump were moved
     *                            by default retrieve dumps stuff in .../unpackaged/ rather than .../src/
     */
    def generateDiffReport(bulkRetrieveResult: BulkRetrieveResult, remoteSrcFolderPath: String): DiffWithRemoteReport = {
        val report = new DiffWithRemoteReport(bulkRetrieveResult, remoteSrcFolderPath)

        //local files
        val existingFileByRelativePath  = getLocalFiles.filter(
            //remove all non apex files
            file => DescribeMetadata.isValidApexFile(session, file) && "package.xml" != file.getName
        ).map(file => (session.getRelativePath(file), file) ).toMap

        //remote files
        val remoteSrcFolder = new File(remoteSrcFolderPath)
        val remoteFiles = getRemoteFiles(remoteSrcFolder).filter(
            //remove all non apex files
            file => DescribeMetadata.isValidApexFile(session, file) && "package.xml" != file.getName
        )

        val remoteFilesByRelativePaths = remoteFiles.map(file => (
                FileUtils.normalizePath(file.getAbsolutePath).replaceAllLiterally(remoteSrcFolder.getParentFile.getAbsolutePath + "/", ""), file
            )).toMap

        //list files where remote version has different size or crc32 compared to local version
        //Modified Files
        for(relPath <- existingFileByRelativePath.keys.toList.sortWith( (left, right) => left.compareTo(right) < 0)) {
            bulkRetrieveResult.getFileProps(relPath) match {
              case Some(props) =>
                  //val key = props.getFileName
                  val localFileOpt = existingFileByRelativePath.get(relPath)
                  val remoteFileOpt = remoteFilesByRelativePaths.get(relPath)
                  if (localFileOpt.isDefined && remoteFileOpt.isDefined) {
                      val localFile = localFileOpt.get
                      val remoteFile = remoteFileOpt.get
                      val sizeLocal = localFile.length()
                      val sizeRemote = remoteFile.length()
                      if (sizeLocal != sizeRemote || FileUtils.getCRC32Hash(localFile) != FileUtils.getCRC32Hash(remoteFile)) {
                          report.addConflictingFiles(relPath, localFile, remoteFile, props)
                      }
                  }
              case None =>
            }
        }


        //list files that exist on locally but do not exist on remote
        for(relPath <- existingFileByRelativePath.keys) {
            if (!remoteFilesByRelativePaths.contains(relPath)) {
                report.addLocalFileMissingOnRemote(relPath, existingFileByRelativePath(relPath))
            }
        }

        //list files that exist on remote but do not exist locally
        for(relPath <- remoteFilesByRelativePaths.keys.toList.sortWith( (left, right) => left.compareTo(right) < 0)) {
            if (!existingFileByRelativePath.contains(relPath)) {
                bulkRetrieveResult.getFileProps(relPath) match {
                    case Some(props) =>
                        report.addRemoteFileMissingLocally(relPath, remoteFilesByRelativePaths(relPath), props)
                    case None =>
                }
            }
        }

        report
    }

    protected def writeReportToResponseFile(report: DiffWithRemoteReport, actionResultBuilder: ActionResultBuilder): Unit = {
        val bulkRetrieveResult: BulkRetrieveResult = report.bulkRetrieveResult
        val remoteSrcFolderPath: String = report.remoteSrcFolderPath

        //responseWriter.println("REMOTE_SRC_FOLDER_PATH=" + remoteSrcFolderPath)
        actionResultBuilder.addMessage(KeyValueMessage(Map("REMOTE_SRC_FOLDER_PATH" -> remoteSrcFolderPath)))
        //responseWriter.println(new Message(INFO, "Remote version is saved in: " + remoteSrcFolderPath))
        actionResultBuilder.addMessage(InfoMessage("Remote version is saved in: " + remoteSrcFolderPath))

        val conflictingFilesMap = report.getConflictingFiles
        val localFilesMissingOnRemoteMap = report.getLocalFilesMissingOnRemote
        val remoteFilesMissingLocallyMap = report.getRemoteFilesMissingLocally

        if (report.hasSomethingToReport) {

            if (conflictingFilesMap.nonEmpty) {
                //list files where remote version has different size compared to local version
                //Modified Files
                val msg = WarnMessage("Different file sizes")
                //responseWriter.println(msg)
                actionResultBuilder.addMessage(msg)

                for (relativePath <- conflictingFilesMap.keys.toList.sortWith((left, right) => left.compareTo(right) < 0)) {
                    conflictingFilesMap.get(relativePath) match {
                        case Some(conflictingFile) =>
                            val props = conflictingFile.remoteProp
                            val sizeLocal = conflictingFile.fileLocal.length()
                            val sizeRemote = conflictingFile.fileRemote.length()
                            val text = conflictingFile.fileLocal.getName +
                                " => Modified By: " + props.getLastModifiedByName +
                                "; at: " + ZuluTime.formatDateGMT(props.getLastModifiedDate) +
                                s"; Local size: $sizeLocal; remote size: $sizeRemote"
                            //responseWriter.println(MessageDetail(msg, Map("filePath" -> relativePath, "text" -> text)))
                            actionResultBuilder.addDetail(MessageDetailMap(msg, Map("filePath" -> relativePath, "text" -> text)))
                        case None =>
                    }
                }
            }

            if (localFilesMissingOnRemoteMap.nonEmpty) {
                //list files that exist on locally but do not exist on remote
                val msg = WarnMessage("Files exist locally but not on remote (based on current package.xml)")
                //responseWriter.println(msg)
                actionResultBuilder.addMessage(msg)

                for(relativePath <- localFilesMissingOnRemoteMap.keys) {
                    localFilesMissingOnRemoteMap.get(relativePath) match {
                      case Some(localFile) =>
                          val text = localFile.getName +
                              " => exists locally but missing on remote"
                          val echoText = localFile.getName
                          //responseWriter.println(MessageDetail(msg, Map("filePath" -> relativePath, "text" -> text, "echoText" -> echoText)))
                          actionResultBuilder.addDetail(MessageDetailMap(msg, Map("filePath" -> relativePath, "text" -> text, "echoText" -> echoText)))
                      case None =>
                    }
                }
            }

            if (remoteFilesMissingLocallyMap.nonEmpty) {
                //list files that exist on remote but do not exist locally
                val msg = WarnMessage("Files exist on remote but not locally (based on current package.xml)")
                //responseWriter.println(msg)
                actionResultBuilder.addMessage(msg)

                for(relativePath <- remoteFilesMissingLocallyMap.keys.toList.sortWith( (left, right) => left.compareTo(right) < 0)) {
                    remoteFilesMissingLocallyMap.get(relativePath) match {
                      case Some(remoteFile) =>
                          bulkRetrieveResult.getFileProps(relativePath) match {
                              case Some(props) =>
                                  val sizeRemote = remoteFile.length()
                                  val text = remoteFile.getName +
                                      " => Modified By: " + props.getLastModifiedByName +
                                      "; at: " + ZuluTime.formatDateGMT(props.getLastModifiedDate) +
                                      s"; remote size: $sizeRemote"
                                  //responseWriter.println(MessageDetail(msg, Map("filePath" -> relativePath, "text" -> text)))
                                  actionResultBuilder.addDetail(MessageDetailMap(msg, Map("filePath" -> relativePath, "text" -> text)))
                              case None =>
                          }
                      case None =>
                    }
                }
            }
        } else {
            //responseWriter.println(new Message(INFO, "No differences between local version and remote Org detected"))
            actionResultBuilder.addMessage(InfoMessage("No differences between local version and remote Org detected"))
        }


    }
}
