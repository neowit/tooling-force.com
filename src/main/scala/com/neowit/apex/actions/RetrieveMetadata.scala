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

package com.neowit.apex.actions

import com.neowit.apex.actions.tooling.BundleMember

import java.io.{File, FileOutputStream, PrintWriter, StringWriter}
import com.neowit.apex.{MetaXml, MetadataType}
import com.neowit.utils._
import com.sforce.soap.metadata.{DescribeMetadataObject, FileProperties, RetrieveMessage, RetrieveRequest, RetrieveResult}

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
                //val (lastModifiedLocally, md5Hash, crc32Hash) = localDateAndHashByFName(fileProp.getFileName)
                localDateAndHashByFName.get(fileProp.getFileName) match {
                  case Some((lastModifiedLocally, md5Hash, crc32Hash)) =>
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
                  case None =>
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
            if (!this.isSessionReadOnly) {
                session.storeSessionData()
            }
            resultsFile.delete()
        }
        propertyByFilePath.toMap
    }
}

/**
 * 'refresh' action is 'retrieve' for all elements specified in package.xml into project folder
 */
case class UpdateFromRetrieveResult(resultFolder: File, fileCount: Int)
class RefreshMetadata extends RetrieveMetadata {

    override def getHelp: ActionHelp = new ActionHelp {
        override def getParamDescription(paramName: String): String = paramName match {
            case "config" => "--config - full path to config.properties file"
            case "projectPath" => "--projectPath - full path to folder which contains ./src/ of apex project."
            case "responseFilePath" => "--responseFilePath - full path to file where result of the operation will be documented."
            case "skipModifiedFilesCheck" => "--skipModifiedFilesCheck - unsafe retrieval, do not check for modified files before retrieval."
            case "modifiedFilesResultCode" => "--modifiedFilesResultCode - If provided (and project has modified files) then instead of normal 'RESULT=FAILURE' 'RESULT=' will be reported with given code."
            case "packageName" => "--packageName - if provided then will retrieve metadata from the specified package rather then 'unpackaged'."
        }

        override def getParamNames: List[String] = List("config", "projectPath", "responseFilePath", "skipModifiedFilesCheck", "modifiedFilesResultCode", "packageName")

        override def getExample: String = ""

        override def getSummary: String = "allows to 'retrieve' all elements specified in package.xml or in a package with the specified name"

        override def getName: String = "refresh"
    }

    protected def act()(implicit ec: ExecutionContext): Future[ActionResult] = {
        val actionResult =
            try {
                //first check if we have modified files
                val skipModifiedFilesCheck = config.getProperty("skipModifiedFilesCheck").getOrElse("false").toBoolean
                val modifiedFileChecker = new ListModified().load[ListModified](session)
                val modifiedFiles = if (skipModifiedFilesCheck) Nil else modifiedFileChecker.getModifiedFiles
                val packageNameOpt = config.getProperty("packageName").orElse(session.getData("package").get("name").map(_.toString))

                if (modifiedFiles.isEmpty) {
                    val retrieveRequest = new RetrieveRequest()
                    retrieveRequest.setApiVersion(config.apiVersion)
                    packageNameOpt match {
                      case Some(packageName) =>
                        retrieveRequest.setPackageNames(Array(packageName))
                        //retrieveRequest.setSinglePackage(true)
                      case None =>
                          setUpackaged(retrieveRequest)
                    }
                    Try(session.retrieve(retrieveRequest)) match {
                        case Success(retrieveResult) =>
                            val result = updateFromRetrieve(retrieveResult, packageNameOpt)
                            ActionSuccess(RefreshMetadataResult(retrieveResult = Option(result), modifiedFiles = Nil))
                        case Failure(err) =>
                            val errorListBuilder = List.newBuilder[ErrorMessage]
                            err match {
                                case e: RetrieveError =>
                                    err.asInstanceOf[RetrieveError].retrieveResult.getMessages match {
                                        case messages if null != messages && messages.nonEmpty=>
                                            //responseWriter.println("RESULT=FAILURE")
                                            for(msg <- messages) {
                                                //responseWriter.println(msg.getFileName + ": " + msg.getProblem)
                                                errorListBuilder += ErrorMessage(msg.getFileName + ": " + msg.getProblem)
                                            }
                                        case _ =>
                                    }
                                case _ =>
                                    throw err
                            }
                            ActionFailure(errorListBuilder.result())
                    }
                } else {
                    //responseWriter.println("RESULT=FAILURE")
                    // in some cases reporting RESULT=FAILURE (if modified files detected) may not be desirable
                    // check if user requested alternative result code
                    //responseWriter.println("RESULT=" + modifiedFilesResultCode)
                    //responseWriter.println(new Message(ResponseWriter.DEBUG, "Use --skipModifiedFilesCheck=true command line option to force Refresh"))

                    //modifiedFileChecker.reportModifiedFiles(modifiedFiles, WARN, actionResultBuilder)
                    config.getProperty("modifiedFilesResultCode") match {
                        case Some("SUCCESS") =>
                            ActionSuccess(RefreshMetadataResult(retrieveResult = None, modifiedFiles))
                        case _ =>
                            ActionFailure(DebugMessage("Use --skipModifiedFilesCheck=true command line option to force Refresh"))
                    }
                }
            } catch {
                case ex:Throwable =>
                    val sw = new StringWriter
                    ex.printStackTrace(new PrintWriter(sw))
                    val stackTraceStr = sw.toString
                    // dump exception information to log
                    logger.error("", ex)
                    logger.error(stackTraceStr)
                    //responseWriter.println("RESULT=FAILURE")
                    //responseWriter.println(new Message(ERROR, ex.toString))
                    ActionFailure(ErrorMessage(ex.toString))
            }
        Future.successful(actionResult)
    }

    /**
     * using ZIP file produced, for example, as a result of Retrieve operation
     * extract content and generate response file
     */
    def updateFromRetrieve(retrieveResult: com.sforce.soap.metadata.RetrieveResult, packageNameOpt: Option[String]): UpdateFromRetrieveResult = {
        val tempFolder = FileUtils.createTempDir(config/*, packageNameOpt*/)
        val filePropsMap = updateFromRetrieve(retrieveResult, tempFolder)
        //clear Ids of all files not loaded from the Org
        session.resetData(filePropsMap.keySet)

        //config.responseWriter.println("RESULT=SUCCESS")
        //config.responseWriter.println("RESULT_FOLDER=" + tempFolder.getAbsolutePath)
        //config.responseWriter.println("FILE_COUNT=" + filePropsMap.values.count(props => !props.getFullName.endsWith("-meta.xml") && props.getFullName != "package.xml"))
        val fileCount = filePropsMap.values.count(props => !props.getFullName.endsWith("-meta.xml") && props.getFullName != "package.xml")
        UpdateFromRetrieveResult(resultFolder = tempFolder, fileCount = fileCount)
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
     * @return
     */
    def generateConflictMessageDetails(conflictingFiles: List[Map[String, Any]]):DeploymentConflictsReport = {
        val conflicts =
            conflictingFiles.flatMap { prop =>
                prop.get("file").map { x =>
                    val f = x.asInstanceOf[File]
                    SingleFileConflictDetail(
                        file = f,
                        lastModifiedByName = prop.get("LastModifiedByName").map(_.toString),
                        lastModifiedById = prop.get("LastModifiedById").map(_.toString),
                        remoteLastModifiedDate = prop.get("Remote-LastModifiedDateStr").map(_.toString),
                        localLastModifiedDate = prop.get("Local-LastModifiedDateStr").map(_.toString)
                    )
                }
            }
        DeploymentConflictsReport(conflicts.nonEmpty, conflicts)
    }

    protected def act()(implicit ec: ExecutionContext): Future[ActionResult] = {
        val checker = new ListModified().load[ListModified](session)
        val modifiedFiles = checker.getModifiedFiles


        getFilesNewerOnRemote(modifiedFiles) match {
            case Some(fileProps) =>
                //config.responseWriter.println("RESULT=SUCCESS")
                val conflictReport =
                if (fileProps.nonEmpty) {
                    //val msg = InfoMessage("Outdated file(s) detected.")
                    //config.responseWriter.println(msg)
                    //generateConflictMessageDetails(fileProps, msg).foreach{detail => config.responseWriter.println(detail)}
                    generateConflictMessageDetails(fileProps)
                } else {
                    //config.responseWriter.println(new Message(INFO, "No outdated files detected."))
                    DeploymentConflictsReport(hasConflicts = false)
                }
                //fileProps.isEmpty
                Future.successful(ActionSuccess(ListConflictingResult(conflictReport)))
            case None =>
                Future.successful(ActionSuccess(ListConflictingResult(DeploymentConflictsReport(hasConflicts = false))))
        }
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

    // lower case relativeFilePath => FileProperties
    private val filePropsByRelativePath = filePropsMapByXmlType.values.flatten.toMap.map(v => (v._1.toLowerCase, v._2))
    /**
     *
     * @param relativeFilePath - e.g. src/classes/MyClass.cls
     * @return - FileProperties for file MyClass
     */
    def getFileProps(relativeFilePath: String): Option[FileProperties] = {

        val fPath = if (relativeFilePath.startsWith("src")) relativeFilePath.replaceFirst("src", "unpackaged") else relativeFilePath
        filePropsByRelativePath.get(fPath.toLowerCase)
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
    private val complexTypes = Map(
        "Profile" -> Set("CustomObject", "CustomTab"),
        "PermissionSet" -> Set("CustomObject", "CustomTab")
    )
    /**
     * retrieve single type and its members
     * @param metadataTypeName, e.g. ApexClass
     */
    def retrieveOne(metadataTypeName: String, membersByXmlName: Map[String, List[String]]): RetrieveResult = {
        logger.info("retrieve: " + metadataTypeName)
        val packageNameOpt = config.getProperty("packageName").orElse(session.getData("package").get("name").map(_.toString))

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

        packageNameOpt match {
            case Some(packageName) if "unpackaged" != packageName =>
                retrieveRequest.setPackageNames(Array(packageName))
            case _ =>
                val unpackagedManifest = metaXml.createPackage(config.apiVersion, typesMap)
                retrieveRequest.setUnpackaged(unpackagedManifest)
        }


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
        val componentListFile = new File(config.getRequiredProperty("specificTypes"))
        val components:List[String] = FileUtils.readFile(componentListFile).getLines().filter(_.trim.nonEmpty).toList
        components.map(FileUtils.normalizePath)
    }

    protected def isUpdateSessionDataOnSuccess: Boolean = config.getProperty("updateSessionDataOnSuccess").getOrElse("false").toBoolean
    protected def getTypesFileFormat: String = config.getProperty("typesFileFormat").getOrElse("json")
    protected def getSpecificTypesFile: File = new File(config.getRequiredProperty("specificTypes"))

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
            Future.successful(ActionSuccess(BulkRetrieveActionResult(resultFolder = tempFolder, fileCountByType = fileCountByType, errors)))
        } else {
            //config.responseWriter.println("RESULT=FAILURE")
            //errors.foreach(responseWriter.println(_))

            Future.successful(ActionFailure(BulkRetrieveActionResult(resultFolder = tempFolder, fileCountByType = fileCountByType, errors)))
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
            for (line <- FileUtils.readFile(typesFile).getLines().filter(_.trim.nonEmpty)) {
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

                        namesByDir.getOrElse(dirName, Nil) match {
                            case _fileNames if _fileNames.nonEmpty && Nil != _fileNames =>

                                val objNames = _fileNames.map (
                                    path => formatObjectNameForRetrieve(describeObject, path )

                                ).map(_.drop(dirName.length + 1))
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

    /**
     *
     * @param describeObject result of metadata describe call
     * @param path file path, e.g. class/MyClass.cls or aura/MyBundleName/MyBundleName.cmp
     * @return
     *  for bundled types: just bundle name, e.g. "MyBundleName"
     *  for other types: just file name without extension, e.g. "MyClass"
     */
    private def formatObjectNameForRetrieve( describeObject: DescribeMetadataObject, path: String): String = {
        var extension = describeObject.getSuffix
        if (null == extension) {
            extension = ""
        } else {
            extension = "." + extension
        }

        val fileOrBundleName: String =
            BundleMember.getBundleMemberHelper(path) match {
                case Some(/*helper*/_) =>
                    //keep only bundle names for aura and LWC files
                    var pathParts: Array[String] = FileUtils.normalizePath(path).split(FileUtils.NORMAL_SLASH)
                    if (pathParts.length > 2){
                        // path contains not just 'aura/bundle-name' but 'aura/bundle-name/file-name.ext'
                        // then need to drop file-name.ext because this will not be recognised by metadata retrieve
                        pathParts = pathParts.take(2)
                    }
                    pathParts.mkString(FileUtils.NORMAL_SLASH)

                case None => path
            }

        // remove extension if necessary
        if (fileOrBundleName.endsWith(extension)) fileOrBundleName.dropRight(extension.length) else fileOrBundleName

    }
}


class ConflictingFile(val fileLocal: File, val fileRemote: File, val remoteProp: FileProperties)
trait DiffWithRemoteReport {
    def hasSomethingToReport: Boolean

    def getLocalFilesMissingOnRemote: Map[String, File]

    def getRemoteFilesMissingLocally: Map[String, File]

    def getConflictingFiles: Map[String, ConflictingFile]
}

case class DiffWithRemoteReportFailure(errors: List[Message]) extends DiffWithRemoteReport {
    override def hasSomethingToReport: Boolean = false

    override def getLocalFilesMissingOnRemote: Map[String, File] = Map.empty

    override def getRemoteFilesMissingLocally: Map[String, File] = Map.empty

    override def getConflictingFiles: Map[String, ConflictingFile] = Map.empty
}
/**
  * this is a helper class which serves as a container of information collected by DiffWithRemote command
  * @param remoteSrcFolderPath - path to src/ folder where remote version of current project is saved/dumped by BulkRetrieve
  */
case class DiffWithRemoteReportSuccess(bulkRetrieveResult: BulkRetrieveResult, remoteSrcFolderPath: String) extends DiffWithRemoteReport {
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


    def addConflictingFiles(relativePath: String, fileLocal: File, fileRemote: File, remoteProp: FileProperties): Unit = {
        conflictingFileMapBuilder += relativePath -> new ConflictingFile(fileLocal, fileRemote, remoteProp)
        _hasSomethingToReport = true
    }

    def hasSomethingToReport: Boolean = _hasSomethingToReport

    def getLocalFilesMissingOnRemote: Map[String, File] = localFilesMissingOnRemoteBuilder.result()

    def getRemoteFilesMissingLocally: Map[String, File] = remoteFilesMissingLocallyBuilder.result()

    def getConflictingFiles: Map[String, ConflictingFile] = conflictingFileMapBuilder.result()
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

    override def isSessionReadOnly: Boolean = true

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
        val actionResult =
            (getDiffReport: @unchecked) match {
                case report @ DiffWithRemoteReportSuccess(_, _) =>
                    //responseWriter.println("RESULT=SUCCESS")
                    //writeReportToResponseFile(diffReport, actionResultBuilder)
                    ActionSuccess(DiffWithRemoteResult(report))
                case report @ DiffWithRemoteReportFailure(_) =>
                    ActionFailure(DiffWithRemoteResult(report))
            }
        Future.successful(actionResult)

    }

    /**
     * path to folder where remote version of current project will be saved
     * @return - Option(/path/to/folder)
     */
    def getTargetFolder: Option[String] = config.getProperty("targetFolder")

    def getDiffReport: DiffWithRemoteReport = {
        val tempFolder = getTargetFolder match {
            case Some(x) => new File(x)
            case _ => FileUtils.createTempDir(config)
        }
        val bulkRetrieve = new BulkRetrieve {
            override protected def isUpdateSessionDataOnSuccess: Boolean = false
            override def isSessionReadOnly: Boolean = true

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
        processRetrieveResult(tempFolder, bulkRetrieveResult)
    }

    protected def processRetrieveResult(tempFolder: File, bulkRetrieveResult: BulkRetrieveResult): DiffWithRemoteReport = {
        val packageNameOpt = config.getProperty("packageName").orElse(session.getData("package").get("name").map(_.toString))

        val errors = bulkRetrieveResult.errors

        if (errors.isEmpty) {
            packageNameOpt  match {
              case Some(packageName) if "unpackaged" != packageName =>
                  val remoteProjectDir = new File(tempFolder, packageName)
                  val report = generateDiffReport(bulkRetrieveResult, remoteProjectDir.getAbsolutePath)
                  report
              case _ =>
                  //by default retrieve result is unpacked in .../unpackaged/... folder
                  val remoteProjectDir = new File(tempFolder, "unpackaged")
                  //rename destination folder to be .../src/... instead of .../unpackaged/...
                  val destinationSrcFolder = new File(tempFolder, "src")
                  if (destinationSrcFolder.exists()) {
                      FileUtils.delete(destinationSrcFolder)
                  }
                  if (remoteProjectDir.renameTo(new File(tempFolder, "src"))) {
                      val report = generateDiffReport(bulkRetrieveResult, destinationSrcFolder.getAbsolutePath)
                      report

                  } else {
                      //failed to rename unpackaged/ into src/
                      //responseWriter.println("RESULT=FAILURE")
                      //responseWriter.println(new Message(ERROR,
                      //    s"Failed to rename $remoteProjectDir + into $destinationSrcFolder"))
                      DiffWithRemoteReportFailure(List(ErrorMessage(s"Failed to rename '$remoteProjectDir' into '$destinationSrcFolder'")))
                  }
            }
        } else {
            //config.responseWriter.println("RESULT=FAILURE")
            //errors.foreach(responseWriter.println(_))
            DiffWithRemoteReportFailure(errors)
        }
    }

    private def getFilePaths(srcDir: File): List[File] = {
        config.getRequiredProperty("typesFileFormat") match {
            case "packageXml" =>
                FileUtils.listFiles(srcDir)
            case "file-paths" =>
                FileUtils.readFile(new File(config.getRequiredProperty("specificTypes"))).
                                                getLines().filter(_.trim.nonEmpty).
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
        val report = DiffWithRemoteReportSuccess(bulkRetrieveResult, remoteSrcFolderPath)

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
                FileUtils.normalizePath(file.getAbsolutePath.replace(remoteSrcFolder.getParentFile.getAbsolutePath + File.separator, "")), file
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

}
