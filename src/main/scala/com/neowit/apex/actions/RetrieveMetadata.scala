package com.neowit.apex.actions

import com.sforce.soap.metadata.{FileProperties, RetrieveMessage, RetrieveRequest, RetrieveResult}
import com.neowit.apex.{MetadataType, MetaXml, Session}
import java.io.{FileOutputStream, File}
import com.neowit.utils.{ResponseWriter, ZipUtils, FileUtils}
import scala.util.{Failure, Success, Try}
import com.neowit.utils.ResponseWriter.{MessageDetail, Message}
import scala.collection.mutable
import scala.util.parsing.json.{JSONArray, JSONObject, JSON}

case class RetrieveError(retrieveResult: RetrieveResult) extends Error

abstract class RetrieveMetadata(session: Session) extends ApexAction(session: Session) {

    def setUpackaged(retrieveRequest: RetrieveRequest) {
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
            case messages if null != messages && !messages.isEmpty=>
                //check if all errors we have are about missing files
                val messagesOtherThanMissingFiles = messages.filterNot(isMissingFileError(_))
                if (reportMissing || !messagesOtherThanMissingFiles.isEmpty) {
                    throw new RetrieveError(retrieveResult)
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
     * using ZIP file produced, for example, as a result of Retrieve operation
     * extract content and generate response file
     */
    def updateFromRetrieve(retrieveResult: com.sforce.soap.metadata.RetrieveResult, tempFolder: File): Map[String, FileProperties] = {

        //val outputPath = appConfig.srcDir.getParentFile.getAbsolutePath
        //extract in temp area first
        val resultsFile = FileUtils.createTempFile("retrieveResult", ".zip")
        val out = new FileOutputStream(resultsFile)
        try {
            out.write(retrieveResult.getZipFile)
        } finally {
            out.close()
        }
        val calculateMD5 = config.useMD5Hash
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
                session.setData(key, valueMap)

                propertyByFilePath.put(fileProp.getFileName, fileProp)
            }
        } finally {
            session.storeSessionData()
            resultsFile.delete()
        }
        propertyByFilePath.toMap
    }
}

/**
 * 'refresh' action is 'retrieve' for all elements specified in package.xml
 *@param session - SFDC session
 */
class RefreshMetadata(session: Session) extends RetrieveMetadata(session: Session) {


    override def getParamDescription(paramName: String): String = ""

    override def getParamNames: List[String] = Nil

    override def getExample: String = ""

    override def getSummary: String = "allows to 'retrieve' all elements specified in package.xml"

    override def getName: String = "refresh"

    def act() {
        //first check if we have modified files
        val skipModifiedFilesCheck = config.getProperty("skipModifiedFilesCheck").getOrElse("false").toBoolean
        val modifiedFileChecker = new ListModified(session)
        val modifiedFiles = if (skipModifiedFilesCheck) Nil else modifiedFileChecker.getModifiedFiles

        if (modifiedFiles.isEmpty) {
            val retrieveRequest = new RetrieveRequest()
            retrieveRequest.setApiVersion(config.apiVersion)
            setUpackaged(retrieveRequest)
            Try(session.retrieve(retrieveRequest)) match {
                case Success(retrieveResult) =>
                    updateFromRetrieve(retrieveResult)
                case Failure(err) =>
                    err match {
                        case e: RetrieveError =>
                            err.asInstanceOf[RetrieveError].retrieveResult.getMessages match {
                                case messages if null != messages && !messages.isEmpty=>
                                    responseWriter.println("RESULT=FAILURE")
                                    for(msg <- messages) {
                                        responseWriter.println(msg.getFileName + ": " + msg.getProblem)
                                    }
                                case _ =>
                            }
                        case _ => throw err
                    }
            }
        } else {
            responseWriter.println("RESULT=FAILURE")
            responseWriter.println(new Message(ResponseWriter.DEBUG,
                "Use --skipModifiedFilesCheck=true command line option to force Refresh"))
            modifiedFileChecker.reportModifiedFiles(modifiedFiles, ResponseWriter.WARN)
        }
    }
    /**
     * using ZIP file produced, for example, as a result of Retrieve operation
     * extract content and generate response file
     */
    def updateFromRetrieve(retrieveResult: com.sforce.soap.metadata.RetrieveResult) {
        val tempFolder = FileUtils.createTempDir(config)
        val filePropsMap = updateFromRetrieve(retrieveResult, tempFolder)

        config.responseWriter.println("RESULT=SUCCESS")
        config.responseWriter.println("RESULT_FOLDER=" + tempFolder.getAbsolutePath)
        config.responseWriter.println("FILE_COUNT=" + filePropsMap.values.filter(props => !props.getFullName.endsWith("-meta.xml") && props.getFullName != "package.xml").size)
    }
}
/**
 * check local modified files against their Remote versions to see if remote is newer
 */
class ListConflicting(session: Session) extends RetrieveMetadata(session: Session) {


    override def getExample: String = ""

    override def getParamDescription(paramName: String): String = ""

    override def getParamNames: List[String] = Nil

    override def getSummary: String =
        """ Check if there have been updates of remote (SFDC) versions of local files since last 'refresh' or 'deploy' operation
          | """

    override def getName: String = "listConflicts"

    def getFilesNewerOnRemote(files: List[File]): Option[List[File]] = {
        val filesWithoutPackageXml = files.filterNot(_.getName == "package.xml").toList
        if (filesWithoutPackageXml.isEmpty) {
            Option(filesWithoutPackageXml)
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
                    val res = newerProps.map(p => {fileMap(p.getFileName)})
                    Some(res.toList)

                case Failure(err) =>
                    throw err
            }
        }

    }
    def act {
        val checker = new ListModified(session)
        val modifiedFiles = checker.getModifiedFiles
        getFilesNewerOnRemote(modifiedFiles) match {
            case Some(files) =>
                config.responseWriter.println("RESULT=SUCCESS")
                if (!files.isEmpty) {
                    val msg = new Message(ResponseWriter.INFO, "Outdated file(s) detected.")
                    config.responseWriter.println(msg)
                    files.foreach{
                        f => config.responseWriter.println(new MessageDetail(msg, Map("filePath" -> f.getAbsolutePath, "text" -> f.getName)))
                    }
                } else {
                    config.responseWriter.println(new Message(ResponseWriter.INFO, "No outdated files detected."))
                }
                files.isEmpty
            case None =>
        }

    }
}
/**
 * 'bulkRetrieve' action uses type list specified in a file and sends retrieve() call for each type
 *@param session - SFDC session
 * Extra command line params:
 * --targetFolder=/path/to/dir (optional), if specified then extract will be done in this folder
 * --specificTypes=/path/to/file.js with extraction details
 * --updatePackageXMLOnSuccess=true|false (defaults to false) - if true then update package.xml to add missing types (if any)
 * --typesFileFormat=json|file-paths, default is 'json'
 *  if format is set to "file-paths" then types list should look like so:
 *  -------------------------------
 *  objects/Account.object
 *  classes/MyClass.cls
 *  classes/A_Class.cls
 *  -------------------------------
 */
class BulkRetrieve(session: Session) extends RetrieveMetadata(session: Session) {

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

    override def getParamDescription(paramName: String): String = {
        paramName match {
            case "specificTypes" =>
                """---specificTypes=/path/to/file
                  |full path to file with the list of metadata types to retrieve
                  |each metadata type must be on its own line in the file
                """.stripMargin
            case "updatePackageXMLOnSuccess" =>
                """--updatePackageXMLOnSuccess=true|false
                  |if --updatePackageXMLOnSuccess=true then package.xml will be updated when types file contains
                  |types missing in package.xml
                """.stripMargin
            case "typesFileFormat" =>
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
                """.stripMargin
            case "targetFolder" => "/path/to/dir (optional), if specified then retrieved files will be saved in this folder"

        }
    }

    override def getParamNames: List[String] = List("specificTypes", "updatePackageXMLOnSuccess", "typesFileFormat", "targetFolder")

    override def getSummary: String =
        """using type list specified in a given file send retrieve() call for each type
          |and load retrieve results into a temporary folder
        """.stripMargin

    override def getName: String = "bulkRetrieve"

    //some types require special treatment and need other object types included in the package in order to return meaningful result
    private val complexTypes = Map("Profile" -> Set("CustomObject"), "PermissionSet" -> Set("CustomObject"))
    /**
     * retrieve single type and its members
     * @param metadataTypeName
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
            case messages if null != messages && !messages.isEmpty=>
                //check if there are errors
                throw new RetrieveError(retrieveResult)
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
        val components:List[String] = scala.io.Source.fromFile(componentListFile).getLines().filter(!_.trim.isEmpty).toList
        components.map(FileUtils.normalizePath(_))
    }

    def act(): Unit = {
        val tempFolder = config.getProperty("targetFolder")  match {
          case Some(x) => new File(x)
          case None =>
              FileUtils.createTempDir(config)
        }

        //load file list from specified file
        val typesFile = new File(config.getRequiredProperty("specificTypes").get)
        val isJSONFormat = config.getProperty("typesFileFormat").getOrElse("json") == "json"

        var fileCountByType = Map[String, Int]()
        var errors = List[ResponseWriter.Message]()
        var membersByXmlName = new mutable.HashMap[String, List[String]]()


        if (isJSONFormat) {
            for (line <- scala.io.Source.fromFile(typesFile).getLines().filter(!_.trim.isEmpty)) {
                JSON.parseRaw(line)  match {
                    case Some(json) =>
                        val data = json.asInstanceOf[JSONObject].obj
                        val typeName = data("XMLName").asInstanceOf[String]
                        val members = data("members").asInstanceOf[JSONArray].list.asInstanceOf[List[String]]
                        membersByXmlName += typeName -> members

                    case None =>
                        errors ::= new Message(ResponseWriter.ERROR, "failed to parse line: '" + line + "'. Make sure you specified correct '--typesFileFormat' value")
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
                          case _fileNames if !_fileNames.isEmpty && Nil != _fileNames =>

                              val objNames = _fileNames.map(_.drop(dirName.size + 1)).map(
                                  name => if (name.endsWith(extension)) name.dropRight(extension.size) else name
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
            val membersByXmlNameMap = membersByXmlName.toMap

            for (typeName <- membersByXmlNameMap.keySet) {
                Try(retrieveOne(typeName, membersByXmlNameMap)) match {
                    case Success(retrieveResult) =>
                        val filePropsMap = updateFromRetrieve(retrieveResult, tempFolder)
                        val fileCount = filePropsMap.values.filter(props => !props.getFullName.endsWith("-meta.xml") && !props.getFullName.endsWith("package.xml")).size
                        fileCountByType += typeName -> fileCount
                    case Failure(err) =>
                        err match {
                            case e: RetrieveError =>
                                err.asInstanceOf[RetrieveError].retrieveResult.getMessages match {
                                    case messages if null != messages && !messages.isEmpty=>
                                        for(msg <- messages) {
                                            errors ::= new Message(ResponseWriter.ERROR, msg.getFileName + ": " + msg.getProblem)
                                        }
                                    case _ =>
                                }
                            case _ => throw err
                        }
                }

            }
        }
        if (errors.isEmpty) {
            config.responseWriter.println("RESULT=SUCCESS")
            config.responseWriter.println("RESULT_FOLDER=" + tempFolder.getAbsolutePath)
            config.responseWriter.println("FILE_COUNT_BY_TYPE=" + JSONObject(fileCountByType).toString(ResponseWriter.defaultFormatter))
        } else {
            config.responseWriter.println("RESULT=FAILURE")
            errors.foreach(responseWriter.println(_))
        }

    }
}
