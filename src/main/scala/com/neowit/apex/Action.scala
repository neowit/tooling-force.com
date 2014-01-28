/*
 * Copyright (c) 2014 Andrey Gavrikov.
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

import com.neowit.utils._
import java.io.{PrintWriter, File, FileOutputStream}
import com.sforce.soap.metadata._
import scala.util.{Try, Failure, Success}
import com.neowit.utils.ResponseWriter.{MessageDetail, Message}
import scala.util.parsing.json._
import scala.Predef._
import scala.util.Failure
import scala.Some
import scala.util.Success
import com.neowit.utils.ResponseWriter.MessageDetail
import scala.util.parsing.json.JSONArray
import scala.util.parsing.json.JSONObject
import scala.collection.mutable

class UnsupportedActionError(msg: String) extends Error(msg: String)

class ActionError(msg: String) extends Error(msg: String) {
    val writer = Config.getConfig.responseWriter
    writer.println("RESULT=FAILURE")
    writer.println(msg)
}

object ActionFactory {

    def getAction(session:Session, name: String): Option[Action] = {
        name.toLowerCase match {
            case "refresh" => Some(new RefreshMetadata(session))
            case "listmodified" => Some(new ListModified(session))
            case "deploymodified" => Some(new DeployModified(session))
            case "deployall" => Some(new DeployAll(session))
            case "deployspecificfiles" => Some(new DeploySpecificFiles(session))
            case "listconflicts" => Some(new ListConflicting(session))
            case "describemetadata" => Some(new DescribeMetadata(session))
            case "bulkretrieve" => Some(new BulkRetrieve(session))
            case "listmetadata" => Some(new ListMetadata(session))
            case _ => throw new UnsupportedActionError(name + " is not supported")
        }
    }
}
trait Action extends Logging {
    def act
    //TODO def help //return formatted help for given action

}
trait AsyncAction extends Action {
}

abstract class MetadataAction(session: Session) extends AsyncAction {
    val config:Config = session.getConfig
    val responseWriter: ResponseWriter = config.responseWriter

}

case class RetrieveError(retrieveResult: RetrieveResult) extends Error

abstract class RetrieveMetadata(session: Session) extends MetadataAction(session: Session) {

    def setUpackaged(retrieveRequest: RetrieveRequest) {
        val metaXml = new MetaXml(session.getConfig)
        val unpackagedManifest = metaXml.getPackageXml
        logger.debug("Manifest file: " + unpackagedManifest.getAbsolutePath)

        retrieveRequest.setUnpackaged(metaXml.getPackage)
    }

    /**
     * retrieve information about specific files
     * @param files
     */
    def retrieveFiles(files: List[File], reportMissing: Boolean = true): RetrieveResult = {
        val retrieveRequest = new RetrieveRequest()
        retrieveRequest.setApiVersion(config.apiVersion)
        //setSpecificFiles requires file names that look like: classes/MyClass.cls
        retrieveRequest.setSpecificFiles(files.map(session.getRelativePath(_).replaceFirst("src/", "")).toArray)
        retrieveRequest.setSinglePackage(true)
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
    def updateFromRetrieve(retrieveResult: com.sforce.soap.metadata.RetrieveResult, tempFolder: File): Int = {

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
        propertyByFilePath.size
    }
}

/**
 * 'refresh' action is 'retrieve' for all elements specified in package.xml
 *@param session - SFDC session
 */
class RefreshMetadata(session: Session) extends RetrieveMetadata(session: Session) {

    def act {
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
        val fileCount = updateFromRetrieve(retrieveResult, tempFolder)

        config.responseWriter.println("RESULT=SUCCESS")
        config.responseWriter.println("RESULT_FOLDER=" + tempFolder.getAbsolutePath)
        config.responseWriter.println("FILE_COUNT=" + fileCount)
    }
}

class ListModified(session: Session) extends MetadataAction(session: Session) {
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
        config.responseWriter.println(msg)
        for(f <- modifiedFiles) {
            config.responseWriter.println(new MessageDetail(msg, Map("filePath" -> f.getAbsolutePath, "text" -> session.getRelativePath(f))))
        }
        responseWriter.println("HAS_MODIFIED_FILES=true")
        config.responseWriter.startSection("MODIFIED FILE LIST")
        for(f <- modifiedFiles) {
            config.responseWriter.println("MODIFIED_FILE=" + session.getRelativePath(f))
        }
        config.responseWriter.endSection("MODIFIED FILE LIST")

    }

    def act {
        val modifiedFiles = getModifiedFiles

        config.responseWriter.println("RESULT=SUCCESS")
        config.responseWriter.println("FILE_COUNT=" + modifiedFiles.size)
        if (!modifiedFiles.isEmpty) {
            reportModifiedFiles(modifiedFiles)
        } else {
            config.responseWriter.println(new Message(ResponseWriter.INFO, "No Modified file(s) detected."))

        }
    }

}

/**
 * check local modified files against their Remote versions to see if remote is newer
 */
class ListConflicting(session: Session) extends RetrieveMetadata(session: Session) {

    def getFilesNewerOnRemote(files: List[File]): Option[List[File]] = {
        val fileMap = files.map(f => (session.getRelativePath(f).replaceFirst("src/", ""), f) ).toMap

        Try(retrieveFiles(files, reportMissing = false)) match {
          case Success(retrieveResult) =>
              val newerProps = retrieveResult.getFileProperties.filter(
                  props => {
                      val key = (if (null == props.getNamespacePrefix) "unpackaged" else props.getNamespacePrefix ) +
                          File.separator + props.getFileName

                      val millsLocal = session.getData(key).getOrElse("LastModifiedDateMills", 0).asInstanceOf[BigDecimal].toLong
                      val millsRemote = MetadataType.getLastModifiedDateMills(props)
                      millsLocal < millsRemote
                  }
              )
              val res = newerProps.map(p => {fileMap(p.getFileName)})
              Some(res.toList)

          case Failure(err) =>
              err match {
                  case e: RetrieveError =>
                      val messages = err.asInstanceOf[RetrieveError].retrieveResult.getMessages
                      config.responseWriter.println("RESULT=FAILURE")
                      for(msg <- messages) {
                          config.responseWriter.println("ERROR", Map("filePath" -> msg.getFileName, "text" -> msg.getProblem))
                      }
                      None
                  case _ => throw err
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
 * 'deployModified' action grabs all modified files and sends deploy() File-Based call
 *@param session - SFDC session
 * Extra command line params:
 * --ignoreConflicts=true|false (defaults to false) - if true then skip ListConflicting check
 * --checkOnly=true|false (defaults to false) - if true then do a dry-run without modifying SFDC
 */
class DeployModified(session: Session) extends MetadataAction(session: Session) {
    private val alwaysIncludeNames = Set("src", "package.xml")

    private def excludeFileFromZip(modifiedFiles: Set[File], file: File) = {
        val exclude = !modifiedFiles.contains(file) && !alwaysIncludeNames.contains(file.getName)
        logger.trace(file.getName + " include=" + !exclude)
        exclude
    }

    protected def hasConflicts(files: List[File]): Boolean = {
        logger.info("Check Conflicts with Remote")
        val checker = new ListConflicting(session)
        checker.getFilesNewerOnRemote(files) match {
            case Some(conflictingFiles) =>
                if (!conflictingFiles.isEmpty) {
                    config.responseWriter.println("RESULT=FAILURE")

                    val msg = new Message(ResponseWriter.WARN, "Outdated file(s) detected.")
                    config.responseWriter.println(msg)
                    conflictingFiles.foreach{
                        f => config.responseWriter.println(new MessageDetail(msg, Map("filePath" -> f.getAbsolutePath, "text" -> f.getName)))
                    }
                    config.responseWriter.println(new Message(ResponseWriter.WARN, "Use 'refresh' before 'deploy'."))
                }
                !conflictingFiles.isEmpty
            case None => false
        }
    }

    def act {
        val modifiedFiles = new ListModified(session).getModifiedFiles
        if (modifiedFiles.isEmpty) {
            config.responseWriter.println("RESULT=SUCCESS")
            config.responseWriter.println("FILE_COUNT=" + modifiedFiles.size)
            config.responseWriter.println(new Message(ResponseWriter.INFO, "no modified files detected."))
        } else {
            //first check if SFDC has newer version of files we are about to deploy
            val ignoreConflicts = config.getProperty("ignoreConflicts").getOrElse("false").toBoolean

            val canDeploy = ignoreConflicts || !hasConflicts(modifiedFiles)

            if (canDeploy) {
                val checkOnly = config.getProperty("checkOnly").getOrElse("false").toBoolean
                deploy(modifiedFiles, updateSessionDataOnSuccess = !checkOnly)
            }
        }
    }

    def deploy(files: List[File], updateSessionDataOnSuccess: Boolean) {
        //for every modified file add its -meta.xml if exists
        val metaXmlFiles = for (file <- files;
                                metaXml = new File(file.getAbsolutePath + "-meta.xml")
                                if metaXml.exists()) yield metaXml

        var allFilesToDeploySet = (files ++ metaXmlFiles).toSet

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

        val deployOptions = new DeployOptions()
        deployOptions.setPerformRetrieve(false)
        deployOptions.setAllowMissingFiles(true)
        deployOptions.setRollbackOnError(true)
        val checkOnly = config.getProperty("checkOnly").getOrElse("false").toBoolean
        deployOptions.setCheckOnly(checkOnly)
        //deployOptions.setPerformRetrieve(true)

        val deployResult = session.deploy(ZipUtils.zipDirToBytes(session.getConfig.srcDir, excludeFileFromZip(allFilesToDeploySet, _) ), deployOptions)

        val deployDetails = deployResult.getDetails
        if (!deployResult.isSuccess) {
            config.responseWriter.println("RESULT=FAILURE")
            if (null != deployDetails) {
                config.responseWriter.startSection("ERROR LIST")
                for ( failureMessage <- deployDetails.getComponentFailures) {
                    val line = failureMessage.getLineNumber
                    val column = failureMessage.getColumnNumber
                    val filePath = failureMessage.getFileName
                    val problem = failureMessage.getProblem
                    config.responseWriter.println("ERROR", Map("line" -> line, "column" -> column, "filePath" -> filePath, "text" -> problem))
                }
                config.responseWriter.endSection("ERROR LIST")
            }


        } else {
            //update session data for successful files
            if (updateSessionDataOnSuccess) {
                val calculateMD5 = config.useMD5Hash
                val calculateCRC32 = !calculateMD5  //by default use only CRC32

                for ( successMessage <- deployDetails.getComponentSuccesses) {
                    val relativePath = successMessage.getFileName
                    val key = session.getKeyByRelativeFilePath(relativePath)
                    val f = new File(config.projectDir, relativePath)
                    val localMills = f.lastModified()

                    val md5Hash = if (calculateMD5) FileUtils.getMD5Hash(f) else ""
                    val crc32Hash = if (calculateCRC32) FileUtils.getCRC32Hash(f) else -1L

                    val fMeta = new File(f.getAbsolutePath + "-meta.xml")
                    val (metaLocalMills: Long, metaMD5Hash: String, metaCRC32Hash:Long) = if (fMeta.canRead) {
                        (   fMeta.lastModified(),
                            if (calculateMD5) FileUtils.getMD5Hash(fMeta) else "",
                            if (calculateCRC32) FileUtils.getCRC32Hash(fMeta) else -1L)
                    } else {
                        (-1L, "", -1L)
                    }

                    val newData = MetadataType.getValueMap(deployResult, successMessage, localMills, md5Hash, crc32Hash, metaLocalMills, metaMD5Hash, metaCRC32Hash)
                    val oldData = session.getData(key)
                    session.setData(key, oldData ++ newData)
                }
            }
            session.storeSessionData()
            config.responseWriter.println("RESULT=SUCCESS")
            config.responseWriter.println("FILE_COUNT=" + files.size)
            if (!checkOnly) {
                config.responseWriter.startSection("DEPLOYED FILES")
                files.foreach(f => config.responseWriter.println(f.getName))
                config.responseWriter.endSection("DEPLOYED FILES")
            }
        }
    }
}

/**
 * 'deployAll' action grabs all project files and sends deploy() File-Based call
 *@param session - SFDC session
 * Extra command line params:
 * --updateSessionDataOnSuccess=true|false (defaults to false) - if true then update session data if deployment is successful
 */
class DeployAll(session: Session) extends DeployModified(session: Session) {
    override def act {
        val allFiles = getAllFiles

        val callingAnotherOrg = session.callingAnotherOrg
        val updateSessionDataOnSuccess = !callingAnotherOrg || config.getProperty("updateSessionDataOnSuccess").getOrElse("false").toBoolean
        deploy(allFiles, updateSessionDataOnSuccess)


    }

    /**
     * list locally modified files using data from session.properties
     */
    def getAllFiles:List[File] = {
        val config = session.getConfig
        //check if package.xml is modified
        val packageXml = new MetaXml(config)
        val packageXmlFile = packageXml.getPackageXml
        //val packageXmlData = session.getData(session.getKeyByFile(packageXmlFile))

        //logger.debug("packageXmlData=" + packageXmlData)
        //val allFiles  = (packageXmlFile :: FileUtils.listFiles(config.srcDir)).toSet
        val allFiles  = FileUtils.listFiles(config.srcDir).filter(
            //remove all non apex files
            file => DescribeMetadata.isValidApexFile(session, file)
        ).toSet

        allFiles.toList
    }
}

/**
 * 'deploySpecificFiles' action uses file list specified in a file and sends deploy() File-Based call
 *@param session - SFDC session
 * Extra command line params:
 * --specificFiles=/path/to/file with file list
 * --updateSessionDataOnSuccess=true|false (defaults to false) - if true then update session data if deployment is successful
 */
class DeploySpecificFiles(session: Session) extends DeployModified(session: Session) {
    override def act {
        val files = getFiles
        if (files.isEmpty) {
            config.responseWriter.println("RESULT=FAILURE")
            val fileListFile = new File(config.getRequiredProperty("specificFiles").get)
            responseWriter.println(new Message(ResponseWriter.ERROR, "no valid files in " + fileListFile))
        } else {

            //first check if SFDC has newer version of files we are about to deploy
            val ignoreConflicts = config.getProperty("ignoreConflicts").getOrElse("false").toBoolean

            val canDeploy = ignoreConflicts || !hasConflicts(files)
            if (canDeploy) {
                val callingAnotherOrg = session.callingAnotherOrg
                val updateSessionDataOnSuccess = !callingAnotherOrg || config.getProperty("updateSessionDataOnSuccess").getOrElse("false").toBoolean
                deploy(files, updateSessionDataOnSuccess)
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

object DescribeMetadata {
    private var describeMetadataObjectMap:Map[String, DescribeMetadataObject] = Map()

    def getMap(session: Session): Map[String, DescribeMetadataObject] = {
        if (describeMetadataObjectMap.isEmpty) {
            val describer = new DescribeMetadata(session)
            //first try to get metadata description from local file
            val localMap = describer.loadFromFile
            if (localMap.isEmpty) {
                //finally try loading from remote
                val remoteMap = describer.loadFromRemote
                describeMetadataObjectMap = remoteMap
            } else {
                describeMetadataObjectMap = localMap
            }
        }
        describeMetadataObjectMap
    }

    private var xmlNameBySuffix:Map[String, String] = Map()

    def getXmlNameBySuffix(session: Session, suffix: String): Option[String] = {
        if (xmlNameBySuffix.isEmpty) {
            val nameBySuffix = new mutable.HashMap[String, String]()
            for (describeObject <- getMap(session).values) {
                if (null != describeObject.getSuffix) {
                    nameBySuffix += describeObject.getSuffix -> describeObject.getXmlName
                }
            }
            xmlNameBySuffix = nameBySuffix.toMap
        }
        xmlNameBySuffix.get(suffix)
    }
    def isValidApexFile(session: Session, file: File): Boolean = {
       if ("package.xml" == file.getName)
           true
       else
           getXmlNameBySuffix(session, FileUtils.getExtension(file)) match {
               case Some(x) => true
               case None => false
           }
    }
}

/**
 * 'decribeMetadata' action saves result of describeMetadata call in JSON format
 *@param session - SFDC session
 * Extra command line params:
 * --allMetaTypesFilePath - path to file where results shall be saved
 */
class DescribeMetadata(session: Session) extends MetadataAction(session: Session) {

    def loadFromFile: Map[String, DescribeMetadataObject] = {

        val describeMetadataObjectMap = new mutable.HashMap[String, DescribeMetadataObject]

        for (line <- scala.io.Source.fromFile(config.storedDescribeMetadataResultFile).getLines()) {
            //JSON.parseFull(line)
            JSON.parseRaw(line)  match {
                case Some(json) =>
                    val data = json.asInstanceOf[JSONObject].obj
                    val descrObj = new DescribeMetadataObject()
                    descrObj.setDirectoryName(data.getOrElse("DirName", "").asInstanceOf[String])
                    descrObj.setInFolder(data.getOrElse("InFolder", false).asInstanceOf[Boolean])
                    descrObj.setMetaFile(data.getOrElse("HasMetaFile", false).asInstanceOf[Boolean])
                    descrObj.setSuffix(data.getOrElse("Suffix", "").asInstanceOf[String])
                    val xmlName = data.getOrElse("XMLName", "").asInstanceOf[String]
                    descrObj.setXmlName(xmlName)
                    val xmlNames = data.getOrElse("ChildObjects", new JSONArray(List())).asInstanceOf[JSONArray]
                    descrObj.setChildXmlNames(xmlNames.list.asInstanceOf[List[String]].toArray)

                    describeMetadataObjectMap += xmlName -> descrObj
                case None =>
            }

        }
        describeMetadataObjectMap.toMap
    }

    private def storeDescribeResult(file: File, lines: Iterator[String]) {
        val writer = new PrintWriter(file)
        lines.foreach(writer.println)
        writer.close()
    }

    def loadFromRemote: Map[String, DescribeMetadataObject] = {
        Try(session.describeMetadata(config.apiVersion)) match {
            case Success(describeResult) =>
                val describeMetadataObjectMap = describeResult.getMetadataObjects.map(describeObject => (describeObject.getXmlName, describeObject)).toMap
                //dump to local file
                val linesBuf = new scala.collection.mutable.ListBuffer[String]
                for (xmlName <- describeMetadataObjectMap.keySet) {

                    describeMetadataObjectMap.get(xmlName) match {
                        case Some(_describeObject) =>
                            val data = Map(
                                "ChildObjects" -> JSONArray(_describeObject.getChildXmlNames.toList),
                                "DirName" -> _describeObject.getDirectoryName,
                                "InFolder" -> _describeObject.isInFolder,
                                "HasMetaFile" -> _describeObject.getMetaFile,
                                "Suffix" -> (if (null != _describeObject.getSuffix) _describeObject.getSuffix else "\"\""),
                                "XMLName" -> _describeObject.getXmlName
                            )
                            linesBuf += JSONObject(data).toString(ResponseWriter.defaultFormatter)
                        case None =>
                    }
                }
                storeDescribeResult(config.storedDescribeMetadataResultFile, linesBuf.iterator)
                //check if user requested alternative response location
                config.getProperty("allMetaTypesFilePath") match {
                  case Some(allMetaTypesFilePath) =>
                      val userDefinedFile = new File(allMetaTypesFilePath)
                      if (userDefinedFile != config.storedDescribeMetadataResultFile) {
                          storeDescribeResult(userDefinedFile, linesBuf.iterator)

                      }
                  case None => //no action required
                }

                describeMetadataObjectMap
            case Failure(throwed) => throw throwed
        }
    }

    def act {
        //load from SFDC and dump to local file
        val resMap = loadFromRemote
        responseWriter.println("RESULT=SUCCESS")
        responseWriter.println("RESULT_FILE=" + config.storedDescribeMetadataResultFile.getAbsolutePath)
        responseWriter.println("FILE_COUNT=" + resMap.size)
    }
}


/**
 * 'bulkRetrieve' action uses type list specified in a file and sends retrieve() call for each type
 *@param session - SFDC session
 * Extra command line params:
 * --specificTypes=/path/to/file with file list
 * --updatePackageXMLOnSuccess=true|false (defaults to false) - if true then update package.xml to add missing types (if any)
 */
class BulkRetrieve(session: Session) extends RetrieveMetadata(session: Session) {
    //some types require special treatment and need other object types included in the package in order to return meaningful result
    private val complexTypes = Map("Profile" -> Set("CustomObject"), "PermissionSet" -> Set("CustomObject"))
    /**
     * retrieve single type and its members
     * @param metadataTypeName
     */
    def retrieveOne(metadataTypeName: String, membersByXmlName: Map[String, List[String]]): RetrieveResult = {
        val members = membersByXmlName(metadataTypeName)
        val retrieveRequest = new RetrieveRequest()
        retrieveRequest.setApiVersion(config.apiVersion)
        retrieveRequest.setSinglePackage(true)

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

    def act {
        val tempFolder = FileUtils.createTempDir(config)

        //load file list from specified file
        val typesFile = new File(config.getRequiredProperty("specificTypes").get)
        val metadataByXmlName = DescribeMetadata.getMap(session)

        var fileCountByType = Map[String, Int]()
        var errors = List[ResponseWriter.Message]()
        var membersByXmlName = new mutable.HashMap[String, List[String]]()
        for (line <- scala.io.Source.fromFile(typesFile).getLines()) {
            JSON.parseRaw(line)  match {
                case Some(json) =>
                    val data = json.asInstanceOf[JSONObject].obj
                    val typeName = data("XMLName").asInstanceOf[String]
                    val members = data("members").asInstanceOf[JSONArray].list.asInstanceOf[List[String]]
                    membersByXmlName += typeName -> members

                case None =>
                    errors ::= new Message(ResponseWriter.ERROR, "failed to parse line: " + line)
            }
        }

        if (errors.isEmpty) {
            val membersByXmlNameMap = membersByXmlName.toMap

            for (typeName <- membersByXmlNameMap.keySet) {
                Try(retrieveOne(typeName, membersByXmlNameMap)) match {
                    case Success(retrieveResult) =>
                        val realFileCount = updateFromRetrieve(retrieveResult, tempFolder) - 1 //-1 because no need to count package.xml
                    val fileCount = metadataByXmlName.get(typeName) match {
                            case Some(describeMetadataObject) =>
                                //ignore -meta.xml in file count
                                if (describeMetadataObject.isMetaFile)
                                    realFileCount / 2
                                else
                                    realFileCount
                            case None => 0
                        }
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

            config.responseWriter.println("RESULT=SUCCESS")
            config.responseWriter.println("RESULT_FOLDER=" + tempFolder.getAbsolutePath)
            config.responseWriter.println("FILE_COUNT_BY_TYPE=" + JSONObject(fileCountByType).toString(ResponseWriter.defaultFormatter))
        } else {
            config.responseWriter.println("RESULT=FAILURE")
            errors.foreach(responseWriter.println(_))
        }

    }
}

/**
 * 'listMetadata' action uses type list specified in a file and sends listMetadata() call for specified types
 *@param session - SFDC session
 * Extra command line params:
 * --specificTypes=/path/to/file with file list
 */
class ListMetadata(session: Session) extends MetadataAction(session: Session) {
    def act: Unit = {

        val metadataByXmlName = DescribeMetadata.getMap(session)
        //load file list from specified file
        val queries = new mutable.ArrayBuffer[ListMetadataQuery]()
        val typesFile = new File(config.getRequiredProperty("specificTypes").get)
        for (typeName <- scala.io.Source.fromFile(typesFile).getLines()) {
            if (!typeName.isEmpty) {
                metadataByXmlName.get(typeName)  match {
                    case Some(describeObject) =>
                        val query = new ListMetadataQuery()
                        query.setType(typeName)
                        queries += query
                    //query.setFolder(describeObject.get)
                    case None => throw new Error("Invalid type: " + typeName)
                }
            }

        }

        def addToMap(originalMap: Map[String, List[String]], key: String, value: String): Map[String, List[String]] = {
            originalMap.get(key)  match {
              case Some(list) =>
                  val newList: List[String] = value :: list
                  originalMap ++ Map(key -> newList)
              case None => originalMap ++ Map(key -> List(value))
            }
        }

        var resourcesByXmlTypeName = Map[String, List[String]]()
        Try(session.listMetadata(queries.toArray, config.apiVersion)) match {
            case Success(fileProperties) =>
                for (fileProp <- fileProperties) {
                    val typeName = fileProp.getType
                    val resourceName = fileProp.getFullName
                    resourcesByXmlTypeName = addToMap(resourcesByXmlTypeName, typeName, resourceName)
                }
            case Failure(error) => throw error
        }

        config.responseWriter.println("RESULT=SUCCESS")
        if (!resourcesByXmlTypeName.isEmpty) {
            //dump results to JSON file, with each line looking like this
            //{"CustomTab" : ["Account_Edit", "My_Object__c"]}

            val tempFile = FileUtils.createTempFile("listMetadata", ".js")
            val writer = new PrintWriter(tempFile)
            resourcesByXmlTypeName.foreach{case (key: String, values: List[String]) =>
                val line = JSONObject(Map(key -> JSONArray(values))).toString(ResponseWriter.defaultFormatter)
                writer.println(line)
            }
            writer.close()
            config.responseWriter.println("RESULT_FILE=" + tempFile.getAbsolutePath)
        }
    }
}