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

import com.neowit.utils.{ZipUtils, FileUtils, Logging, Config}
import java.io.{File, FileOutputStream}
import com.sforce.soap.metadata.{RetrieveMessage, RetrieveResult, RetrieveRequest, DeployOptions}
import scala.util.{Try, Failure, Success}
import com.neowit.utils.ResponseWriter.{MessageDetail, Message}

class UnsupportedActionError(msg: String) extends Error(msg: String)

/*
class ActionError(msg: String) extends Error(msg: String) {
    val writer = Config.getConfig.responseWriter
    writer.println("RESULT=FAILURE")
    writer.println(msg)
}
*/

object ActionFactory {

    def getAction(session:Session, name: String): Option[Action] = {
        name match {
            case "refresh" => Some(new RefreshMetadata(session))
            case "listModified" => Some(new ListModified(session))
            case "deployModified" => Some(new DeployModified(session))
            case "listConflicts" => Some(new ListConflicting(session))
            case _ => throw new UnsupportedActionError(name + " is not supported")
        }

    }
}
trait Action extends Logging {
    def act

}
trait AsyncAction extends Action {
}

abstract class MetadataAction(session: Session) extends AsyncAction {
    val config:Config = session.getConfig

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
                if (reportMissing || !messages.filterNot(isMissingFileError(_)).isEmpty) {
                    throw new RetrieveError(retrieveResult)
                }
            case _ =>
        }
        retrieveResult

    }
    private def isMissingFileError(message: RetrieveMessage): Boolean = {
        message.getProblem.contains("In field: specific ids - no ApexClass named")
    }
}

/**
 * 'refresh' action is 'retrieve' for all elements specified in package.xml
 *@param session - SFDC session
 */
class RefreshMetadata(session: Session) extends RetrieveMetadata(session: Session) {

    def act {
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
                                config.responseWriter.println("RESULT=FAILURE")
                                for(msg <- messages) {
                                    config.responseWriter.println(msg.getFileName + ": " + msg.getProblem)
                                }
                            case _ =>
                        }
                    case _ => throw err
                }
        }
    }
    /**
     * using ZIP file produced, for example, as a result of Retrieve operation
     * extract content and generate response file
     */
    def updateFromRetrieve(retrieveResult: com.sforce.soap.metadata.RetrieveResult) {

        //val outputPath = appConfig.srcDir.getParentFile.getAbsolutePath
        //extract in temp area first
        val resultsFile = FileUtils.createTempFile("retrieveResult", ".zip")
        val out = new FileOutputStream(resultsFile)
        try {
            out.write(retrieveResult.getZipFile)
        } finally {
            out.close()
        }
        val tempFolder = FileUtils.createTempDir(config)
        val propertyByFilePath = new collection.mutable.HashMap[String,  com.sforce.soap.metadata.FileProperties]()
        try {
            val localDateByFName = ZipUtils.extract(resultsFile, tempFolder)
            //update session with file properties
            for (fileProp <- retrieveResult.getFileProperties) {
                val key = MetadataType.getKey(fileProp)
                val lastModifiedLocally = localDateByFName(fileProp.getFileName)
                val valueMap = MetadataType.getValueMap(fileProp, lastModifiedLocally)
                session.setData(key, valueMap)

                propertyByFilePath.put(fileProp.getFileName, fileProp)
            }
        } finally {
            session.storeSessionData()
            resultsFile.delete()
        }
        config.responseWriter.println("RESULT=SUCCESS")
        config.responseWriter.println("result.folder=" + tempFolder.getAbsolutePath)
        config.responseWriter.println("file-count=" + propertyByFilePath.size)
    }
}

class ListModified(session: Session) extends MetadataAction(session: Session) {
    /**
     * list locally modified files using data from session.properties
     */
    def getModifiedFiles:List[File] = {
        val config = session.getConfig
        //check if package.xml is modified
        val packageXml = new MetaXml(config)
        val packageXmlFile = packageXml.getPackageXml
        //val packageXmlData = session.getData(session.getKeyByFile(packageXmlFile))

        //logger.debug("packageXmlData=" + packageXmlData)
        val allFiles  = packageXmlFile :: FileUtils.listFiles(config.srcDir)
        val modifiedFiles = allFiles.filter(session.isModified(_))
        modifiedFiles
    }

    def act {
        val modifiedFiles = getModifiedFiles

        config.responseWriter.println("RESULT=SUCCESS")
        config.responseWriter.println("file-count=" + modifiedFiles.size)
        val msg = new Message("info", "Modified file(s) detected.")
        config.responseWriter.println(msg)
        config.responseWriter.startSection("MODIFIED FILE LIST")
        for(f <- modifiedFiles) {
            config.responseWriter.println(new MessageDetail(msg, Map("filePath" -> f.getAbsolutePath, "text" -> session.getRelativePath(f))))
        }
        config.responseWriter.endSection("MODIFIED FILE LIST")
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

                      val millsLocal = session.getData(key).getOrElse("LastModifiedDateMills", "0").toLong
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
                    val msg = new Message("info", "Outdated file(s) detected.")
                    config.responseWriter.println(msg)
                    files.foreach{
                        f => config.responseWriter.println(new MessageDetail(msg, Map("filePath" -> f.getAbsolutePath, "text" -> f.getName)))
                    }
                } else {
                    config.responseWriter.println(new Message("info", "No outdated files detected."))
                }
                files.isEmpty
            case None =>
        }

    }
}
/**
 * 'deployModified' action grabs all modified files and sends deploy() File-Based call
 *@param session - SFDC session
 */
class DeployModified(session: Session) extends MetadataAction(session: Session) {
    private val alwaysIncludeNames = Set("src", "package.xml")

    private def excludeFileFromZip(modifiedFiles: Set[File], file: File) = {
        val exclude = !modifiedFiles.contains(file) && !alwaysIncludeNames.contains(file.getName)
        logger.trace(file.getName + " include=" + !exclude)
        exclude
    }

    def act {
        val modifiedFiles = new ListModified(session).getModifiedFiles
        if (modifiedFiles.isEmpty) {
            config.responseWriter.println("RESULT=SUCCESS")
            config.responseWriter.println("file-count=" + modifiedFiles.size)
            config.responseWriter.println(new Message("info", "no modified files detected."))
        } else {
            //first check if SFDC has newer version of files we are about to deploy
            val checker = new ListConflicting(session)
            val canDeploy = checker.getFilesNewerOnRemote(modifiedFiles) match {
              case Some(files) =>
                  if (!files.isEmpty) {
                      config.responseWriter.println("RESULT=FAILURE")

                      val msg = new Message("info", "Outdated file(s) detected. Use 'refresh' before 'deploy'.")
                      config.responseWriter.println(msg)
                      files.foreach{
                          f => config.responseWriter.println(new MessageDetail(msg, Map("filePath" -> f.getAbsolutePath, "text" -> f.getName)))
                      }
                  }
                  files.isEmpty
              case None => false
            }
            if (canDeploy) {

                //for every modified file add its -meta.xml if exists
                val metaXmlFiles = for (file <- modifiedFiles;
                                        metaXml = new File(file.getAbsolutePath + "-meta.xml")
                                        if metaXml.exists()) yield metaXml

                val allFilesToDeploySet = (modifiedFiles ++ metaXmlFiles).toSet
                /*
                for debug purpose only, to check what is put in the archive
                //get temp file name
                val destZip = FileUtils.createTempFile("deploy", ".zip")
                destZip.delete()

                ZipUtils.zipDir(session.getConfig.srcPath, destZip.getAbsolutePath, excludeFileFromZip(allFilesToDeploy, _))
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
                    if (!checkOnly) {
                        for ( successMessage <- deployDetails.getComponentSuccesses) {
                            val relativePath = successMessage.getFileName
                            val key = session.getKeyByRelativeFilePath(relativePath)
                            val localMills = new File(config.projectDir, relativePath).lastModified()
                            val newData = MetadataType.getValueMap(deployResult, successMessage, localMills)
                            val oldData = session.getData(key)
                            session.setData(key, oldData ++ newData)
                        }
                    }
                    session.storeSessionData()
                    config.responseWriter.println("RESULT=SUCCESS")
                    config.responseWriter.println("file-count=" + modifiedFiles.size)
                    if (!checkOnly) {
                        config.responseWriter.startSection("DEPLOYED FILES")
                        modifiedFiles.foreach(f => config.responseWriter.println(f.getName))
                        config.responseWriter.endSection("DEPLOYED FILES")
                    }
                }
            }
        }
    }
}