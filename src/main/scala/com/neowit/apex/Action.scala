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
import com.sforce.soap.metadata.{FileProperties, RetrieveResult, RetrieveRequest, DeployOptions}

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
            case "checkAgainstRemote" => Some(new CheckAgainstRemote(session))
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

abstract class RetrieveMetadata(session: Session) extends MetadataAction(session: Session) {

    def setUpackaged(retrieveRequest: RetrieveRequest) {
        val metaXml = new MetaXml(session.getConfig)
        val unpackagedManifest = metaXml.getPackageXml
        logger.debug("Manifest file: " + unpackagedManifest.getAbsolutePath)

        retrieveRequest.setUnpackaged(metaXml.getPackage)
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
        val retrieveResult = session.retrieve(retrieveRequest)
        updateFromRetrieve(retrieveResult)


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

object ListModified {
    /**
     * list locally modified files using data from session.properties
     */
    def getModifiedFiles(session: Session):List[File] = {
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


}
class ListModified(session: Session) extends MetadataAction(session: Session) {
    def act {
        val modifiedFiles = ListModified.getModifiedFiles(session)

        config.responseWriter.println("RESULT=SUCCESS")
        config.responseWriter.println("file-count=" + modifiedFiles.size)
        config.responseWriter.println("# MODIFIED FILE LIST START")
        for(f <- modifiedFiles) {
            config.responseWriter.println(session.getRelativePath(f))
        }
        config.responseWriter.println("# MODIFIED FILE LIST END")
    }

}

class CheckAgainstRemote(session: Session) extends RetrieveMetadata(session: Session) {
    /**
     * retrieve information about specific files
     * @param files
     */
    def retrieveFiles(files: List[File]): Option[RetrieveResult] = {
        val retrieveRequest = new RetrieveRequest()
        retrieveRequest.setApiVersion(config.apiVersion)
        //setSpecificFiles requires file names that look like: classes/MyClass.cls
        retrieveRequest.setSpecificFiles(files.map(session.getRelativePath(_).replaceFirst("src/", "")).toArray)
        retrieveRequest.setSinglePackage(true)
        setUpackaged(retrieveRequest)
        val retrieveResult = session.retrieve(retrieveRequest)
        if (retrieveResult.getMessages.isEmpty)
            Some(retrieveResult)
        else
            None
    }

    def getFilesNewerOnRemote(files: List[File]): Option[List[File]] = {
        val fileMap = files.map(f => (session.getRelativePath(f).replaceFirst("src/", ""), f) ).toMap

        retrieveFiles(files) match {
          case Some(retrieveResult) =>
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

          case None => None
        }

    }
    def act {
        val modifiedFiles = ListModified.getModifiedFiles(session)
        //val retrieveResult = retrieveFiles(modifiedFiles)
        getFilesNewerOnRemote(modifiedFiles)

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
        logger.debug(file.getName + " include=" + !exclude)
        exclude
    }

    def act {
        val modifiedFiles = ListModified.getModifiedFiles(session)
        if (modifiedFiles.isEmpty) {
            config.responseWriter.println("RESULT=SUCCESS")
            config.responseWriter.println("file-count=" + modifiedFiles.size)
            config.responseWriter.println("MESSAGE=no modified files detected")
        } else {
            //TODO first check if SFDC has newer version of files we are about to deploy
            val checker = new CheckAgainstRemote(session)
            val canDeploy = checker.getFilesNewerOnRemote(modifiedFiles) match {
              case Some(files) =>
                  if (!files.isEmpty) {
                      config.responseWriter.println("RESULT=FAILURE")
                      config.responseWriter.println("MESSAGE=Some files are out of date")
                      config.responseWriter.println("OUTDATED FILES START")
                      config.responseWriter.println(files.map(_.getName).mkString("\n"))
                      config.responseWriter.println("OUTDATED FILES END")
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
                //get temp file name
                val destZip = FileUtils.createTempFile("deploy", ".zip")
                destZip.delete()

                ZipUtils.zipDir(session.getConfig.srcPath, destZip.getAbsolutePath, excludeFileFromZip(allFilesToDeploy, _))
                */

                val deployOptions = new DeployOptions()
                deployOptions.setPerformRetrieve(false)
                deployOptions.setAllowMissingFiles(true)
                deployOptions.setRollbackOnError(true)
                //deployOptions.setPerformRetrieve(true)

                val deployResult = session.deploy(ZipUtils.zipDirToBytes(session.getConfig.srcDir, excludeFileFromZip(allFilesToDeploySet, _) ), deployOptions)

                val deployDetails = deployResult.getDetails
                if (!deployResult.isSuccess) {
                    config.responseWriter.println("RESULT=FAILURE")
                    if (null != deployDetails) {
                        config.responseWriter.println("# COMPONENT FAILURES START")
                        for ( failureMessage <- deployDetails.getComponentFailures) {
                            val line = failureMessage.getLineNumber
                            val column = failureMessage.getColumnNumber
                            val filePath = failureMessage.getFileName
                            val problem = failureMessage.getProblem
                            config.responseWriter.println(Map("line" -> line, "column" -> column, "filePath" -> filePath, "problem" -> problem))
                        }
                        config.responseWriter.println("# COMPONENT FAILURES END")
                    }


                } else {
                    //update session data for successful files
                    for ( successMessage <- deployDetails.getComponentSuccesses) {
                        val relativePath = successMessage.getFileName
                        val key = session.getKeyByRelativeFilePath(relativePath)
                        val localMills = new File(config.projectDir, relativePath).lastModified()
                        val newData = MetadataType.getValueMap(deployResult, successMessage, localMills)
                        val oldData = session.getData(key)
                        session.setData(key, oldData ++ newData)
                    }
                    session.storeSessionData()
                    config.responseWriter.println("RESULT=SUCCESS")
                    config.responseWriter.println("file-count=" + modifiedFiles.size)

                }
            }
        }
    }
}