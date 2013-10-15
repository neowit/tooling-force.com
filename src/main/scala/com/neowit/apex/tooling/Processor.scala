/*
 * Copyright (c) 2013 Andrey Gavrikov.
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

package com.neowit.apex.tooling

import java.io.File
import com.sforce.soap.tooling._
import scala.{Array, Some}

object ZuluTime {
    val zulu = new java.text.SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSS'Z'")
    zulu.setTimeZone(java.util.TimeZone.getTimeZone("GMT"))
    def format(d: java.util.Date):String = zulu.format(d)
    def parse(s: String): java.util.Date = zulu.parse(s)

}

trait ProcessorBase extends Logging {
    def isFile(resourcePath: String) = {
        val f = new File(resourcePath)
        f.isFile && f.canRead
    }
    def isDirectory(resourcePath: String) = {
        val f = new File(resourcePath)
        f.isDirectory && f.canRead
    }
    def deleteMetadataContainer(session: SfdcSession, sessionData: SessionData) {
        sessionData.getExistingContainer match {
          case Some(container) =>
              try {
                  sessionData.remove("MetadataContainer")
                  session.delete(container.getId)
                  logger.debug("Deleted MetadataContainer; Id=" + container.getId)
              } catch {
                  case ex:Throwable => //do not really care why delete failed
                      logger.debug("Could not delete MetadataContainer. " + ex.getMessage)
              }
              sessionData.store()
          case None => //nothing to delete
        }
    }

    def withMetadataContainer(session: SfdcSession, sessionData: SessionData)(codeBlock: (MetadataContainer) => Any) = {
        //check if container actually exists
        val container = sessionData.getExistingContainer match {
          case Some(cont) =>
              logger.debug("Re-use Existing MetadataContainer; Id=" + cont.getId)
              cont
          case None => //create container
              val newContainer = new MetadataContainer()
              newContainer.setName(Processor.containerPrefix + System.currentTimeMillis())
              val containerSaveResults: Array[SaveResult] = session.create(Array(newContainer))
              if (containerSaveResults.head.isSuccess) {
                  newContainer.setId(containerSaveResults.head.getId)
                  sessionData.setData("MetadataContainer", Map("Name" -> newContainer.getName, "Id" -> newContainer.getId))
                  sessionData.store()
                  logger.debug("Created new MetadataContainer; Id=" + sessionData.getField("MetadataContainer", "Id"))
              } else {
                  logger.debug("Failed to create Metadata Container. " + containerSaveResults.head.getErrors.head.getMessage)
                  throw new IllegalStateException("Failed to create Metadata Container. " + containerSaveResults.head.getErrors.head.getMessage)
              }
              newContainer
        }

        try {
            codeBlock(container)
        } finally {
            deleteMetadataContainer(session, sessionData)
        }
    }

}
trait Processor extends ProcessorBase {
    def save(session: SfdcSession, sessionData: SessionData)

    protected def saveApexMembers(session: SfdcSession, sessionData: SessionData, container: MetadataContainer,
                                    members: Iterable[SObject]) {

        if (!members.isEmpty) {
            val saveResults = session.create(members.toArray)
            //val saveResults = session.update(members.toArray)
            val res = saveResults.head
            if (res.isSuccess) {
                val request = new ContainerAsyncRequest()
                request.setIsCheckOnly(session.getConfig.isCheckOnly)
                request.setMetadataContainerId(container.getId)
                val requestResults = session.create(Array(request))
                for (res <- requestResults) {
                    if (res.isSuccess) {
                        val requestId = res.getId
                        val soql = "SELECT Id, State, CompilerErrors, ErrorMsg FROM ContainerAsyncRequest where id = '" + requestId + "'"
                        val asyncQueryResult = session.query(soql)
                        if (asyncQueryResult.getSize > 0) {
                            var _request = asyncQueryResult.getRecords.head.asInstanceOf[ContainerAsyncRequest]
                            while ("Queued" == _request.getState) {
                                Thread.sleep(2000)
                                _request = session.query(soql).getRecords.head.asInstanceOf[ContainerAsyncRequest]
                            }
                            processSaveResult(sessionData, _request, members)
                        }
                    } else {
                        throw new IllegalStateException("Failed to create ContainerAsyncRequest. " + res.getErrors.head.getMessage)
                    }
                }
            } else {
                throw new IllegalStateException("Failed to create Apex Member(s). " + res.getErrors.head.getMessage)
            }

            sessionData.store()
        }
    }

    private def processSaveResult(sessionData: SessionData, request: ContainerAsyncRequest, members: Iterable[SObject]) {

        request.getState match {
            case "Completed" =>
                logger.debug("Request succeeded")
                for(m <- members) {
                    val id = Member.toMember(m).getContentEntityId
                    sessionData.getKeyById(id) match {
                        case Some(key) =>
                            sessionData.setField(key, "LastSyncDateLocal", System.currentTimeMillis.toString)
                        case None => throw new IllegalStateException("Failed to fetch session data for using Id=" + id)
                    }
                }
            case state =>
                logger.error("Request Failed with status: " + state)
                throw new IllegalStateException("Failed to send Async Request. Status= " + state)
        }
    }

    /**
     * using stored session data figure out what files have changed
     * @return
     */
    type ChangedFiles = Map[TypeHelper, List[File]]

    def isModified(sessionData: SessionData, helper: TypeHelper, f: File): Boolean = {
        val key = helper.getKey(f)
        sessionData.getField(key, "LastSyncDateLocal") match {
            case Some(x) =>
                f.lastModified > x.toLong && None != sessionData.getField(key, "Id")
            //case None => throw new IllegalStateException("Workspace is out of date. Please refresh before continuing")
            case _ =>
                //if there is no session data for existing file then this file must be Created
                throw new IllegalStateException("New files must be saved before Modified ones")
        }
    }

    /**
     * @return Set of session-data-keys of all files that are older then their remote version
     */
    def getFilesOlderThanRemote(session: SfdcSession, sessionData: SessionData, changedFileMap: ChangedFiles): Iterable[String] = {

        def checkIfRemoteIsNewer(key: String, lastModifiedDate: Option[Long]) = {
            val localLMD = sessionData.getField(key, "LastModifiedDate") match {
                case Some(x) => x.toLong
                case None => 0
            }
            lastModifiedDate match {
              case Some(remoteLMD) => remoteLMD > localLMD
              case None => false
            }
        }

        val newerFiles  =
            for (helper <- changedFileMap.keys) yield {

                val files = changedFileMap(helper)
                val remoteLMDByKeyMap = getRemoteLastModifiedDates(session, sessionData, helper, files)

                val outdatedFiles = files.filter(f => checkIfRemoteIsNewer(helper.getKey(f), remoteLMDByKeyMap.get(helper.getKey(f)) ))
                val keysOfOutdatedFiles = outdatedFiles.map(f => helper.getKey(f))
                keysOfOutdatedFiles
            }
        newerFiles.flatten
    }

    def reloadRemoteLastModifiedDate(session: SfdcSession, sessionData: SessionData, changedFileMap: ChangedFiles) {
        for (helper <- changedFileMap.keys) yield {

            val pairs = getRemoteLastModifiedDates(session, sessionData, helper, changedFileMap(helper))
            for ((key, remoteLMD) <- pairs) {
                sessionData.setField(key, "LastModifiedDate", remoteLMD.toString)
            }

        }
        sessionData.store()
    }

    /**
     * @return Map(session-data-key -> LastModifiedDate mills)
     */
    def getRemoteLastModifiedDates(session: SfdcSession, sessionData: SessionData, helper: TypeHelper, files: List[File]): Map[String, Long] = {
        val names = files.map(f => helper.getName(f)).toList.mkString("','")
        if (!names.isEmpty) {
            val queryRes = session.query("select Id, Name, LastModifiedDate from " + helper.typeName + " where Name in ('" + names + "')")
            queryRes.getRecords.map (rec => helper.getKey(rec) -> helper.getLastModifiedDate(rec)).toMap
        } else Map()
    }

    def refresh(sfdcSession: SfdcSession, sessionData: SessionData)
}

object Processor extends ProcessorBase{
    val containerPrefix = "tooling-force.com"
    def getProcessor(appConfig: Config): Processor = {

        val file = new File(appConfig.resourcePath)
        if (file.isDirectory)
            new PackageProcessor(appConfig, file)
        else
            new FileProcessor(appConfig, file)
    }
}

class FileProcessor(appConfig: Config, resource: File) extends Processor {
    def save(session: SfdcSession, sessionData: SessionData) {
        val helper = TypeHelpers.getHelper(resource)

        withMetadataContainer(session, sessionData) { container =>
            sessionData.getField(helper.getKey(resource), "Id") match {
                case Some(x) => update(session, sessionData, helper, container)
                case None => create(session, sessionData, helper, container)
            }
        }
        sessionData.store()
    }

    def update(session: SfdcSession, sessionData: SessionData, helper: TypeHelper, container: MetadataContainer) = {
        //val container = getMetadataContainer(session, sessionData)
        saveApexMembers(session, sessionData, container, Array(helper.getMemberInstance(sessionData, resource)))
    }

    def create(session: SfdcSession, sessionData: SessionData, helper: TypeHelper, container: MetadataContainer) = {

        val apexObj = helper.newSObjectInstance(resource)
        val saveResults = session.create(Array(apexObj))
        for (res <- saveResults) {
            if (res.isSuccess) {
                //store Id in session
                sessionData.setField(helper.getKey(apexObj), "Id", res.getId)
            } else /* if (!res.getSuccess) */{
                val statusCode = res.getErrors.head.getStatusCode
                statusCode match {
                    case StatusCode.DUPLICATE_VALUE =>
                        //file already exists
                        logger.error("Local project appears to be out of sync. File " + resource.getName + " is marked as new in local version but it already exists in SFDC")
                        logger.error("Refresh your local project to sync with SFDC")
                    case _ => throw new IllegalStateException("failed to create file " + resource + "; " + res.getErrors.head)
                }
                logger.error("" + res.getErrors.head)
            }
        }
    }

    def refresh(sfdcSession: SfdcSession, sessionData: SessionData) {

        val helper = TypeHelpers.getHelper(resource)
        val queryResult = sfdcSession.query(helper.getContentSOQL + " where Name='" + helper.getName(resource) + "'")
        if (queryResult.getSize >0) {
            val record: SObject = queryResult.getRecords.head
            val data = helper.getValueMap(record)
            //save file content
            helper.bodyToFile(appConfig, record)
            //stamp file save time
            val localMills = System.currentTimeMillis.toString
            val key = helper.getKey(record)
            sessionData.setData(key, data ++ Map("LastSyncDateLocal" -> localMills))
            sessionData.store()
        }
    }
}

class PackageProcessor(appConfig: Config, srcDir: File) extends Processor {

    def getModifiedFiles(sessionData: SessionData): ChangedFiles = {
        def iter (helpers: List[TypeHelper], res: ChangedFiles): ChangedFiles = {
            helpers match {
                case Nil => res
                case helper :: xs =>
                    val files = helper.listFiles(srcDir)
                    iter(xs, res ++ Map(helper -> files.filter(isModified(sessionData, helper, _)).toList))

            }
        }
        iter(TypeHelpers.list, Map())
    }

    def save(session: SfdcSession, sessionData: SessionData) {
        val changedFiles = create(session, sessionData)
        reloadRemoteLastModifiedDate(session, sessionData, changedFiles)

        withMetadataContainer(session, sessionData) { container =>
            val changedFiles = update(session, sessionData, container)
            reloadRemoteLastModifiedDate(session, sessionData, changedFiles)
        }

    }
    def update(session: SfdcSession, sessionData: SessionData, container: MetadataContainer):ChangedFiles = {
        val changedFileMap = getModifiedFiles(sessionData)
        //check if remote version of these files is newer
        val newerFileKeys = getFilesOlderThanRemote(session, sessionData, changedFileMap)
        if (!newerFileKeys.isEmpty) {
            logger.info("Remote file(s) newer than local")
            for( key <- newerFileKeys ) {
                //val data = sessionData.getData(key)
                val values = key.split('.') //split to type name and name
                logger.info(values(0) + ": " + values(1))
            }
            Map()
        } else {
            //iterate through changedFileMap and return list of ApexComponentMember objects
            //val container = getMetadataContainer(session, sessionData)
            val members = for (helper <- changedFileMap.keys; f <- changedFileMap(helper)) yield {
                helper.getMemberInstance(sessionData, f)
            }
            //add each file into MetadataContainer and if container update is successful then record serverMills
            //in session data as LastSyncDate for each of successful files
            saveApexMembers(session, sessionData, container, members)
            changedFileMap
        }
    }
    def create(session: SfdcSession, sessionData: SessionData):ChangedFiles = {
        val changedFileMap = getCreatedFiles(sessionData)
        //iterate through changedFileMap and return list of Apex[Class/Page...] objects
        val objects = for (helper <- changedFileMap.keys; f <- changedFileMap(helper)) yield {
            helper.newSObjectInstance(f)
        }
        if (!objects.isEmpty) {
            val objectsArray = objects.toArray
            val saveResults = session.create(objectsArray)
            var i = 0
            for (res <- saveResults) {
                val apexObj = objectsArray(i)
                val helper = TypeHelpers.getHelper(apexObj)
                if (res.isSuccess) {
                    val key = helper.getKey(apexObj)
                    //store Id in session

                    apexObj.setId(res.getId)
                    val data = helper.getValueMap(apexObj)
                    sessionData.setData(key, data)
                    sessionData.setField(key, "LastSyncDateLocal", System.currentTimeMillis.toString)
                    sessionData.store()
                } else /* if (!res.getSuccess) */{
                    val statusCode = res.getErrors.head.getStatusCode
                    val resourceName = helper.getName(apexObj)
                    statusCode match {
                        case StatusCode.DUPLICATE_VALUE =>
                            //file already exists
                            logger.error("Local project appears to be out of sync. File " + resourceName+ " is marked as new in local version but it already exists in SFDC")
                            logger.error("Refresh your local project to sync with SFDC")
                            throw new IllegalStateException("Refresh your local project to sync with SFDC")
                        case _ => throw new IllegalStateException("failed to create file " + resourceName + "; " + res.getErrors.head)
                    }
                    logger.error("" + res.getErrors.head)
                }
                i += 1
            }
            sessionData.store()
        }
        changedFileMap
    }

    def getCreatedFiles(sessionData: SessionData): ChangedFiles = {
        def iter (helpers: List[TypeHelper], res: ChangedFiles): ChangedFiles = {
            helpers match {
                case Nil => res
                case helper :: xs =>
                    val files = helper.listFiles(srcDir)
                    iter(xs, res ++ Map(helper -> files.filter(isCreated(sessionData, helper, _)).toList))

            }
        }
        iter(TypeHelpers.list, Map())
    }
    def isCreated(sessionData: SessionData, helper: TypeHelper, f: File): Boolean = {
        sessionData.getField(helper.getKey(f), "Id") match {
            case None => true
            case Some(x) => false
        }
    }

    def refresh(sfdcSession: SfdcSession, sessionData: SessionData) {

        for (helper <- TypeHelpers.list) {
            //load keys for all locally available resources of current type
            val allResourceKeys = helper.listFiles(srcDir).map(f => helper.getKey(f))
            val allResourcesKeySet = collection.mutable.Set(allResourceKeys.toSeq: _*)

            val queryResult = sfdcSession.query(helper.getContentSOQL)
            if (queryResult.getSize >0) {
                do {
                    for (record: SObject <- queryResult.getRecords) {
                        val data = helper.getValueMap(record)
                        //save file content
                        helper.bodyToFile(appConfig, record)
                        //stamp file save time
                        val localMills = System.currentTimeMillis.toString
                        val key = helper.getKey(record)
                        sessionData.setData(key, data ++ Map("LastSyncDateLocal" -> localMills))
                        allResourcesKeySet.remove(key)

                    }
                }  while (!queryResult.isDone)
            }
            //now for all keys which are still in allResourcesKeySet we can safely assume that they are "new" files
            for (key <- allResourcesKeySet) {
                sessionData.clearField(key, "Id")
            }
        }
        deleteMetadataContainer(sfdcSession, sessionData)
        sessionData.store()
    }
}

object PackageProcessor extends ProcessorBase {
    def unapply(resourcePath: String): Option[File] = {
        if (isDirectory(resourcePath) && resourcePath.matches(""".*src[\\|/]?$""")) {
            val srcDir = new File(resourcePath)
            Some(srcDir)
        } else {
            None
        }
    }
}
