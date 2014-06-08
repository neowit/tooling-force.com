package com.neowit.apex.actions.tooling

import com.neowit.apex.{MetadataType, Session}
import java.io.File
import com.neowit.apex.actions.{DescribeMetadata, DeployModified}
import com.sforce.soap.tooling.{SObject, ContainerAsyncRequest, MetadataContainer, SaveResult}
import scala.util.parsing.json.JSON
import com.neowit.utils.ResponseWriter.Message
import com.neowit.utils.{FileUtils, ZuluTime, ResponseWriter}
import scala.concurrent._
import scala.util.parsing.json.JSONArray
import scala.util.parsing.json.JSONObject
import com.neowit.utils.ResponseWriter.MessageDetail
import com.sforce.ws.bind.XmlObject

class SaveError(msg: String) extends Error(msg: String)
/**
 * Unlike SaveModified tries to leverage ToolingApi and works only in Dev Orgs and Sandboxes
 *
 * @param session - SFDC session
 * Extra command line params:
 * --ignoreConflicts=true|false (defaults to false) - if true then skip ListConflicting check
 * --checkOnly=true|false (defaults to false) - if true then do a dry-run without modifying SFDC
 * --testsToRun=* OR "comma separated list of class.method names",
 * e.g. "ControllerTest.myTest1, ControllerTest.myTest2, HandlerTest1.someTest, Test3.anotherTest1"
 *
 * class/method can be specified in two forms
 * - ClassName[.methodName] -  means specific method of specific class
 * - ClassName -  means *all* test methodsToKeep of specific class
 *
 * if --testsToRun=* (star) then run all tests in all classes (containing testMethod or @isTest ) in
 * the *current* deployment package
 * --reportCoverage=true|false (defaults to false) - if true then generate code coverage file
 *
 */
class SaveModified(session: Session) extends DeployModified(session: Session) {
    val CONTAINER_PREFIX = "tooling-force.com"

    //we can use ToolingApi in following cases
    //1. there are not -meta.xml files
    //2. there are no new files
    //3. all files are supported by Tooling API
    def canUseTooling(files: List[File]): Boolean = {
        val hasMeta = None != files.find(_.getName.endsWith("-meta.xml"))
        if (hasMeta) {
            return false
        }
        //check if there are new files
        val hasNewFile = None != files.find(f => {
            val key = session.getKeyByFile(f)
            "" == session.getData(key).getOrElse("Id", "")
        })

        if (hasNewFile) {
            return false
        }
        //check if all files supported by tooling api
        val hasUnsupportedType = None != files.find(f =>  !ApexMember.isSupportedType(f, session) )
        !hasUnsupportedType
    }

    def deleteMetadataContainer(session: Session) {
        getExistingMetadataContainer(session)  match {
            case Some(container) =>
                try {
                    session.removeData("MetadataContainer")
                    session.deleteTooling(container.getId)
                    logger.debug("Deleted MetadataContainer; Id=" + container.getId)
                } catch {
                    case ex:Throwable => //do not really care why delete failed
                        logger.debug("Could not delete MetadataContainer. " + ex.getMessage)
                }
                session.storeSessionData()
            case None => //nothing to delete
        }
    }

    def getExistingMetadataContainer(session: Session): Option[MetadataContainer] = {
        session.getData("MetadataContainer")  match {
            case data: Map[String, _] if "" != data.getOrElse("Id", "").asInstanceOf[String] =>
                val containerId = data("Id").asInstanceOf[String]
                //logger.debug("Re-use Existing MetadataContainer; Id=" + containerId)
                val container = new MetadataContainer()
                container.setId(containerId)
                container.setName(data.getOrElse("Name", "").asInstanceOf[String])
                Some(container)
            case _ => None
        }
    }

    def withMetadataContainer(session: Session)(codeBlock: (MetadataContainer) => Any) = {

        try {
            codeBlock(getMetadataContainer(session))
        } catch {
            case ex:Throwable =>
                logger.debug(ex)
                //delete container
                deleteMetadataContainer(session)
                //and try again
                codeBlock(getMetadataContainer(session))
        } finally {
            deleteMetadataContainer(session)
        }
    }
    def getMetadataContainer(session: Session): MetadataContainer = {
        val container = getExistingMetadataContainer(session)  match {
            case Some(_container) => _container
            case None =>
                val newContainer = new MetadataContainer()
                newContainer.setName(CONTAINER_PREFIX + session.getUserId.drop(3))//reduce length to fit in 32 characters
            val containerSaveResults: Array[SaveResult] = session.createTooling(Array(newContainer))
                if (containerSaveResults.head.isSuccess) {
                    newContainer.setId(containerSaveResults.head.getId)
                    session.setData("MetadataContainer", Map("Name" -> newContainer.getName, "Id" -> newContainer.getId))
                    session.storeSessionData()
                    logger.debug("Created new MetadataContainer; Id=" + newContainer.getName + " - Id=" + newContainer.getId)
                } else {
                    val msg = "Failed to create Metadata Container. " + containerSaveResults.head.getErrors.head.getMessage
                    throw new IllegalStateException(msg)
                }
                newContainer
        }
        container
    }

    private val ONE_SECOND = 1000
    override def deploy(files: List[File], updateSessionDataOnSuccess: Boolean) {
        logger.debug("Entered deploy()")
        if (!canUseTooling(files)) {
            //can not use tooling, fall back to metadata version - DeployModified
            super.deploy(files, updateSessionDataOnSuccess)
        } else {
            withMetadataContainer(session) { container =>
                val membersMap = (for(f <- files) yield {
                    val member = ApexMember.getInstance(f, session)
                    member.setMetadataContainerId(container.getId)
                    (member, f)
                }).toMap

                val waitTimeMilliSecs = config.getProperty("pollWaitMillis").getOrElse("" + (ONE_SECOND * 3)).toInt
                val saveResults = session.createTooling(membersMap.map(_._1.asInstanceOf[SObject]).toArray)
                val res = saveResults.head
                if (res.isSuccess) {
                    val request = new ContainerAsyncRequest()
                    request.setIsCheckOnly(session.getConfig.isCheckOnly)
                    request.setMetadataContainerId(container.getId)
                    val requestResults = session.createTooling(Array(request))
                    for (res <- requestResults) {
                        if (res.isSuccess) {
                            val requestId = res.getId
                            val soql = "SELECT Id, State, CompilerErrors, ErrorMsg FROM ContainerAsyncRequest where id = '" + requestId + "'"
                            val asyncQueryResult = session.queryTooling(soql)
                            if (asyncQueryResult.getSize > 0) {
                                var _request = asyncQueryResult.getRecords.head.asInstanceOf[ContainerAsyncRequest]
                                var lastReportTime = System.currentTimeMillis()
                                var attempts = 0
                                while ("Queued" == _request.getState) {
                                    val reportAttempt = (System.currentTimeMillis() - lastReportTime) > (ONE_SECOND * 3)
                                    blocking {
                                        Thread.sleep(waitTimeMilliSecs)
                                        _request = session.queryTooling(soql).getRecords.head.asInstanceOf[ContainerAsyncRequest]
                                    }
                                    //report only once every 3 seconds
                                    if (reportAttempt) {
                                        logger.info("waiting result, poll #" + attempts)
                                        lastReportTime = System.currentTimeMillis()
                                    } else {
                                        logger.trace("waiting result, poll #" + attempts)
                                    }
                                    attempts += 1
                                }
                                processSaveResult(_request, membersMap, updateSessionDataOnSuccess)
                            }
                        } else {
                            throw new IllegalStateException("Failed to create ContainerAsyncRequest. " + res.getErrors.head.getMessage)
                        }
                    }
                } else {
                    throw new IllegalStateException("Failed to create Apex Member(s). " + res.getErrors.head.getMessage)
                }


            }
            session.storeSessionData()
        }

    }

    /**
     * when using Tooling try to avoid Metadata.retrieve() as it may be quite slow
     * this method will override its parent in ApexDeploy and uses simple query() calls to load LastModifiedDate
     * @param files - list of files to check for conflicts
     * @return
     */
    override def getFilesNewerOnRemote(files: List[File]): Option[List[Map[String, Any]]] = {
        val modificationDataByFile = getFilesModificationData(files)

        val dataOrNone: List[Option[Map[String, Any]]] = for (file <- files) yield {
            modificationDataByFile.get(file)  match {
                case Some(data) =>
                    val localMillis = ZuluTime.deserialize(data("Local-LastModifiedDateStr").toString).getTimeInMillis
                    val remoteMillis = ZuluTime.deserialize(data("Remote-LastModifiedDateStr").toString).getTimeInMillis

                    if (localMillis < remoteMillis) Some(data) else None

                case None => None
            }
        }
        val res = dataOrNone.filter(_ != None).map(_.get)
        if (res.isEmpty) None else Some(res)
    }
    /**
     * find remote modification Date + ById for all provided files
     * @param files - list of files to retrieve data for
     * @return
     */
    private def getFilesModificationData(files: List[File]): Map[File, Map[String, Any]] = {
        val filesByExtension = files.groupBy(f => FileUtils.getExtension(f))

        val dataByFileCol = filesByExtension.keys.par.map(extension => {
            DescribeMetadata.getXmlNameBySuffix(session, extension) match {
                case Some(xmlType) =>
                    val res = getFilesModificationData(xmlType, filesByExtension(extension))
                    res
                case None => Map[File, Map[String, Any]]() //did not recognise extension
            }

        })
        //now convert ParIterable of maps into a single map
        val dataByFile = dataByFileCol.foldLeft(Map[File, Map[String, Any]] ())(_ ++ _)

        dataByFile.toMap
    }

    /**
     * retrieve modification data for single XML Type
     * @param xmlType, e.g. ApexClass
     * @param filesOfSameType - all files MUST be of the same XML Type
     * @return
     */
    private def getFilesModificationData(xmlType: String, filesOfSameType: List[File]): Map[File, Map[String, Any]] = {
        val fileById = collection.mutable.HashMap[String, File]()
        for (file <- filesOfSameType) {
            val key = session.getKeyByFile(file)
            session.getData(key).get("Id")  match {
                case Some(id) => fileById += id.toString -> file
                case None =>
            }
        }
        val ids = fileById.keys
        if (!ids.isEmpty) {
            val queryResult = session.query("select Id, Name, LastModifiedDate, LastModifiedBy.Name, LastModifiedById from " + xmlType
                + " where Id in (" + ids.map("'" + _ + "'").mkString(",") + ")")
            val records = queryResult.getRecords

            val dataByFile = records.map(record => {
                val file = fileById(record.getId)
                //2014-02-24T20:35:59.000Z
                val lastModifiedStr = record.getField("LastModifiedDate").toString
                val lastModifiedDate = ZuluTime.deserialize(lastModifiedStr)
                val millsLocal = session.getData(session.getKeyByFile(file)).getOrElse("LastModifiedDateMills", 0).toString.toLong

                file -> Map(
                    "file" -> file,
                    "LastModifiedByName" -> record.getField("LastModifiedBy").asInstanceOf[XmlObject].getChild("Name").getValue,
                    "LastModifiedById" -> record.getField("LastModifiedById"),
                    "Remote-LastModifiedDateStr" -> ZuluTime.formatDateGMT(lastModifiedDate),
                    "Local-LastModifiedDateStr" -> ZuluTime.formatDateGMT(ZuluTime.toCalendar(millsLocal))
                )

            })
            dataByFile.toMap
        } else {
            Map()
        }
    }

    private def processSaveResult(request: ContainerAsyncRequest, membersMap: Map[ApexMember, File], updateSessionDataOnSuccess: Boolean) {

        request.getState match {
            case "Completed" =>
                logger.debug("Request succeeded")
                if (updateSessionDataOnSuccess) {
                    val modificationDataByFile = getFilesModificationData(membersMap.values.toList)
                    for (member <- membersMap.keys) {

                        val f = membersMap(member)
                        modificationDataByFile.get(f)  match {
                            case Some(data) =>
                                val key = session.getKeyByFile(f)
                                val lastModifiedDate = ZuluTime.deserialize(data("Remote-LastModifiedDateStr").toString)
                                val newData = MetadataType.getValueMap(config, f, member.xmlType, Some(member.getContentEntityId), lastModifiedDate, fileMeta = None)
                                val oldData = session.getData(key)
                                session.setData(key, oldData ++ newData)
                            case None =>
                        }

                    }
                }
                config.responseWriter.println("RESULT=SUCCESS")
                config.responseWriter.println("FILE_COUNT=" + membersMap.size)
                if (!config.isCheckOnly) {
                    config.responseWriter.startSection("SAVED FILES")
                    membersMap.values.foreach(f => config.responseWriter.println(f.getName))
                    config.responseWriter.endSection("SAVED FILES")
                }

            case "Failed" =>
                logger.debug("Request failed")
                responseWriter.println("RESULT=FAILURE")
                config.responseWriter.startSection("ERROR LIST")
                if ("[]" != request.getCompilerErrors && !request.getCompilerErrors.isEmpty) {
                    JSON.parseRaw(request.getCompilerErrors) match {
                        case Some(x) if x.isInstanceOf[JSONArray] =>
                            logger.debug(x)
                            //display errors both as messages and as ERROR: lines
                            val componentFailureMessage = new Message(ResponseWriter.WARN, "Compiler errors")
                            responseWriter.println(componentFailureMessage)

                            for (err <- x.asInstanceOf[JSONArray].list) {
                                val errObj = err.asInstanceOf[JSONObject].obj
                                logger.debug(errObj)
                                val xmlType = getErrorObjectValue(errObj, "extent").get.asInstanceOf[String]
                                val describeMetadataResult = DescribeMetadata.getMap(session).get(xmlType).get
                                //val directory = describeMetadataResult.getDirectoryName
                                val extension = describeMetadataResult.getSuffix
                                val fName = getErrorObjectValue(errObj, "name").get.asInstanceOf[String]
                                val line = getErrorObjectValue(errObj, "line") match {
                                    case Some(l) => l.asInstanceOf[BigDecimal].toInt
                                    case None => -1
                                }
                                val column = getErrorObjectValue(errObj, "col") match {
                                    case Some(c) => c.asInstanceOf[BigDecimal].toInt
                                    case None => -1
                                }
                                val problem = getErrorObjectValue(errObj, "problem").get.asInstanceOf[String]
                                //val id = errObj("id")

                                val filePath =  if (!extension.isEmpty) session.getRelativeFilePath(fName, extension) match {
                                    case Some(_filePath) => _filePath
                                    case _ => ""
                                }
                                val problemType = "CompileError"
                                responseWriter.println("ERROR", Map("type" -> problemType, "line" -> line, "column" -> column, "filePath" -> filePath, "text" -> problem))
                                responseWriter.println(new MessageDetail(componentFailureMessage, Map("type" -> problemType, "filePath" -> filePath, "text" -> problem)))
                            }
                        case Some(x) if x.isInstanceOf[JSONObject] =>
                            logger.debug(x)
                            val err = x.asInstanceOf[JSONObject]
                            logger.debug(err)

                        case None =>
                    }
                } else {
                    //general error
                    //display errors both as messages and as ERROR: lines
                    val generalFailureMessage = new Message(ResponseWriter.WARN, "General failure")
                    responseWriter.println(generalFailureMessage)
                    val problem = request.getErrorMsg match {
                        case "Can't alter metadata in an active org" => //attempt to deploy using Tooling API into Production
                            request.getErrorMsg + "; If you are trying to deploy using Tooling API in Production org then switch to Metadata API."
                        case s => s
                    }
                    responseWriter.println("ERROR", Map("type" -> "Error", "text" -> problem))
                    responseWriter.println(new MessageDetail(generalFailureMessage, Map("type" -> "Error", "text" -> problem)))
                }

            case state =>
                logger.error("Request Failed with status: " + state)
                throw new IllegalStateException("Failed to send Async Request. Status= " + state)
        }
    }

    /**
     * in Summer'14 SFDC broke ContainerAsyncRequest.getCompilerErrors
     * for every field in getCompilerErrors() JSON - SFDC Orgs starting Summer'14 return JSON arrays instead of normal
     * values, even though we are calling SFDC with API v29.0, so have to check value type (Any or JSONArray) first
     * @param errObj - single element (JSONObject) of ContainerAsyncRequest.getCompilerErrors JSONArray
     * @param fName - field name in errObj
     */
    def getErrorObjectValue(errObj: Map[String, Any], fName: String): Option[Any] = {
        errObj.get(fName)  match {
          case Some(x) if x.isInstanceOf[JSONArray] => Some(x.asInstanceOf[JSONArray].list(0))
          case Some(x) => Some(x)
          case None => None
        }
    }
}
