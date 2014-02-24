package com.neowit.apex.actions.tooling

import com.neowit.apex.{MetadataType, Session}
import java.io.File
import com.neowit.apex.actions.{DescribeMetadata, ListModified, DeployModified}
import com.sforce.soap.tooling.{SObject, ContainerAsyncRequest, MetadataContainer, SaveResult}
import scala.util.parsing.json.{JSON, JSONArray, JSONObject}
import com.neowit.utils.ResponseWriter.{Message, MessageDetail}
import com.neowit.utils.{ZuluTime, FileUtils, ResponseWriter}

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
        val toolingSObjectNames = DescribeTooling.getMap(session).keySet
        val hasUnsupportedType = None != files.find(f => {
                                                            val key = session.getKeyByFile(f)
                                                            !toolingSObjectNames.contains(session.getData(key).
                                                                getOrElse("Type", "Unsupported").asInstanceOf[String])
                                                        })
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

    override def deploy(files: List[File], updateSessionDataOnSuccess: Boolean) {
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

                val saveResults = session.createTooling(membersMap.map(_._1.getInstance).toArray)
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
                                while ("Queued" == _request.getState) {
                                    Thread.sleep(2000)
                                    _request = session.queryTooling(soql).getRecords.head.asInstanceOf[ContainerAsyncRequest]
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


    private def processSaveResult(request: ContainerAsyncRequest, membersMap: Map[ApexMember, File], updateSessionDataOnSuccess: Boolean) {

        request.getState match {
            case "Completed" =>
                logger.debug("Request succeeded")
                if (updateSessionDataOnSuccess) {
                    //here I am making bold assumption that SFDC commits MetadataContainer as a single transaction,
                    // so all files will have the same LastModifiedDate and we can query single file to get LastModifiedDate
                    // for all members of current MetadataContainer
                    val member = membersMap.head._1
                    val xmlType = member.getXmlType
                    val queryResult = session.query("select LastModifiedDate from " + xmlType + " where Id ='" + member.getEntityId + "'")
                    val records = queryResult.getRecords
                    if (!records.isEmpty) {
                        //2014-02-24T20:35:59.000Z
                        val lastModifiedStr = records(0).getField("LastModifiedDate").toString
                        val lastModifiedDate = ZuluTime.deserialize(lastModifiedStr)
                        for (member <- membersMap.keys) {
                            val f = membersMap(member)
                            val key = session.getKeyByFile(f)
                            val newData = MetadataType.getValueMap(config, f, xmlType, Some(member.getEntityId), lastModifiedDate, fileMeta = None)
                            val oldData = session.getData(key)
                            session.setData(key, oldData ++ newData)
                        }
                    }
                }

            case "Failed" =>
                logger.debug("Request failed")
                responseWriter.println("RESULT=FAILURE")
                config.responseWriter.startSection("ERROR LIST")
                if (!request.getCompilerErrors.isEmpty) {
                    JSON.parseRaw(request.getCompilerErrors) match {
                        case Some(x) if x.isInstanceOf[JSONArray] =>
                            logger.debug(x)
                            //display errors both as messages and as ERROR: lines
                            val componentFailureMessage = new Message(ResponseWriter.WARN, "Compiler errors")
                            responseWriter.println(componentFailureMessage)

                            for (err <- x.asInstanceOf[JSONArray].list) {
                                val errObj = err.asInstanceOf[JSONObject].obj
                                logger.debug(errObj)
                                val xmlType = errObj("extent").asInstanceOf[String]
                                val describeMetadataResult = DescribeMetadata.getMap(session).get(xmlType).get
                                val directory = describeMetadataResult.getDirectoryName
                                val extension = describeMetadataResult.getSuffix
                                val fName = errObj("name").asInstanceOf[String]
                                val line = errObj.get("line") match {
                                    case Some(l) => l.asInstanceOf[BigDecimal].toInt
                                    case None => -1
                                }
                                val column = errObj.get("col") match {
                                    case Some(c) => c.asInstanceOf[BigDecimal].toInt
                                    case None => -1
                                }
                                val problem = errObj("problem").asInstanceOf[String]
                                //val id = errObj("id")

                                val filePath = config.srcPath + File.separator + directory + File.separator + fName + "." + extension
                                val problemType = "CompileError"
                                responseWriter.println("ERROR", Map("type" -> problemType, "line" -> line, "column" -> column, "filePath" -> filePath, "text" -> problem))
                                responseWriter.println(new MessageDetail(componentFailureMessage, Map("type" -> problemType, "filePath" -> filePath, "text" -> problem)))
                                /*
                                val helper = TypeHelpers.getHelper(err.asInstanceOf[JSONObject])
                                val directory = helper.directoryName


                                */
                            }
                        case Some(x) if x.isInstanceOf[JSONObject] =>
                            logger.debug(x)
                            val err = x.asInstanceOf[JSONObject]
                            logger.debug(err)

                        case None =>
                    }
                }

            case state =>
                logger.error("Request Failed with status: " + state)
                throw new IllegalStateException("Failed to send Async Request. Status= " + state)
        }
    }

}
