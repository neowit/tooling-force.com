package com.neowit.apex.actions

import com.sforce.soap.metadata.{ListMetadataQuery, DescribeMetadataObject}
import com.neowit.apex.Session
import scala.collection.mutable
import java.io.{PrintWriter, File}
import com.neowit.utils.{ResponseWriter, FileUtils}
import scala.util.parsing.json.{JSONArray, JSONObject, JSON}
import scala.util.{Failure, Success, Try}

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
class DescribeMetadata(session: Session) extends ApexAction(session: Session) {

    override def getExample: String = ""

    override def getParamDescription(paramName: String): String = paramName match {
        case "allMetaTypesFilePath" => "--allMetaTypesFilePath - path to file where results shall be saved"
    }

    override def getParamNames: List[String] = List("allMetaTypesFilePath")

    override def getSummary: String = "saves result of describeMetadata call in JSON format"

    override def getName: String = "decribeMetadata"

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
                    logger.error("Failed to parse line: \n" + line)
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
 * 'listMetadata' action uses type list specified in a file and sends listMetadata() call for specified types
 *@param session - SFDC session
 * Extra command line params:
 * --specificTypes=/path/to/file with types list
 */
class ListMetadata(session: Session) extends ApexAction(session: Session) {

    override def getExample: String = ""

    override def getParamDescription(paramName: String): String = paramName match {
        case "specificTypes" => "--specificTypes=/path/to/file with Xml types list. Each type must be on its own line"
    }

    override def getParamNames: List[String] = List("specificTypes")

    override def getSummary: String = "retrieve members of metadata types listed in --specificTypes=file and return results in another file"

    override def getName: String = "listMetadata"

    def act(): Unit = {

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


        var resourcesByXmlTypeName = Map[String, Set[String]]()
        Try(session.listMetadata(queries.toArray, config.apiVersion)) match {
            case Success(fileProperties) =>
                for (fileProp <- fileProperties) {
                    val typeName = fileProp.getType
                    val resourceName = fileProp.getFullName
                    resourcesByXmlTypeName = addToMap(resourcesByXmlTypeName, typeName, resourceName)
                }
            case Failure(error) => throw error
        }

        responseWriter.println("RESULT=SUCCESS")
        if (!resourcesByXmlTypeName.isEmpty) {
            //dump results to JSON file, with each line looking like this
            //{"CustomTab" : ["Account_Edit", "My_Object__c"]}

            val tempFile = FileUtils.createTempFile("listMetadata", ".js")
            val writer = new PrintWriter(tempFile)
            resourcesByXmlTypeName.foreach{
                case (k, v: Set[String]) if k == null =>
                    logger.trace("key is null for v=" + v)
                case (key: String, values: Set[String]) =>
                    logger.trace("key=" + key)
                    val line = JSONObject(Map(key -> JSONArray(values.toList))).toString(ResponseWriter.defaultFormatter)
                    writer.println(line)
            }
            writer.close()
            responseWriter.println("RESULT_FILE=" + tempFile.getAbsolutePath)
        }
    }
}

