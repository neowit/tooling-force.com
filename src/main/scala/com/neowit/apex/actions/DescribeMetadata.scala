package com.neowit.apex.actions

import com.neowit.apex.actions.tooling.AuraMember
import com.sforce.soap.metadata.{ListMetadataQuery, DescribeMetadataObject}
import com.neowit.apex.Session
import scala.collection.mutable
import java.io.{PrintWriter, File}
import com.neowit.utils.FileUtils
import scala.util.{Failure, Success, Try}

import spray.json._
import DefaultJsonProtocol._
import com.neowit.utils.JsonUtils._

object DescribeMetadata {
    private var describeMetadataObjectMap:Map[String, DescribeMetadataObject] = Map()

    /**
     * @return Map: XmlTypeName -> DescribeMetadataObject
     */
    def getMap(session: Session): Map[String, DescribeMetadataObject] = {
        if (describeMetadataObjectMap.isEmpty) {
            val describer = new DescribeMetadata().load[DescribeMetadata](session)
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

    /**
     * @return Map: DirName -> DescribeMetadataObject
     */
    def getDescribeByDirNameMap(session: Session): Map[String, DescribeMetadataObject] = {
        val objectDescribeByXmlTypeName = getMap(session)
        //turn XmlName -> DescribeMetadataObject (XmlName, DirectoryName, ...)
        //into DirectoryName -> DescribeMetadataObject
        val describeByDirName = objectDescribeByXmlTypeName.filter(pair => !pair._2.getDirectoryName.isEmpty).map(
            pair => pair._2.getDirectoryName -> pair._2
        )
        describeByDirName
    }

    private var xmlNameBySuffix:Map[String, String] = Map()

    /**
     *
     * @param suffix - file extension, e.g. 'cls'
     * @return
     */
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
        else if (file.getName.endsWith("-meta.xml"))
            true
        else
            getXmlNameBySuffix(session, FileUtils.getExtension(file)) match {
                case Some(x) => true
                case None => AuraMember.isSupportedType(file)
            }
    }


    /**
     * using provided xmlName try to find dirName and Suffix
     * @param xmlName - e.g. "ApexClass"
     * @return
     */
    def getDirAndSuffix(session: Session, xmlName: String):Option[(String, String)] = {
        getMap(session).get(xmlName) match {
            case Some(describeObject) => Some((describeObject.getDirectoryName, describeObject.getSuffix))
            case None => None
        }
    }

    /**
     * using file name get apex folder (relatively src/) where this file belongs
     * @param file - e.g. MyClass.cls
     * @return - for MyClass.cls result will be "classes"
     *         if provided file does not have valid apex file extension then result is None
     */
    def getApexFolderNameByFile(session: Session, file: File): Option[String] = {
        if (isValidApexFile(session, file)) {
            if ("package.xml" == file.getName) {
                return Some("") //package.xml belongs to src/ folder
            }

            val extension = if (file.getName.endsWith("-meta.xml")) {
                val nameWithoutMetaXml = file.getName.substring(0, file.getName.length - "-meta.xml".length)
                val extStart = nameWithoutMetaXml.lastIndexOf(".")
                if (extStart > 0) {
                    nameWithoutMetaXml.drop(extStart + 1)
                } else {
                    ""
                }
            }   else FileUtils.getExtension(file)

            if (extension.nonEmpty) {
                getXmlNameBySuffix(session, extension) match {
                  case Some(xmlName) =>
                      getMap(session).get(xmlName) match {
                          case Some(describeObject) => return Some(describeObject.getDirectoryName)
                          case None => None
                      }
                  case None => return None
                }

            }
        }
        None
    }
}
/**
 * 'describeMetadata' action saves result of describeMetadata call in JSON format
 * Extra command line params:
 * --allMetaTypesFilePath - path to file where results shall be saved
 */
class DescribeMetadata extends ApexActionWithReadOnlySession {

    case class MetadataTypeJSON(XMLName: String, HasMetaFile: Boolean, Suffix: String, ChildObjects: List[String], DirName: String, InFolder: Boolean)

    object MetadataDescriptionJsonProtocol extends DefaultJsonProtocol {
        implicit val singleMetadataTypeFormat: JsonFormat[MetadataTypeJSON] = lazyFormat(jsonFormat6(MetadataTypeJSON))
    }

    override def getHelp: ActionHelp = new ActionHelp {
        override def getExample: String = ""

        override def getParamDescription(paramName: String): String = paramName match {
            case "allMetaTypesFilePath" => "--allMetaTypesFilePath - path to file where results shall be saved"
        }

        override def getParamNames: List[String] = List("allMetaTypesFilePath")

        override def getSummary: String = "saves result of describeMetadata call in JSON format"

        override def getName: String = "describeMetadata"
    }


    def loadFromFile: Map[String, DescribeMetadataObject] = {

        val describeMetadataObjectMap = new mutable.HashMap[String, DescribeMetadataObject]

        for (line <- FileUtils.readFile(storedDescribeMetadataResultFile).getLines()) {
            //JSON.parseFull(line)
            val jsonAst = JsonParser(line)
            val data = jsonAst.convertTo[MetadataTypeJSON](MetadataDescriptionJsonProtocol.singleMetadataTypeFormat)
            val descrObj = new DescribeMetadataObject()
            descrObj.setDirectoryName(data.DirName)
            descrObj.setInFolder(data.InFolder)
            descrObj.setMetaFile(data.HasMetaFile)
            descrObj.setSuffix(data.Suffix)
            descrObj.setXmlName(data.XMLName)
            val xmlNames = data.ChildObjects
            descrObj.setChildXmlNames(xmlNames.toArray)

            describeMetadataObjectMap += data.XMLName -> descrObj
        }
        describeMetadataObjectMap.toMap
    }

    private def storeDescribeResult(file: File, lines: Iterator[String]) {
        val writer = new PrintWriter(file)
        lines.foreach(writer.println)
        writer.close()
    }

    /**
      * Local copy of Describe Metadata result
      */
    private lazy val storedDescribeMetadataResultFile:File  = {
        val file = new File(getSessionConfig.sessionFolder, "describeMetadata-result.js")
        if (!file.exists) {
            file.createNewFile()
        }
        file
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
                                "ChildObjects" -> _describeObject.getChildXmlNames.toJson,
                                "DirName" -> _describeObject.getDirectoryName,
                                "InFolder" -> _describeObject.isInFolder,
                                "HasMetaFile" -> _describeObject.getMetaFile,
                                "Suffix" -> (if (null != _describeObject.getSuffix) _describeObject.getSuffix else "\"\""),
                                "XMLName" -> _describeObject.getXmlName
                            )
                            linesBuf += data.toJson.compactPrint
                        case None =>
                    }
                }
                storeDescribeResult(storedDescribeMetadataResultFile, linesBuf.iterator)
                //check if user requested alternative response location
                config.getRequiredProperty("allMetaTypesFilePath") match {
                    case Some(allMetaTypesFilePath) =>
                        val userDefinedFile = new File(allMetaTypesFilePath)
                        if (userDefinedFile != storedDescribeMetadataResultFile) {
                            storeDescribeResult(userDefinedFile, linesBuf.iterator)

                        }
                    case None => //no action required
                }

                describeMetadataObjectMap
            case Failure(thrown) => throw thrown
        }
    }
    def getOutputFilePath = config.getRequiredProperty("allMetaTypesFilePath")

    def act() {
        //load from SFDC and dump to local file
        val resMap = loadFromRemote
        responseWriter.println("RESULT=SUCCESS")
        responseWriter.println("RESULT_FILE=" + getOutputFilePath)
        responseWriter.println("FILE_COUNT=" + resMap.size)
    }
}



/**
 * 'listMetadata' action uses type list specified in a file and sends listMetadata() call for specified types
 * Extra command line params:
 * --specificTypes=/path/to/file with types list
 */
class ListMetadata extends ApexActionWithWritableSession {

    override def getHelp: ActionHelp = new ActionHelp {
        override def getExample: String = ""

        override def getParamDescription(paramName: String): String = paramName match {
            case "specificTypes" => "--specificTypes=/path/to/file with Xml types list. Each type must be on its own line"
        }

        override def getParamNames: List[String] = List("specificTypes")

        override def getSummary: String = "retrieve members of metadata types listed in --specificTypes=file and return results in another file"

        override def getName: String = "listMetadata"
    }

    def act(): Unit = {

        val metadataByXmlName = DescribeMetadata.getMap(session)
        //load file list from specified file
        val queries = new mutable.ArrayBuffer[ListMetadataQuery]()
        val typesFile = new File(config.getRequiredProperty("specificTypes").get)
        for (typeName <- FileUtils.readFile(typesFile).getLines()) {
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
        if (resourcesByXmlTypeName.nonEmpty) {
            //dump results to JSON file, with each line looking like this
            //{"CustomTab" : ["Account_Edit", "My_Object__c"]}

            val tempFile = FileUtils.createTempFile("listMetadata", ".js")
            val writer = new PrintWriter(tempFile)
            resourcesByXmlTypeName.foreach{
                case (k, v: Set[String]) if k == null =>
                    logger.trace("key is null for v=" + v)
                case (key: String, values: Set[String]) =>
                    logger.trace("key=" + key)
                    val line = Map(key -> values.toList.toJson).toJson.compactPrint
                    writer.println(line)
            }
            writer.close()
            responseWriter.println("RESULT_FILE=" + tempFile.getAbsolutePath)
        }
    }
}

