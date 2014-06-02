package com.neowit.apex.actions.tooling

import com.sforce.soap.tooling.DescribeGlobalSObjectResult
import scala.collection.mutable
import com.neowit.apex.Session
import scala.util.{Failure, Success, Try}
import scala.util.parsing.json.{JSON, JSONObject, JSONArray}
import com.neowit.utils.{BasicConfig, ResponseWriter}
import java.io.{PrintWriter, File}
import com.neowit.apex.actions.ApexAction

object DescribeTooling {
    private var describeToolingObjectMap:Map[String, DescribeGlobalSObjectResult] = Map()

    /**
     * @return Map: type Name -> DescribeGlobalSObjectResultt
     */
    def getMap(session: Session): Map[String, DescribeGlobalSObjectResult] = {
        if (describeToolingObjectMap.isEmpty) {
            val describer = new DescribeTooling(session.basicConfig)
            //first try to get metadata description from local file
            val localMap = describer.loadFromFile
            if (localMap.isEmpty) {
                //finally try loading from remote
                val remoteMap = describer.loadFromRemote
                describeToolingObjectMap = remoteMap
            } else {
                describeToolingObjectMap = localMap
            }
        }
        describeToolingObjectMap
    }
}

class DescribeTooling(basicConfig: BasicConfig) extends ApexAction(basicConfig: BasicConfig) {
    override def getExample: String = ""

    override def getParamDescription(paramName: String): String = paramName match {
        case "allToolingTypesFilePath" => "--allToolingTypesFilePath - path to file where results shall be saved"
    }

    override def getParamNames: List[String] = List("allToolingTypesFilePath")

    override def getSummary: String = "saves result of Tooling API describeGlobal() call in JSON format"

    override def getName: String = "DescribeTooling"

    def act() {
        //load from SFDC and dump to local file
        val resMap = loadFromRemote
        responseWriter.println("RESULT=SUCCESS")
        responseWriter.println("RESULT_FILE=" + config.storedDescribeMetadataResultFile.getAbsolutePath)
        responseWriter.println("FILE_COUNT=" + resMap.size)
    }

    private def storeDescribeResult(file: File, lines: Iterator[String]) {
        val writer = new PrintWriter(file)
        lines.foreach(writer.println)
        writer.close()
    }
    def loadFromFile: Map[String, DescribeGlobalSObjectResult] = {
        val describeMetadataObjectMap = new mutable.HashMap[String, DescribeGlobalSObjectResult]

        for (line <- scala.io.Source.fromFile(config.storedDescribeToolingResultFile).getLines()) {
            //JSON.parseFull(line)
            JSON.parseRaw(line)  match {
                case Some(json) =>
                    val data = json.asInstanceOf[JSONObject].obj
                    val descrObj = new DescribeGlobalSObjectResult()
                    descrObj.setActivateable(data.getOrElse("isActivateable", false).asInstanceOf[Boolean])
                    descrObj.setCustom(data.getOrElse("isCustom", false).asInstanceOf[Boolean])
                    descrObj.setCustomSetting(data.getOrElse("isCustomSetting", false).asInstanceOf[Boolean])
                    descrObj.setQueryable(data.getOrElse("isQueryable", false).asInstanceOf[Boolean])
                    descrObj.setKeyPrefix(data.getOrElse("keyPrefix", "").asInstanceOf[String])
                    val name = data.getOrElse("name", "").asInstanceOf[String]
                    descrObj.setName(name)

                    describeMetadataObjectMap += name -> descrObj
                case None =>
                    logger.error("Failed to parse line: \n" + line)
            }

        }
        describeMetadataObjectMap.toMap
    }
    def loadFromRemote: Map[String, DescribeGlobalSObjectResult] = {
        Try(session.describeTooling) match {
            case Success(describeResult) =>
                val describeToolingObjectMap = describeResult.getSobjects.map(describeSObjectResult => (describeSObjectResult.getName, describeSObjectResult)).toMap
                //dump to local file
                val linesBuf = new scala.collection.mutable.ListBuffer[String]
                for (name <- describeToolingObjectMap.keySet) {

                    describeToolingObjectMap.get(name) match {
                        case Some(_describeObject) =>

                            val data = Map(
                                "isActivateable" -> _describeObject.isActivateable,
                                "isCustomSetting" -> _describeObject.isCustomSetting,
                                "isCustom" -> _describeObject.isCustom,
                                "isQueryable" -> _describeObject.getQueryable,
                                "keyPrefix" -> (if (null == _describeObject.getKeyPrefix) "" else _describeObject.getKeyPrefix),
                                "name" -> _describeObject.getName
                            )
                            linesBuf += JSONObject(data).toString(ResponseWriter.defaultFormatter)
                        case None =>
                    }
                }
                storeDescribeResult(config.storedDescribeToolingResultFile, linesBuf.iterator)
                //check if user requested alternative response location
                config.getProperty("allToolingTypesFilePath") match {
                    case Some(allMetaTypesFilePath) =>
                        val userDefinedFile = new File(allMetaTypesFilePath)
                        if (userDefinedFile != config.storedDescribeToolingResultFile) {
                            storeDescribeResult(userDefinedFile, linesBuf.iterator)

                        }
                    case None => //no action required
                }

                describeToolingObjectMap
            case Failure(thrown) => throw thrown
        }

    }
}
