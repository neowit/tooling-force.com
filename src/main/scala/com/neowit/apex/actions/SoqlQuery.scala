package com.neowit.apex.actions

import java.io.File
import com.sforce.ws.bind.XmlObject
//import collection.JavaConverters._
import spray.json._
import DefaultJsonProtocol._

class SoqlQuery extends ApexAction {
    override def act(): Unit = {
        val codeFile = new File(config.getRequiredProperty("queryFilePath").get)
        val soqlQuery = scala.io.Source.fromFile(codeFile).getLines().mkString(" ")
        //dump first batch of results into the output file
        var queryResult = session.query(soqlQuery)
        writeResults(queryResult.getRecords)

        while (!queryResult.isDone) {
            queryResult = session.queryMore(queryResult.getQueryLocator)
            writeResults(queryResult.getRecords)
        }

    }

    override def getHelp: ActionHelp = new ActionHelp {
        override def getExample: String = ""

        override def getParamDescription(paramName: String): String = {
            paramName match {
                case "projectPath" => "--projectPath - full path to project folder"
                case "queryFilePath" => "--queryFilePath - full path to file containing SOQL query to run"
                case "responseFilePath" => "--responseFilePath - path to file where operation result will be reported"
                case "outputFilePath" => "--outputFilePath - path to file where query result will be saved in specified format"
                case "outputFormat" => "--outputFormat [optional] - how query results will be formatted. Accepted values: 'json', 'pipe'(default)"
                case _ => ""
            }
        }

        override def getParamNames: List[String] = List("projectPath", "codeFile", "responseFilePath")

        override def getSummary: String = "execute provided SOQL query and return results"

        override def getName: String = "soqlQuery"
    }

    def writeResults(records: Array[com.sforce.soap.partner.sobject.SObject]): Unit = {
        config.getProperty("outputFormat") .getOrElse("pipe") match {
            case "json" => writeAsJsonLines(records)
            case "pipe" => writeAsPipeSeparatedLines(records)
            case x => throw new ShowHelpException(getHelp, "Invalid outputFormat: " + x)
        }
    }

    /**
     * first two fields of every returned record are
     * - type - SFDC Object Type
     * - Id - SFDC Object Id
     * we do not need them in query result. If User requested Id it will be in fields list as yet another field
     * @param fields - SObject fields iterator
     */
    private def skipTypeAndId(fields: java.util.Iterator[XmlObject]): java.util.Iterator[XmlObject] = {
        if (fields.hasNext) {
            val f1 = fields.next() //skip type
            if ("type" == f1.getName.getLocalPart && fields.hasNext) {
                fields.next() //skip Id
            }
        }
        fields
    }

    private def writeAsJsonLines(records: Array[com.sforce.soap.partner.sobject.SObject]): Unit = {

        var i = 0
        while (i < records.length) {
            val record = records(i)
            val result = Map.newBuilder[String, JsValue]
            val fields = skipTypeAndId(record.getChildren)
            while (fields.hasNext) {
                val field = fields.next()
                //val name = field.getName.getLocalPart
                val value = getFieldValue(field)
                result += (field.getName.getLocalPart -> value.toJson)
                //println(name + "=" + value.serialiseToJson)
                println(value.toJson)

            }
            i += 1
            println("--------------")
            println(result.result().toJson)
        }
    }

    private def writeAsPipeSeparatedLines(records: Array[com.sforce.soap.partner.sobject.SObject]): Unit = {

    }

    case class FieldValue(node: XmlObject) {

        def getName = node.getName.getLocalPart

        def toJson: JsValue = {

            if (node.hasChildren) {
                val records = Array.newBuilder[JsValue]
                for (recordsNode <- getRecordsNodes(node.getChildren)) {
                    val values = Map.newBuilder[String, JsValue]
                    val children = recordsNode.getChildren
                    while (children.hasNext) {
                        val child = children.next()
                        val value = getFieldValue(child)
                        values += (child.getName.getLocalPart -> value.toJson)
                    }
                    records += values.result().toJson

                }
                records.result().toJson
            } else {
                //normal field
                node.getValue.toString.toJson
            }

        }

        //skip un-interesting stuff in field which contains relationship data
        private def getRecordsNodes(children: java.util.Iterator[XmlObject]): List[XmlObject] = {
            val recordNodes = List.newBuilder[XmlObject]
            while (children.hasNext) {
                val child = children.next()
                if ("records" == child.getName.getLocalPart) {
                    recordNodes += child
                }
            }
            recordNodes.result()
        }
    }

    private def getFieldValue(field: XmlObject): FieldValue = {
        new FieldValue(field)

    }
}
