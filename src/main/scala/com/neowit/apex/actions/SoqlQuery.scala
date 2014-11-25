package com.neowit.apex.actions

import java.io.File
import com.neowit.utils.FileUtils
import com.sforce.ws.bind.XmlObject

//import collection.JavaConverters._
import spray.json._
import DefaultJsonProtocol._

class SoqlQuery extends ApexAction {
    override def act(): Unit = {
        val outputFilePath = config.getRequiredProperty("outputFilePath").get
        //make sure output file does not exist
        FileUtils.delete(new File(outputFilePath))


        val codeFile = new File(config.getRequiredProperty("queryFilePath").get)
        val soqlQuery = scala.io.Source.fromFile(codeFile).getLines().filterNot(_.startsWith("--")).mkString(" ")
        //dump first batch of results into the output file
        var queryResult = session.query(soqlQuery)
        val outputFile = new File(outputFilePath)
        writeResults(queryResult.getRecords, outputFile)

        while (!queryResult.isDone) {
            queryResult = session.queryMore(queryResult.getQueryLocator)
            writeResults(queryResult.getRecords, outputFile)
        }

    }

    override def getHelp: ActionHelp = new ActionHelp {
        override def getExample: String = ""

        override def getParamDescription(paramName: String): String = {
            paramName match {
                case "projectPath" => "--projectPath - full path to project folder"
                case "queryFilePath" => "--queryFilePath - full path to file containing SOQL query to run"
                case "responseFilePath" => "--responseFilePath - path to file where operation result will be reported"
                case "outputFilePath" => "--outputFilePath - path to file where query result will be dumped in specified format"
                case "outputFormat" => "--outputFormat [optional] - how query results will be formatted. Accepted values: 'json', 'pipe'(default)"
                case _ => ""
            }
        }

        override def getParamNames: List[String] = List("projectPath", "codeFile", "responseFilePath")

        override def getSummary: String = "execute provided SOQL query and return results"

        override def getName: String = "soqlQuery"
    }

    def writeResults(records: Array[com.sforce.soap.partner.sobject.SObject], outputFile: File): Unit = {
        config.getProperty("outputFormat") .getOrElse("pipe") match {
            case "json" => writeAsJsonLines(records, outputFile)
            case "pipe" => writeAsPipeSeparatedLines(records, outputFile)
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

    private def writeAsJsonLines(records: Array[com.sforce.soap.partner.sobject.SObject], outputFile: File): Unit = {

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
                //println(value.toJson)

            }
            i += 1
            //println("--------------")
            //println(result.result().toJson)
            FileUtils.writeFile(result.result().toJson.toString() + "\n", outputFile, append = true)
            logger.debug("\n" + result.result().toJson.toString())
        }
    }

    private def writeAsPipeSeparatedLines(records: Array[com.sforce.soap.partner.sobject.SObject], outputFile: File): Unit = {
        var i = 0
        val allRecords = List.newBuilder[String]
        val headers = getHeaders(records)

        val totalLineLength = headers.wideHeaders.map(_.length).sum + headers.orderedNames.size * 3

        var needHeader = true
        while (i < records.length) {
            val record = records(i)
            val result = List.newBuilder[String]
            val fields = skipTypeAndId(record.getChildren)
            while (fields.hasNext) {
                val field = fields.next()
                val name = field.getName.getLocalPart
                val value = getFieldValue(field)
                val columnWidth = headers.lengthByName.getOrElse(name, -1)
                val strValue = value.toPipeDelimited(columnWidth)
                if (strValue.nonEmpty) {
                    if (!headers.namesToExclude.contains(name)) {
                        if (needHeader) {
                            allRecords += headers.wideHeaders.mkString(" | ")
                            needHeader = false
                        }
                        result += strValue.padTo(columnWidth, " ").mkString("")

                    } else {
                        result += strValue
                        needHeader = true
                    }
                }

            }
            i += 1
            //println("--------------")
            val thisRecord = result.result().mkString(" | ")
            allRecords += thisRecord
        }
        //println(allRecords.result().mkString("\n" + "-".padTo(totalLineLength, "-").mkString("")  + " \n"))
        FileUtils.writeFile(allRecords.result().mkString("\n" + "-".padTo(totalLineLength, "-").mkString("")  + " \n"), outputFile)
        logger.debug("\n" + allRecords.result().mkString("\n" + "-".padTo(totalLineLength, "-").mkString("")  + " \n"))

    }

    private def getHeaders(records: Array[com.sforce.soap.partner.sobject.SObject]): Headers = {
        val headers = List.newBuilder[String]
        var maxValueLengthByHeader = new collection.mutable.HashMap[String, Int]
        //record name of fields that have "records" in them instead of plain value
        val fieldsWithNestedValueBuilder = Set.newBuilder[String]
        var i = 0
        while (i < records.length) {
            val record = records(i)
            val fields = skipTypeAndId(record.getChildren)
            while(fields.hasNext) {
                val fieldNode = fields.next()
                val name = fieldNode.getName.getLocalPart
                if (!fieldNode.hasChildren) {
                    val value = getFieldValue(fieldNode).toPipeDelimited(-1)
                    val valueLength = value.length
                    maxValueLengthByHeader.get(name) match {
                      case Some(len) if len < valueLength =>
                          maxValueLengthByHeader += (name -> valueLength)
                      case Some(len) =>
                      case None => //first time seeing this field name
                          headers += name
                          val len = Math.max(name.length, valueLength)
                          maxValueLengthByHeader += (name -> len)
                    }
                } else {
                    fieldsWithNestedValueBuilder += name
                }

            }
            i += 1
        }

        val fieldsWithNestedValue = fieldsWithNestedValueBuilder.result()
        val compactHeaders = headers.result().filterNot(fieldsWithNestedValue.contains(_))
        //expand each header to the size of its longest value
        val wideHeaders = for (header <- compactHeaders) yield {
            maxValueLengthByHeader.get(header) match {
              case Some(len) =>
                  "" + header.padTo(len, " ").mkString("")
              case None => ""
            }
        }
        new Headers(compactHeaders, fieldsWithNestedValue, maxValueLengthByHeader.toMap, wideHeaders)

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
                if (null != node.getValue) node.getValue.toString.toJson else "".toJson
            }

        }

        def toPipeDelimited(width: Int): String = {
            if (node.hasChildren) {
                val records = Array.newBuilder[String]
                for (recordsNode <- getRecordsNodes(node.getChildren)) {
                    records += "    \n"
                    var isSkipId = true
                    val values = List.newBuilder[String]
                    val children = recordsNode.getChildren
                    while (children.hasNext) {
                        val child = children.next()
                        val skipValue = isSkipId && "Id" == child.getName.getLocalPart
                        if (!skipValue) {
                            val value = getFieldValue(child)
                            values += value.toPipeDelimited(width)
                        } else {
                            isSkipId = false
                        }
                    }
                    records += values.result().mkString(" | ")
                }
                records.result().mkString(" | ")
            } else {
                //normal field
                if (null != node.getValue) {
                    val strVal = node.getValue.toString
                    if (width > 0) {
                        strVal.padTo(width, " ").mkString("")
                    } else {
                        strVal
                    }
                } else {
                    ""
                }
            }

        }

        //find child which contains actual child "records" payload of current node
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

    case class Headers (orderedNames: List[String], namesToExclude: Set[String], lengthByName: Map[String, Int], wideHeaders: List[String])
}
