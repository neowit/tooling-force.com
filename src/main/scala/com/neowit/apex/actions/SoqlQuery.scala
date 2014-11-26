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
            case "plain" => writeAsStrings(records, outputFile)
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

    private def writeAsStrings(records: Array[com.sforce.soap.partner.sobject.SObject], outputFile: File): Unit = {

        var i = 0
        while (i < records.length) {
            val record = new ResultRecord(records(i))
            for (fValue <- record.getFieldValues) {
                println(fValue.toString)
            }
            i += 1
            println("--------------")
            //println(result.result().toJson)
            //FileUtils.writeFile(result.result().toJson.toString() + "\n", outputFile, append = true)
            //logger.debug("\n" + result.result().toJson.toString())
        }
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

    /**
     * just a container to store results of headers preparation cycle
     * @param orderedNames - field names in the order they shall appear
     * @param namesToExclude - field names which are not simple values (e.g. nested child query)
     * @param lengthByName - max width of each column, by field name
     * @param wideHeaders - column headers expanded to the relevant max width
     */
    case class Headers (orderedNames: List[String], namesToExclude: Set[String], lengthByName: Map[String, Int], wideHeaders: List[String])

    /**
     * this is a very slow method - it scans the whole result set to determine max length of column values
     * in order to align columns
     * @param records - array of SObject-s
     * @return
     */
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

    case class ResultRecord(record: XmlObject) {
        assert(null != record.getXmlType && "sObject" == record.getXmlType.getLocalPart)

        def getFieldValues: List[FieldValue] = {

            val result = List.newBuilder[FieldValue]
            val fields = skipTypeAndId(record.getChildren)
            while (fields.hasNext) {
                val field = fields.next()
                //val name = field.getName.getLocalPart
                val value = getFieldValue(field)
                result += value
            }
            result.result()
        }

        def getType: String = record.getChild("type").getValue.toString
        override def toString: String = {
            "\t" + getType + " => " + getFieldValues.map(_.toString).mkString(" | ")
        }
        def toJson: JsValue = {
            val fields = Map.newBuilder[String, JsValue]
            for (field <- getFieldValues) {
                fields += field.getName -> field.toJson
            }
            Map(getType -> new JsObject(fields.result())).toJson
        }
    }

    case class FieldValue(node: XmlObject, parentNode: Option[FieldValue]) {

        def getName: String = {
            parentNode match {
              case Some(parent) =>
                  parent.getName + "." + getLocalName
              case None =>
                  getLocalName
            }
        }
        def getLocalName: String = node.getName.getLocalPart

        def getValue:Option[Any] = {
            if (node.hasChildren) {
                val records = node.getChildren("records")
                val values = List.newBuilder[ResultRecord]
                if (records.hasNext) {
                    //embedded query - e.g. select ... (select Name, CreatedDate from Contacts), (select Id, Subject from Cases) from Account
                    //node may contain a number of "records" children, each of which may contain several field/values
                    while (records.hasNext) {
                        val recordXml = records.next()
                        val record = new ResultRecord(recordXml)
                        values += record
                    }
                    Some(values.result())
                } else {
                    //relationship field - e.g. Owner.Name
                    //node may contain several field/values (in addition to type and Id values)
                    val children = skipTypeAndId(node.getChildren)
                    val values = List.newBuilder[FieldValue]
                    while (children.hasNext) {
                        val child = children.next()
                        val value = getFieldValue(child, Some(this))
                        values += value
                    }
                    Some(values.result()) //Option[List[FieldValue]]
                }
            } else {
                //normal field - Option[FieldValue]
                if (null != node.getValue) Some(node.getValue) else None
            }

        }

        override def toString: String = {
            val value = getValue match {
                case Some(values: List[Any]) =>
                    values.map(value => value.toString).mkString("\n")
                case Some(_value) => getName + "=" + _value.toString
                case None => ""
            }
            value
        }

        def toJson: JsValue = {
            val value = getValue match {
                case Some(x::xs) if x.isInstanceOf[FieldValue]=>
                    val values = x::xs
                    val fields = Map.newBuilder[String, JsValue]
                    for (value <- values) {
                        val field = value.asInstanceOf[FieldValue]
                        fields += field.getLocalName -> field.toJson
                    }
                    new JsObject(fields.result())
                    //values.map(value => value.asInstanceOf[FieldValue].toJson).toJson
                case Some(x::xs) if x.isInstanceOf[ResultRecord]=>
                    val values = x::xs
                    values.map(value => value.asInstanceOf[ResultRecord].toJson).toJson
                case Some(_value) =>
                    _value.toString.toJson
                case None => "".toJson
            }
            value
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

    private def getFieldValue(field: XmlObject, parent: Option[FieldValue] = None): FieldValue = {
        new FieldValue(field, parent)

    }

}
