package com.neowit.apex.actions

import java.io.File
import com.neowit.apex.Session
import com.neowit.utils.{Logging, ResponseWriter, FileUtils}
import com.sforce.ws.bind.XmlObject

//import collection.JavaConverters._
import spray.json._
import DefaultJsonProtocol._

object SoqlQuery {

    def queryAllPartner(soqlQuery: String, session: Session, logger: Logging): Array[com.sforce.soap.partner.sobject.SObject] = {
        val records = Array.newBuilder[com.sforce.soap.partner.sobject.SObject]
        var queryResult = session.query(soqlQuery)

        val size = queryResult.getSize
        var batchOfRecords = queryResult.getRecords
        var loadedRecordCount = batchOfRecords.size
        records ++= batchOfRecords
        while (!queryResult.isDone) {
            logger.info("Loaded " + loadedRecordCount + " out of " + size)
            queryResult = session.queryMore(queryResult.getQueryLocator)

            batchOfRecords = queryResult.getRecords
            loadedRecordCount = batchOfRecords.size
            records ++= batchOfRecords
        }
        records.result()
    }

    def getQueryBatchIteratorTooling[A](session: Session, queryStr: String):QueryBatchIterator[A] = {
        new QueryBatchIterator[A](session, new QueryResultTooling(session.queryTooling(queryStr)))
    }
    def getQueryIteratorTooling[A](session: Session, queryStr: String):QueryIterator[A] = {
        new QueryIterator[A](session, new QueryResultTooling(session.queryTooling(queryStr)))
    }

    def getQueryBatchIteratorPartner[A](session: Session, queryStr: String):QueryBatchIterator[A] = {
        new QueryBatchIterator[A](session, new QueryResultPartner(session.query(queryStr)))
    }
    def getQueryIteratorPartner[A](session: Session, queryStr: String):QueryIterator[A] = {
        new QueryIterator[A](session, new QueryResultPartner(session.query(queryStr)))
    }

    class QueryBatchIterator[A](session: Session, queryResult: GenericQueryResult) extends Iterator[Array[A]] {
        private var queryResultInternal = queryResult
        //private var onBatchCompleteFun:(Int, Int) => Unit = (0, 0) => Unit
        private var onBatchCompleteFun = (totalRecordsLoaded: Int, batchNumber: Int) => {}
        private var batchNumber = -1
        private var totalRecordsLoaded = 0

        override def hasNext: Boolean = !queryResultInternal.isDone || (size > 0 && batchNumber < 0)


        override def isEmpty: Boolean = size < 1

        override def next(): Array[A] = {
            val records =
                if (batchNumber < 0) {
                    queryResultInternal.getRecords[A]
                }  else if (queryResultInternal.isDone) {
                    throw new IllegalAccessError("Out of range")
                } else {
                    queryResultInternal = queryResult.queryMore(session, queryResultInternal.getQueryLocator)
                    queryResultInternal.getRecords[A]
                }
            batchNumber += 1
            totalRecordsLoaded += records.length
            //alert about batch completion
            onBatchCompleteFun(totalRecordsLoaded, batchNumber)
            records
        }


        override def size: Int = queryResult.getSize
        def getCurrentBatchSize: Int = queryResultInternal.getRecords.length

        /**
         *
         * @param fun: (totalRecordsLoaded: Int, batchNumber: Int) = {}
         */
        def setOnBatchComplete(fun: (Int, Int) => Unit): Unit = {
            onBatchCompleteFun = fun
        }
        def getBatchNumber: Int = batchNumber
    }

    class QueryIterator[A](session: Session, queryResult: GenericQueryResult) extends Iterator[A] {
        private val batchIterator = new QueryBatchIterator(session, queryResult)

        private var indexInBatch = 0
        private var _records = batchIterator.next()

        override def hasNext: Boolean = indexInBatch < batchIterator.getCurrentBatchSize || batchIterator.hasNext

        override def next(): A = {
            if (indexInBatch >= batchIterator.getCurrentBatchSize) {
                _records = batchIterator.next()
                indexInBatch = 0
            }
            val record: A = _records(indexInBatch)
            indexInBatch += 1
            record
        }
    }

    trait GenericQueryResult {
        def getQueryLocator: String
        def getRecords[A]: Array[A]
        def getSize: Int
        def isDone: Boolean
        def queryMore(session: Session, queryLocator: String): GenericQueryResult
    }

    class QueryResultTooling(queryResult: com.sforce.soap.tooling.QueryResult) extends GenericQueryResult {
        override def getQueryLocator: String = queryResult.getQueryLocator

        override def queryMore(session: Session, queryLocator: String): GenericQueryResult = new QueryResultTooling(session.queryMoreTooling(queryResult.getQueryLocator))

        override def getSize: Int = queryResult.getSize

        override def getRecords[A]: Array[A] = queryResult.getRecords.asInstanceOf[Array[A]]

        override def isDone: Boolean = queryResult.isDone
    }

    class QueryResultPartner(queryResult: com.sforce.soap.partner.QueryResult) extends GenericQueryResult {
        override def getQueryLocator: String = queryResult.getQueryLocator

        override def queryMore(session: Session, queryLocator: String): GenericQueryResult = new QueryResultTooling(session.queryMoreTooling(queryResult.getQueryLocator))

        override def getSize: Int = queryResult.getSize

        override def getRecords[A]: Array[A] = queryResult.getRecords.asInstanceOf[Array[A]]

        override def isDone: Boolean = queryResult.isDone
    }
}

class SoqlQuery extends ApexAction {
    override def getHelp: ActionHelp = new ActionHelp {
        override def getExample: String = ""

        override def getParamDescription(paramName: String): String = {
            paramName match {
                case "projectPath" => "--projectPath - full path to project folder"
                case "queryFilePath" => "--queryFilePath - full path to file containing SOQL query to run"
                case "responseFilePath" => "--responseFilePath - path to file where operation result will be reported"
                case "outputFilePath" => "--outputFilePath - path to file where query result will be dumped in specified format"
                case "outputFormat" => "--outputFormat [optional] - how query results will be formatted. Accepted values: 'json', 'pipe'(default), 'plain'(each field name/value on its own line)"
                case _ => ""
            }
        }

        override def getParamNames: List[String] = List("projectPath", "codeFile", "responseFilePath")

        override def getSummary: String = "execute provided SOQL query and return results"

        override def getName: String = "soqlQuery"
    }

    override def act(): Unit = {


        val codeFile = new File(config.getRequiredProperty("queryFilePath").get)
        val soqlQuery = FileUtils.readFile(codeFile).getLines().filterNot(_.startsWith("--")).mkString(" ")

        val queryIterator = SoqlQuery.getQueryBatchIteratorPartner[com.sforce.soap.partner.sobject.SObject](session, soqlQuery)

        def onBatchComplete(totalRecordsLoaded: Int, batchNum: Int) = {
            logger.info("Loaded " + totalRecordsLoaded + " out of " + queryIterator.size)
        }
        queryIterator.setOnBatchComplete(onBatchComplete)

        responseWriter.println("RESULT=SUCCESS")
        responseWriter.println("RESULT_SIZE=" + queryIterator.size)
        if (queryIterator.isEmpty) {
            //looks like this is just a count() query
            responseWriter.println(new ResponseWriter.Message(ResponseWriter.INFO, "size=" + queryIterator.size))
        } else {
            val outputFilePath = config.getRequiredProperty("outputFilePath").get
            //make sure output file does not exist
            FileUtils.delete(new File(outputFilePath))
            val outputFile = new File(outputFilePath)
            for (batch <- queryIterator) {
                writeQueryBatchResults(batch, outputFile)
            }
            responseWriter.println("RESULT_FILE=" + outputFilePath)
        }
    }


    /*
    def act2(): Unit = {
        val outputFilePath = config.getRequiredProperty("outputFilePath").get
        //make sure output file does not exist
        FileUtils.delete(new File(outputFilePath))


        val codeFile = new File(config.getRequiredProperty("queryFilePath").get)
        val soqlQuery = FileUtils.readFile(codeFile).getLines().filterNot(_.startsWith("--")).mkString(" ")
        //dump first batch of results into the output file
        var queryResult = session.query(soqlQuery)
        var loadedRecordCount = queryResult.getRecords.length
        val size = queryResult.getSize
        if (loadedRecordCount > 0) {
            val outputFile = new File(outputFilePath)
            writeQueryBatchResults(queryResult.getRecords, outputFile)

            while (!queryResult.isDone) {
                logger.info("Loaded " + loadedRecordCount + " out of " + size)
                queryResult = session.queryMore(queryResult.getQueryLocator)
                writeQueryBatchResults(queryResult.getRecords, outputFile)
                loadedRecordCount += queryResult.getRecords.length
            }
            responseWriter.println("RESULT=SUCCESS")
            responseWriter.println("RESULT_FILE=" + outputFilePath)
        } else {
            //looks like this is just a count() query
            responseWriter.println("RESULT=SUCCESS")
            responseWriter.println("RESULT_SIZE=" + size)
            responseWriter.println(new ResponseWriter.Message(ResponseWriter.INFO, "size=" + size))
        }

    }
    */

    def writeQueryBatchResults(records: Array[com.sforce.soap.partner.sobject.SObject], outputFile: File): Unit = {
        config.getProperty("outputFormat") .getOrElse("pipe") match {
            case "json" => writeAsJsonLines(records, outputFile)
            case "plain" => writeAsPlainStrings(records, outputFile)
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

    private def writeAsPlainStrings(records: Array[com.sforce.soap.partner.sobject.SObject], outputFile: File): Unit = {

        var i = 0
        while (i < records.length) {
            val record = new ResultRecord(records(i))
            for (fValue <- record.getFieldValues) {
                val strValue = fValue.toString
                FileUtils.writeFile(strValue + "\n", outputFile, append = true)
                logger.debug("\n" + strValue)
            }
            i += 1
            FileUtils.writeFile("--------------" + "\n", outputFile, append = true)
            logger.debug("\n" + "--------------")
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

    /**
     * this method tries to format output like a text table, with pipe '|' used as a column separator
     * Note: in the current version headers for inner queries are are not provided to save space on the screen
     */
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
                val value = getFieldValue(field)
                for (columnHeader <- value.getHeaderNames) {
                    val path = columnHeader.split("\\.")
                    //at this point we have either
                    //Name - i.e. field which does not need further resolving (i.e. value can be obtained directly using columnHeader)
                    //or Account.Name or Account.Owner.Name, where current value is 'Account'
                    //  and we need to resolve children 'Name' & 'Owner.name', hence path.tail and not columnHeader
                    val pathToResolve = if (path.length <2) columnHeader else path.tail.mkString(".")
                    val childXml = value.getChild(pathToResolve)
                    val childValue = if (null != childXml) getFieldValue(childXml, Some(value)) else value
                    val columnWidth = headers.lengthByName.getOrElse(columnHeader, -1)
                    val strValue = childValue.toPipeDelimited(columnWidth)
                    if (strValue.nonEmpty) {
                        if (!headers.namesToExclude.contains(columnHeader)) {
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

            }
            i += 1

            val thisRecord = result.result().mkString(" | ")
            allRecords += thisRecord
        }
        FileUtils.writeFile(allRecords.result().mkString("\n" + "-".padTo(totalLineLength, "-").mkString("")  + " \n"), outputFile, append = true)
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
            val record = new ResultRecord(records(i))
            for (fValues <- record.getFieldValues) {
                for (columnName <- fValues.getHeaderNames) {
                    val value = record.getChild(columnName).getValue
                    if (null != value) {
                        val valueLength = value.toString.length
                        maxValueLengthByHeader.get(columnName) match {
                            case Some(len) if len < valueLength =>
                                maxValueLengthByHeader += (columnName -> valueLength)
                            case Some(len) =>
                            case None => //first time seeing this field name
                                headers += columnName
                                val len = Math.max(columnName.length, valueLength)
                                maxValueLengthByHeader += (columnName -> len)
                        }
                        //println(columnName + "=" + record.getChild(columnName).getValue)
                    } else {
                        fieldsWithNestedValueBuilder += columnName
                    }
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

    trait XmlObjectUtils {
        def getXmlObject: XmlObject

        def getChild(name: String): XmlObject = {
            def getFieldValue(field: XmlObject, path: List[String]): XmlObject = {
                if (Nil == path.tail) {
                    //arrived at the end of the path
                    field.getChild(path.head)
                } else {
                    getFieldValue(field.getChild(path.head), path.tail)
                }
            }

            if (name.indexOf(".") < 0) {
                getXmlObject.getChild(name)
            } else {
                val path = name.split("\\.")
                getFieldValue(getXmlObject, path.toList)
            }
        }

    }
    case class ResultRecord(record: XmlObject) extends XmlObjectUtils {
        assert(null != record.getXmlType && "sObject" == record.getXmlType.getLocalPart)

        def getXmlObject: XmlObject = record

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

        def toPipeDelimited(headers: Option[Headers] = None): String = {

            val fields = List.newBuilder[String]
            for (field <- getFieldValues) {
                headers match {
                  case Some(_headers) =>
                      val width = _headers.lengthByName.getOrElse(field.getLocalName, -1)
                      fields += field.toPipeDelimited(width)
                  case None =>
                      fields += field.toPipeDelimited(-1)
                }
            }
            fields.result().mkString(" | ")
        }

        def getFieldNames: List[String] = {
            getFieldValues.map(_.getName)
        }
    }

    case class FieldValue(node: XmlObject, parentNode: Option[FieldValue]) extends XmlObjectUtils {
        def getXmlObject: XmlObject = node

        def getHeaderNames: List[String] = {
            if (null != node.getXmlType && "sObject" == node.getXmlType.getLocalPart) {
                //this is a relationship field
                //Owner (Name , Id)
                getValue match {
                  case Some(x::xs) =>
                      val values = x::xs
                      val result = values.map(value => value.asInstanceOf[FieldValue].getHeaderNames).flatten
                      result
                  case _ => List("")
                }
            } else {
                List(getName)
            }
        }

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
            val value = getValue match {
                case Some(x::xs) if x.isInstanceOf[FieldValue]=>
                    val values = x::xs
                    val fields = List.newBuilder[String]
                    for (value <- values) {
                        val field = value.asInstanceOf[FieldValue]
                        fields += field.toPipeDelimited(width)
                    }
                    fields.result().mkString(" | ")
                case Some(x::xs) if x.isInstanceOf[ResultRecord]=>
                    val values = x::xs
                    values.map{value =>
                        val record = value.asInstanceOf[ResultRecord]
                        "\n\t" + record.getType + " => " + record.toPipeDelimited()
                    }.mkString("")
                case Some(_value) =>
                    if (width > 0) {
                        _value.toString.padTo(width, "").mkString("")
                    } else {
                        _value.toString
                    }
                case None => ""
            }
            value

        }
    }

    private def getFieldValue(field: XmlObject, parent: Option[FieldValue] = None): FieldValue = {
        new FieldValue(field, parent)

    }

}
