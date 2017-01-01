package com.neowit.apex.actions

import java.io.File
import java.net.URLEncoder

import com.neowit.apex.Session
import com.neowit.response._
import com.neowit.utils.FileUtils

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

//import collection.JavaConverters._
import spray.json._

object QueryResultJsonProtocol extends DefaultJsonProtocol {
    implicit val queryResultFormat: JsonFormat[SoqlQuery.QueryResultJson] = lazyFormat(jsonFormat7(SoqlQuery.QueryResultJson))
}

class SoqlQuery extends ApexActionWithReadOnlySession {

    import SoqlQuery._
    import QueryResultJsonProtocol._

    override def getHelp: ActionHelp = new ActionHelp {
        override def getExample: String = ""

        override def getParamDescription(paramName: String): String = {
            paramName match {
                case "projectPath" => "--projectPath - full path to project folder"
                case "queryFilePath" => "--queryFilePath - full path to file containing SOQL query to run"
                case "responseFilePath" => "--responseFilePath - path to file where operation result will be reported"
                case "api" => "--api=Partner|Tooling - type of API to make a query call"
                case "outputFilePath" => "--outputFilePath - path to file where query result will be dumped in specified format"
                case "outputFormat" =>
                    """--outputFormat [optional] - how query results will be formatted.
                      |  Accepted values: 'json', 'pipe'(default), 'plain'
                      |  - json: each record from main query is serialised into JSON and placed in its own line in the file
                      |         Note: the resulting file is not valid JSON, but each of its lines is valid JSON
                      |               this allows reading and de-serializing file 1 record at a time.
                      |  - pipe: pretty printed version of query result
                      |  - plain: each field name/value on its own line
                      """.stripMargin
                case _ => ""
            }
        }

        override def getParamNames: List[String] = List("projectPath", "codeFile", "responseFilePath", "api", "outputFilePath", "outputFormat")

        override def getSummary: String = "execute provided SOQL query and return results"

        override def getName: String = "soqlQuery"
    }
    //this method should implement main logic of the action
    override protected def act()(implicit ec: ExecutionContext): Future[ActionResult] = {
        val codeFile = new File(config.getRequiredProperty("queryFilePath").get)
        val soqlQuery = FileUtils.readFile(codeFile).getLines().filterNot(_.startsWith("--")).mkString(" ")

        val queryString = "q=" + URLEncoder.encode(soqlQuery, "UTF-8")
        val requestResult = config.getProperty("api").getOrElse("Partner") match {
            case "Partner" => Try(session.getRestContentPartner("/query/", queryString))
            case "Tooling" => Try(session.getRestContentTooling("/query/", queryString))
            case x => Try(throw new ShowHelpException(getHelp, "Invalid API: " + x))
        }
        val builderWithSuccess = new ActionResultBuilder(SUCCESS)
        val builderWithFailure = new ActionResultBuilder(FAILURE)

        val resultBuilder =
            requestResult match {
                case Success(result) =>
                    result match {
                        case Some(doc) =>
                            val queryResult = SoqlQuery.parseQueryResultDoc(doc)
                            val queryIterator = new QueryBatchIterator(session, queryResult,
                                (locator: String) => {
                                    val batchResult = config.getProperty("api").getOrElse("Partner") match {
                                        case "Partner" => session.getRestContentPartner(s"/query/$locator", "")
                                        case "Tooling" => session.getRestContentTooling(s"/query/$locator", "")
                                        case x => throw new ShowHelpException(getHelp, "Invalid API: " + x)
                                    }
                                    batchResult match {
                                        case Some(batchDoc) => SoqlQuery.parseQueryResultDoc(batchDoc)
                                        case _ => throw new IllegalAccessError("Out of range")
                                    }
                                })

                            def onBatchComplete(totalRecordsLoaded: Int, batchNum: Int) = {
                                logger.info("Loaded " + totalRecordsLoaded + " out of " + queryIterator.size)
                            }
                            queryIterator.setOnBatchComplete(onBatchComplete)
                            //responseWriter.println("RESULT=SUCCESS")
                            //responseWriter.println("RESULT_SIZE=" + queryResult.totalSize)
                            builderWithSuccess.addMessage(KeyValueMessage(Map("RESULT_SIZE" -> queryResult.totalSize)))

                            if (queryResult.totalSize < 1 || queryResult.records.isEmpty) {
                                //looks like there are no results or this is just a count() query
                                //responseWriter.println(InfoMessage("size=" + queryResult.totalSize))
                                builderWithSuccess.addMessage(InfoMessage("size=" + queryResult.totalSize))
                            } else {
                                val outputFilePath = config.getRequiredProperty("outputFilePath").get
                                //make sure output file does not exist
                                FileUtils.delete(new File(outputFilePath))
                                val outputFile = new File(outputFilePath)

                                var displayHeader = true
                                for (batch <- queryIterator) {
                                    writeQueryBatchResults(batch, outputFile, displayHeader)
                                    displayHeader = false
                                }

                                //responseWriter.println("RESULT_FILE=" + outputFilePath)
                                builderWithSuccess.addMessage(KeyValueMessage(Map("RESULT_FILE" -> outputFilePath)))
                                builderWithSuccess
                            }
                        case None =>

                    }
                    builderWithSuccess
                case Failure(result) =>
                    //responseWriter.println("RESULT=FAILURE")
                    result match {
                        case ex: Session.RestCallException if null != ex.connection =>
                            //responseWriter.println(ErrorMessage(ex.connection.getResponseCode + ": " + ex.connection.getResponseMessage))
                            builderWithFailure.addMessage(ErrorMessage(ex.connection.getResponseCode + ": " + ex.connection.getResponseMessage))
                            ex.getRestErrorCode match {
                                case Some(errorCode) =>
                                    //responseWriter.println(ErrorMessage( errorCode + ": " + ex.getRestMessage.getOrElse("") )
                                    builderWithFailure.addMessage(ErrorMessage(errorCode + ": " + ex.getRestMessage.getOrElse("")))

                                case None =>
                            }
                        case _ =>
                            //responseWriter.println(ErrorMessage( result.getMessage))
                            builderWithFailure.addMessage(ErrorMessage(result.getMessage))
                    }
                    builderWithFailure
            }
        Future.successful(resultBuilder.result())
    }


    def writeQueryBatchResults(records: List[JsObject], outputFile: File, displayHeader: Boolean = false): Unit = {
        config.getProperty("outputFormat").getOrElse("pipe") match {
            case "json" => writeAsJson(records, outputFile)
            case "plain" => writeAsPlainStrings(records, outputFile)
            case "pipe" => writeAsPipeSeparatedLines(records, outputFile, displayHeader)
            case x => throw new ShowHelpException(getHelp, "Unsupported outputFormat: " + x)
        }
    }
    private def writeAsJson(records: List[JsObject], outputFile: File): Unit = {
        val lines = records.map(_.toJson)
        FileUtils.writeFile(lines.mkString("\n"), outputFile, append = true)
    }
    private def writeAsPlainStrings(records: List[JsObject], outputFile: File): Unit = {
        var i = 0
        for (record <- records) {
            for (field <- record.fields) {
                val fName = field._1
                if ("attributes" != fName) {
                    val strValue = field._2
                    FileUtils.writeFile(fName + ": " + strValue + "\n", outputFile, append = true)
                    logger.debug("\n" + strValue)

                }
            }
            FileUtils.writeFile("--------------" + "\n", outputFile, append = true)
            logger.debug("\n" + "--------------")
        }
    }

    private def writeAsPipeSeparatedLines(records: List[JsObject], outputFile: File, displayHeader: Boolean = false): Unit = {

        if (records.nonEmpty) {
            //find max column length for each column
            val resultRecords = records.map(new ResultRecord(_))
            val maxWidthByName = getMaxWidthByColumn(resultRecords)
            //prepare header display string
            val sampleRecord = resultRecords.head
            val header = sampleRecord.getFieldNames.map(fName => fName.padTo(maxWidthByName(fName), " ").mkString("")).mkString("|")
            val headerDivider = "".padTo(maxWidthByName.values.sum, "=").mkString("")
            //prepare rows
            val rows = records.flatMap(new ResultRecord(_).toPipeDelimited(maxWidthByName))

            val allLines = if (displayHeader) header :: headerDivider :: rows else rows
            FileUtils.writeFile(allLines.mkString("\n") + "\n", outputFile, append = true)
            //logger.debug(allLines.mkString("\n"))
        }
    }
}



object SoqlQuery {
    import QueryResultJsonProtocol._

    def parseQueryResultDoc(doc: String):QueryResultJson = {
        val jsonAst = JsonParser(doc)
        val queryResult = jsonAst.convertTo[QueryResultJson]

        queryResult
    }

    private def queryMoreFun(session: Session, api: String) (locator: String) = {
            val batchResult = session.config.getProperty("api").getOrElse("Partner") match {
                case "Partner" => session.getRestContentPartner(s"/query/$locator", "")
                case "Tooling" => session.getRestContentTooling(s"/query/$locator", "")
                case x => throw new IllegalAccessError("Invalid API: " + x)
            }
            batchResult match {
                case Some(batchDoc) => SoqlQuery.parseQueryResultDoc(batchDoc)
                case _ => throw new IllegalAccessError("Out of range")
            }
    }

    def getQueryIteratorTooling(session: Session, soqlQuery: String):Iterator[JsObject] = {
        val queryString = "q=" + soqlQuery.replaceAll(" |\n", "+")
        session.getRestContentTooling("/query/", queryString) match {
            case Some(doc) =>
                val queryResult = parseQueryResultDoc(doc)
                new QueryIterator(session, queryResult, queryMoreFun(session, "Tooling"))
            case None => new EmptyQueryIterator()
        }
    }

    def getQueryIteratorTyped[A](session: Session, queryResult: com.sforce.soap.tooling.QueryResult):Iterator[A] = {
        import com.sforce.soap.tooling._

        def queryMore(queryResult: com.sforce.soap.tooling.QueryResult, records: Array[SObject]): Iterator[A] = {
            if (queryResult.isDone) {
                records.map(_.asInstanceOf[A]).toIterator
            } else {
                val _queryResult = session.queryMoreTooling(queryResult.getQueryLocator)
                queryMore(_queryResult, records ++ _queryResult.getRecords)
            }
        }
        queryMore(queryResult, queryResult.getRecords)
    }


    case class QueryResultJson(size: Option[Int], totalSize: Int, done: Boolean,
                               queryLocator: Option[String], nextRecordsUrl: Option[String],
                               entityTypeName: Option[String], records: List[JsObject])

    def getMaxWidthByColumn(records: List[ResultRecord]): Map[String, Int] = {
        var maxWidthByName = new scala.collection.mutable.HashMap[String, Int]()
        //find max column length for each column
        if (records.nonEmpty) {
            //init with column names
            val sampleRecord = records.head
            maxWidthByName ++= sampleRecord.getFieldNames.map(fName => fName ->fName.length)
            //process column values
            for (record <- records) {
                maxWidthByName ++= record.getColumnWidths.map{
                    case (fName, width) =>
                        val maxWidth = maxWidthByName.get(fName) match {
                            case Some(currWidth) => if (currWidth < width) width else currWidth
                            case None => //this may be a relationship column which has value = null
                                //check if map contains values that start with <fName.>
                                maxWidthByName.find{ case (n, v) => n.startsWith(fName + ".")} match {
                                  case Some(v) => v._2
                                  case None => width
                                }
                        }
                        fName -> maxWidth
                }
            }
        }
        maxWidthByName.toMap
    }
    /**
     *
     * @param session - Session
     * @param queryResult - result of initial query
     * @param queryMoreFun - function which does "query-more" call and accepts query-locator (query identifier)
     */
    class QueryBatchIterator(session: Session, queryResult: QueryResultJson, queryMoreFun: (String) => QueryResultJson) extends Iterator[List[JsObject]] {
        private var queryResultInternal = queryResult
        //private var onBatchCompleteFun:(Int, Int) => Unit = (0, 0) => Unit
        private var onBatchCompleteFun = (totalRecordsLoaded: Int, batchNumber: Int) => {}
        private var batchNumber = -1
        private var totalRecordsLoaded = 0

        override def hasNext: Boolean = !queryResultInternal.done || (size > 0 && batchNumber < 0)


        override def isEmpty: Boolean = size < 1

        override def next(): List[JsObject] = {
            val records =
                if (batchNumber < 0) {
                    queryResultInternal.records
                }  else if (queryResultInternal.done) {
                    throw new IllegalAccessError("Out of range")
                } else {
                    queryResultInternal.queryLocator match {
                      case Some(locator) =>
                          //query more
                          queryResultInternal = queryMoreFun(locator)
                          queryResultInternal.records
                      case None =>
                          queryResultInternal.nextRecordsUrl match {
                            case Some(nextRecordsUrl) =>
                                //Tooling Api uses full path format, as opposed to locator only, like Partner API
                                //convert from: "/services/data/v33.0/query/01gg000000JcuQQAAZ-124"
                                //to: "01gg000000JcuQQAAZ-124"
                                val locator = nextRecordsUrl.split("/").last
                                queryResultInternal = queryMoreFun(locator)
                                queryResultInternal.records
                            case None => Nil
                          }
                    }
                }
            batchNumber += 1
            totalRecordsLoaded += records.length
            //alert about batch completion
            onBatchCompleteFun(totalRecordsLoaded, batchNumber)
            records
        }


        override def size: Int = queryResult.totalSize
        def getCurrentBatchSize: Int = queryResultInternal.records.length

        /**
         *
         * @param fun: (totalRecordsLoaded: Int, batchNumber: Int) = {}
         */
        def setOnBatchComplete(fun: (Int, Int) => Unit): Unit = {
            onBatchCompleteFun = fun
        }
        def getBatchNumber: Int = batchNumber
    }

    class EmptyQueryIterator extends Iterator[JsObject] {
        override def hasNext: Boolean = false

        override def next(): JsObject = throw new IllegalAccessError("Iterator is Empty")
    }
    class QueryIterator(session: Session, queryResult: QueryResultJson, queryMoreFun: (String) => QueryResultJson) extends Iterator[JsObject] {
        private val batchIterator = new QueryBatchIterator(session, queryResult, queryMoreFun)

        private var indexInBatch = 0
        private var _records = batchIterator.next()

        override def hasNext: Boolean = indexInBatch < batchIterator.getCurrentBatchSize || batchIterator.hasNext

        override def next(): JsObject = {
            if (indexInBatch >= batchIterator.getCurrentBatchSize) {
                _records = batchIterator.next()
                indexInBatch = 0
            }
            val record: JsObject = _records(indexInBatch)
            indexInBatch += 1
            record
        }
    }

    class ResultRecord(record: JsObject) {

        private def getFieldValue(fName: String): Option[JsValue] = {
            val fieldName = fName.toLowerCase
            if (fieldName.indexOf('.') >0) {
                //Parent.Object.Value
                val names = fieldName.split('.')
                val relContainer = getFieldAsObject(names.head)
                relContainer match {
                  case Some(container) =>
                      container.getFieldValue(names.drop(1).mkString("."))
                  case None => None
                }
            } else {
                record.fields.find {
                    case (name, value) => isOwnColumn(name, value) && name.toLowerCase == fieldName
                } match {
                    case Some(value) => Some(value._2)
                    case None => None
                }
            }
        }
        def getFieldAsString(fName: String): Option[String] = {
            getFieldValue(fName) match {
              case Some(value) => Some(value.asInstanceOf[JsString].value)
              case None => None
            }
        }
        def getFieldAsNumber(fName: String): Option[BigDecimal] = {
            getFieldValue(fName) match {
                case Some(value) => Some(value.asInstanceOf[JsNumber].value)
                case None => None
            }
        }
        def getFieldAsArray(fName: String): Option[JsArray] = {
            getFieldValue(fName) match {
                case Some(value) => Some(value.asInstanceOf[JsArray])
                case None => None
            }
        }

        def getFieldAsObject(fName: String): Option[ResultRecord] = {
            val fieldName = fName.toLowerCase
            record.fields.find{
                case (name, value) => !isOwnColumn(name, value) && name.toLowerCase == fieldName
            } match {
                case Some(value) => Some(new ResultRecord(value._2.asJsObject))
                case None => None
            }
        }
        /**
         * @return - only single level field names, no related child records included
         */
        def getFieldNames: List[String] = {
            val namesBuilder = List.newBuilder[String]
            for (fName <- record.fields.keys) {
                val fValue = record.fields(fName)
                if (null== fValue || fValue == JsNull || isOwnColumn(fName, fValue)) {
                    namesBuilder += fName
                } else if (isParentRelationshipContainer(fName, fValue)) {
                    //resolve relationship
                    namesBuilder ++= traverseParentFieldNames(fName, fValue.asJsObject)
                }
            }
            namesBuilder.result()
        }

        /**
         * descend into parent relationship record
         * @param fName -  field name, e.g. Account
         * @param fValue - parent values container, e.g.
         * {
         *       "attributes" : {
         *           "type" : "Account",
         *           "url" : "/services/data/v34.0/sobjects/Account/001000000000000"
         *       },
         *     "Name" : "Edge Communications",
         *     "BillingStreet" : "some street name"
         *  }
         *
         * @return Account.Name, Account.BillingStreet
         */
        private def traverseParentFieldNames(fName: String, fValue: JsObject): List[String] = {
            new ResultRecord(fValue).getFieldNames.map(name => fName + "." + name)
        }

        /**
         * @return - only child record-sets
         */
        def getChildResultContainers: List[QueryResultJson] = {
            val jsObjects = record.fields.filter{case (name, value) => isChildRecordsContainer(name, value)}.values.map(_.asJsObject).toList
            //jsObjects.flatMap(obj => obj.fields("records").asInstanceOf[JsArray].elements.map(rec => new ResultRecord(rec.asJsObject)))
            jsObjects.map(_.convertTo[QueryResultJson])
        }

        /**
         * Account.Name
         * @return
         */
        def getRelationshipResultContainers: List[ResultRecord] = {
            val relationshipRecords = record.fields.filter{case (name, value) => isParentRelationshipContainer(name, value)}.values.map(v => new ResultRecord(v.asJsObject)).toList
            relationshipRecords
        }
        def getColumnWidths: Map[String, Int] = {
            val widthByName = Map.newBuilder[String, Int]
            for (fName <- getFieldNames) {
                val fVal = new ResultRecord(record).getFieldValue(fName).getOrElse(new JsString(""))
                widthByName += fName -> fVal.compactPrint.length
            }
            widthByName.result()
        }

        def jsPrinter(value: JsValue): String = {
            value match {
                case v:JsString => v.value
                case v:JsNumber => v.value.toString()
                case v:JsBoolean => v.value.toString
                case v => v.toString()
            }
        }
        def getHeader(fieldNames: List[String], maxWidthByName: Map[String, Int]): String = {
            val header = fieldNames.map(fName => fName.padTo(maxWidthByName(fName), " ").mkString("")).mkString("|")
            header
        }
        def toPipeDelimited(sizeByName: Map[String, Int], shiftLeft:Int = 0): List[String] = {
            val mainLine = "".padTo(shiftLeft, " ").mkString + getFieldNames.map{
                fName => getFieldValue(fName).getOrElse(new JsString("")).toString(jsPrinter).padTo(sizeByName(fName), " ").mkString("")
            }.mkString("|")

            //process child records
            val childRecordsPipeDelimited = List.newBuilder[String]
            val childContainers = getChildResultContainers
            var hasChildRecords = false
            for (childContainer <- childContainers) {
                val childRecords = childContainer.records.map(new ResultRecord(_))
                if (childRecords.nonEmpty) {
                    hasChildRecords = true
                    val maxWidthByName = getMaxWidthByColumn(childRecords)
                    val sampleRecord = childRecords.head
                    val relationshipName = sampleRecord.getAttribute("type").getOrElse("")
                    val indentation = relationshipName + " => |"
                    val shiftLeft = indentation.length
                    val header = sampleRecord.getHeader(sampleRecord.getFieldNames, maxWidthByName)
                    childRecordsPipeDelimited += (indentation + header)
                    childRecordsPipeDelimited ++= childRecords.flatMap(_.toPipeDelimited(maxWidthByName, shiftLeft))
                }
            }
            if (hasChildRecords) {
                val divider = "".padTo(sizeByName.values.sum, "-").mkString("")
                childRecordsPipeDelimited += divider
            }
            mainLine :: childRecordsPipeDelimited.result()
        }
        private def getAttribute(name: String): Option[JsValue] = {
            val attrs = record.fields("attributes")
            if (attrs.asJsObject.fields.contains(name)) Some(attrs.asJsObject.fields(name)) else None

        }

        private def isOwnColumn(fName: String, fValue: JsValue): Boolean = {
            "attributes" != fName && null != fValue && "null" != fValue.toString() && !fValue.isInstanceOf[JsObject]
        }
        private def isChildRecordsContainer(fName: String, fValue: JsValue): Boolean = {
            "attributes" != fName && null != fValue && "null" != fValue.toString() && fValue.isInstanceOf[JsObject] && fValue.asJsObject.getFields("totalSize").nonEmpty
        }
        private def isParentRelationshipContainer(fName: String, fValue: JsValue): Boolean = {
            "attributes" != fName && null != fValue && "null" != fValue.toString() && fValue.isInstanceOf[JsObject] && fValue.asJsObject.getFields("totalSize").isEmpty
        }
    }
}
