/*
 * Copyright (c) 2014 Andrey Gavrikov.
 * this file is part of tooling-force.com application
 * https://github.com/neowit/tooling-force.com
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package com.neowit.apex

import java.net.{HttpURLConnection, URL}
import java.security.MessageDigest
import java.util.zip.GZIPInputStream

import com.neowit.utils.{BasicConfig, FileUtils, Logging, Config}
import com.sforce.soap.partner.PartnerConnection
import com.sforce.soap.metadata._

import scala.concurrent._
import collection.JavaConverters._
import java.io._
import com.neowit.apex.actions.{ActionError, DescribeMetadata}

import scala.util.{Failure, Success, Try}

/**
 * manages local data store related to specific project
 */
object Session {
    case class ConnectionException(connection: HttpURLConnection) extends RuntimeException
    case class UnauthorizedConnectionException(connection: HttpURLConnection) extends RuntimeException

    def apply(basicConfig: BasicConfig) = new Session(basicConfig)
}

/**
 * Session has following responsibilities
 * 1. Maintains/stores persistent connection and can resue connection
 * 2. Maintains cache of some important information about project metadata (Pages, Classes, etc)
 *
 * @param basicConfig - main application config
 */
class Session(val basicConfig: BasicConfig) extends Logging {

    val config = new Config(basicConfig)

    private val sessionProperties = config.lastSessionProps
    private var connectionPartner:Option[PartnerConnection] = None
    private var connectionMetadata:Option[MetadataConnection] = None
    private var connectionTooling:Option[com.sforce.soap.tooling.ToolingConnection] = None
    private var connectionApex:Option[com.sforce.soap.apex.SoapConnection] = None

    //when user wants to work with files from one org and deploy them in another org we can not use stored session
    lazy val callingAnotherOrg:Boolean = config.getProperty("callingAnotherOrg").getOrElse("false").toBoolean

    def getConfig = config


    def storeSessionData() {
        config.storeSessionProps()
    }
    def getSavedConnectionData :(Option[String], Option[String])= {

        val emptySession = (None, None)
        if (!callingAnotherOrg) {
            val connectionData = getData("session")
            connectionData.get("hash") match {
              case Some(hash) if hash == getHash =>
                  (connectionData.get("sessionId").map(_.toString), connectionData.get("serviceEndpoint").map(_.toString))
              case _ =>
                  emptySession
            }
        } else {
            emptySession
        }
    }
    //hash is used to check if current session Id was generated against current set of login credentials
    private def getHash: String = {
        val md5 = MessageDigest.getInstance("SHA-256")
        md5.reset()
        val str = config.username + config.password + config.soapEndpoint
        md5.digest(str.getBytes("UTF-8")).map(0xFF & _).map { "%02x".format(_) }.foldLeft(""){_ + _}
    }


    override def hashCode(): Int = getHash.hashCode

    override def equals(p1: scala.Any): Boolean = {
        p1.isInstanceOf[Session] && this.getHash == p1.asInstanceOf[Session].getHash
    }

    def storeConnectionData(connectionConfig: com.sforce.ws.ConnectorConfig) {
        if (!callingAnotherOrg) {
            sessionProperties.setJsonData("session", Map(
                            "sessionId" -> connectionConfig.getSessionId,
                            "serviceEndpoint" -> connectionConfig.getServiceEndpoint,
                            "hash" -> getHash
            ))
            sessionProperties.remove("UserInfo")
        } else {
            sessionProperties.remove("session")
            sessionProperties.remove("UserInfo")
        }
        storeSessionData()
    }
    def setData(key: String, data: Map[String, Any]) = {
        sessionProperties.setJsonData(key, data)
    }
    def getData(key: String): Map[String, Any] = {
        sessionProperties.getJsonData(key)
    }

    def removeData(key: String) {
        sessionProperties.remove(key)
    }

    /**
     * find all file keys (i.e. those starting with "unpackaged/...") and if that key is not in keepKeys then reset all remote attributes
     * such as Id and LastModifiedDate
     * @param keepKeys - keys of files to keep
     * @return - original values of cleaned entries before reset
     */
    def resetData(keepKeys: Set[String]): Map[String, Any] = {
        val oldValues = Map.newBuilder[String, Any]
        for (key <- getFileKeysInSession) {
            if (!keepKeys.contains(key)) {
                val oldData = getData(key)
                oldValues += key -> oldData
                val newData = oldData.filterKeys(_field => !Set("Id", "LastModifiedDate", "LastModifiedDateMills").contains(_field))
                setData(key, newData)
            }

        }
        storeSessionData()
        oldValues.result()
    }

    /**
     * using keys stored in session return all those that point to files
     * such keys always start with prefix: "unpackaged/"
     * @return list of files in the following format
     *         src/classes/Class1.cls
     *         src/classes/OtherClass.cls
     *         src/pages/MyPage.page
     *
     */
    def getRelativeFilePathsInSession: List[String] = {
        val fileNameKeys = getFileKeysInSession
        fileNameKeys.map(key => getRelativeFilePathByKey(key.toString))
    }

    /**
     * using keys stored in session return all those that point to files
     * such keys always start with prefix: "unpackaged/"
     * @return list of keys that start with "unpackaged/"
     */
    def getFileKeysInSession: List[String] = {
        val names = sessionProperties.propertyNames()
        val prefix = "unpackaged/"
        val fileNameKeys = names.asScala.toList.filter(_.toString.startsWith(prefix)).map(_.toString)
        fileNameKeys
    }

    /**
     * @param extraSrcFoldersToLookIn - set of extra folders where to search for project files
     *                                this parameter is useful when result will be merged from several folders
     *                                e.g. refresh result is a combination of project files + files brought by retrieve() and stored in temp folder
     * @return list of relative file paths (e.g. src/classes/MyClass.cls) that exist in session but do not exist locally as a file
     */
    def getDeletedLocalFilePaths(extraSrcFoldersToLookIn: List[File]):List[String] = {
        val config = getConfig
        val allFiles = (config.srcDir :: extraSrcFoldersToLookIn).map(FileUtils.listFiles(_, descentIntoFolders = true, includeFolders = true)).flatten
        val existingFiles = allFiles.filter(
            //remove all non apex files
            file => file.isDirectory || DescribeMetadata.isValidApexFile(this, file)
        ).map(getRelativePath(_))
            //normalise all paths to start with src/ rather than unpackaged/
            .map(name => if (name.startsWith("unpackaged/")) name.replaceFirst("unpackaged/", "src/") else name)
            .toSet

        val relativePathsInSession = this.getRelativeFilePathsInSession
        val deletedFilePaths = relativePathsInSession.filterNot(existingFiles.contains(_))
        deletedFilePaths
    }

    /**
     * return relative path inside project folder
     * this path is used as key in session
     * @param file - resource under project folder
     * @return - string, looks like: unpackaged/pages/Hello.page
     */
    def getKeyByFile(file: File): String = {
        val relativePath = getRelativePath(file)
        getKeyByRelativeFilePath(relativePath)
    }

    def getKeyByRelativeFilePath(filePath: String): String = {
        val normalPath = normalizePath(filePath)
        val relPath = if (normalPath.startsWith("src/"))
            normalPath.replaceFirst("src/", "unpackaged/")
        else
            normalPath
        if (relPath.endsWith("-meta.xml"))
            relPath.substring(0, relPath.length - "-meta.xml".length)
        else
            relPath
    }

    /**
     * @param key - e.g. unpackaged/classes/MyClass.cls
     * @return - e.g. src/classes/MyClass.cls
     */
    def getRelativeFilePathByKey(key: String): String = {
        if (key.startsWith("unpackaged/"))
            key.replaceFirst("unpackaged/", "src/")
        else
            key
    }

    /**
     * in order to be used for session 'key' purpose file name must contain unix separator '/' as opposed to Windows one
     * @return turns path\\to\file into path/to/file
     */
    def normalizePath(filePath: String) = FileUtils.normalizePath(filePath)

    /**
     * @param dirName - e.g. "classes"
     * @param fileName - e.g. "Messages.cls"
     * @return relative path in project folder, e.g. src/classes.MyClass.cls
     */
    def getRelativePath(dirName: String, fileName: String): Option[String] = {
        if (sessionProperties.containsKey("unpackaged/" + dirName + "/" + fileName)) {
            Some("src" + File.separator + dirName + File.separator + fileName)
        } else {
            //looks like this file is not in session yet, last attempt, check if local file exists
            val folder = new File(config.srcDir, dirName)
            val file = new File(folder, fileName)
            if (file.canRead)
                Some(getRelativePath(file))
            else
                None
        }
    }

    /**
     * return relative path inside project folder
     * this path is used as key in session
     * @param file - resource under project folder
     * @return - string, looks like: unpackaged/pages/Hello.page
     */
    def getRelativePath(file: File): String = {
        //find parent src/ or unpackaged/
        FileUtils.getParentByName(file, Set("src", "unpackaged")) match {
          case Some(srcFolder) if null != srcFolder.getParentFile =>
              val projectPath = srcFolder.getParentFile.getAbsolutePath + File.separator
              val res = file.getAbsolutePath.substring(projectPath.length)
              normalizePath(res)
          case None => file.getAbsolutePath
        }
    }
    /**
     * @param fileName, e.g. "MyClass"
     * @param extension, e.g. "cls"
     * @return relative path in project folder, e.g. src/classes.MyClass.cls
     */
    def getRelativeFilePath(fileName: String, extension: String ): Option[String] = {

        val path = DescribeMetadata.getXmlNameBySuffix(this, extension) match {
            case Some(xmlTypeName) => DescribeMetadata.getMap(this).get(xmlTypeName)  match {
                case Some(describeMetadataObject) =>
                    getRelativePath(describeMetadataObject.getDirectoryName, fileName + "." + extension)
                case _ => None
            }
            case _ => None
        }
        path
    }
    /**
     * using only file name, e.g. MyClass and "ApexClass" try to find existing file
     * @param nameWithoutExtension, e.g. "MyClass"
     * @param xmlName, e.g. "ApexClass"
     * @return
     */
    def findFile(nameWithoutExtension: String, xmlName: String): Option[File] = {
        DescribeMetadata.getDirAndSuffix(this, xmlName) match {
          case Some((dirName, suffix)) =>
              getRelativePath(dirName, nameWithoutExtension + "." + suffix) match {
                case Some(relativePath) =>
                    val f = new File(config.projectDir, relativePath)
                    if (f.canRead) Some(f) else None
                case _ => None
              }
          case _ => None
        }
    }
    /**
     * keys usually look like so: unpackaged/classes/Messages.cls
     * @param dirName - e.g. "classes"
     * @param fileName - e.g. "Messages.cls"
     * @return
     */
    def findKey(dirName: String, fileName: String): Option[String] = {
        val key = "unpackaged/" + dirName + "/" + fileName
        if (sessionProperties.containsKey(key)) {
            Some(key)
        } else {
            None
        }
    }

    //Windows does not support cp -p (preserve last modified date) copy so have to assume that copy of all project files
    //on refresh takes no longer than this number of seconds
    private val SESSION_TO_FILE_TIME_DIFF_TOLERANCE_SEC = if (config.isUnix) 0 else 1000 * 3

    //if .cls and .cls-meta.xml file time differs by this number of seconds (or less) we consider time equal
    private val FILE_TO_META_TIME_DIFF_TOLERANCE_SEC = 1000

    /**
     * NOTE: this method does not check if provided file is a valid apex project file
     * @return true if file does not exist in session.properties or its hash does not match
     */
    def isModified(file: File): Boolean = {
        val prefix =  if (file.getName.endsWith("-meta.xml")) "meta" else ""
        val fileData = getData(getKeyByFile(file))
        val useMD5Hash = !fileData.getOrElse(MetadataType.MD5, "").asInstanceOf[String].isEmpty

        val hashFieldName = if (useMD5Hash) MetadataType.MD5 else MetadataType.CRC32
        val useHashCheck = fileData.contains(hashFieldName)

        val fileDiffBySessionData = useHashCheck match {
          case true =>
              val hashDifference = fileData.get(prefix + hashFieldName) match {
                  case Some(storedHash) =>
                      if (useMD5Hash) FileUtils.getMD5Hash(file) != storedHash
                      else FileUtils.getCRC32Hash(file)!= storedHash
                  case None => true //file is not listed in session, so must be new
              }
              hashDifference
          case false =>
              val fileTimeNewerThanSessionTimeData = fileData.get(prefix + MetadataType.LOCAL_MILLS) match {
                  case Some(x) => Math.abs(file.lastModified() - x.asInstanceOf[Long]) > SESSION_TO_FILE_TIME_DIFF_TOLERANCE_SEC
                  case None => true //file is not listed in session, so must be new
              }
              fileTimeNewerThanSessionTimeData

        }
        fileDiffBySessionData
    }

    /**
     * using text file with list of relative file paths
     * - check each file to be Apex file type
     * - check if file exists
     *
     * @param fileListFile - file with following structure
     * -------------------------------
     * src/classes/AccountHandler.cls
     * src/triggers/Account.trigger
     * ...
     * -------------------------------
     * @return list of valid files
     */
    def listApexFilesFromFile(fileListFile: File): List[File] = {
        val config = this.getConfig

        //logger.debug("packageXmlData=" + packageXmlData)
        val projectDir = config.projectDir

        //load file list from specified file
        val files:List[File] = FileUtils.readFile(fileListFile).getLines().map(relativeFilePath => new File(projectDir, relativeFilePath)).toList

        //for each file check that it exists
        files.find(!_.canRead) match {
            case Some(f) =>
                throw new ActionError("Can not read file: " + f.getAbsolutePath)
            case None =>
        }

        val allFiles  = files.filter(
            //remove all non apex files
            file => DescribeMetadata.isValidApexFile(this, file)
        ).toSet

        allFiles.toList
    }

    private def getPartnerConnection: PartnerConnection = {
        val conn = connectionPartner match {
          case Some(connection) => connection
          case None =>
              //check if we have previously established session id
              getSavedConnectionData match {
                  case (Some(sessionId), Some(serviceEndpoint)) =>
                      //use cached data
                      Connection.getPartnerConnection(config, sessionId, serviceEndpoint)
                  case _ =>
                      //login explicitly
                      val _conn = Connection.createPartnerConnection(config)
                      storeConnectionData(_conn.getConfig)
                      _conn
              }

        }
        connectionPartner = Some(conn)

        conn
    }
    private def getMetadataConnection: MetadataConnection = {
        import com.sforce.soap.metadata._
        val conn = connectionMetadata match {
            case Some(connection) => connection
            case None => Connection.getMetadataConnection(config, getPartnerConnection)
        }
        val debugHeader = new DebuggingHeader_element()
        debugHeader.setDebugLevel(LogType.valueOf(config.logLevel))
        conn.__setDebuggingHeader(debugHeader)

        connectionMetadata = Some(conn)
        conn
    }

    private def getToolingConnection: com.sforce.soap.tooling.ToolingConnection = {
        import com.sforce.soap.tooling._
        val conn = connectionTooling match {
            case Some(connection) => connection
            case None => Connection.getToolingConnection(config, getPartnerConnection)
        }
        val debugHeader = new DebuggingHeader_element()
        debugHeader.setDebugLevel(LogType.valueOf(config.logLevel))
        conn.__setDebuggingHeader(debugHeader)

        connectionTooling = Some(conn)
        conn
    }

    private def getApexConnection: com.sforce.soap.apex.SoapConnection = {
        import com.sforce.soap.apex._
        val conn = connectionApex match {
            case Some(connection) => connection
            case None => Connection.getApexConnection(config, getPartnerConnection)
        }
        connectionApex = Some(conn)

        /*
        val infoAll = new LogInfo()
        infoAll.setCategory(LogCategory.All)
        infoAll.setLevel(LogCategoryLevel.Finest)

        val infoApex = new LogInfo()
        infoApex.setCategory(LogCategory.Apex_code)
        infoApex.setLevel(LogCategoryLevel.Finest)

        val infoProfiling = new LogInfo()
        infoProfiling.setCategory(LogCategory.Apex_profiling)
        infoProfiling.setLevel(LogCategoryLevel.Finest)

        val infoDB = new LogInfo()
        infoDB.setCategory(LogCategory.Db)
        infoDB.setLevel(LogCategoryLevel.Finest)

        conn.setDebuggingHeader(Array(infoAll, infoApex, infoProfiling, infoDB), LogType.Detail)
        //conn.setDebuggingHeader(Array(), LogType.Detail)
        */


        val debugHeader = new DebuggingHeader_element()
        //debugHeader.setCategories(Array(infoAll, infoApex, infoProfiling, infoDB))
        debugHeader.setDebugLevel(LogType.valueOf(config.logLevel))
        conn.__setDebuggingHeader(debugHeader)

        conn
    }

    /**
     * @param connectorConfig - e.g. getToolingConnection.getConfig
     * @param apiPath e.g. /tooling or blank for Partner API
     * @param path - API specific path, e.g. "/query/" for /query/?q=...
     * @param urlParameters - query string, e.g. "q=select+Id+from+User"
     * @param httpHeaders - any extra HTTP headers
     * @return
     */
    private def getRestContent(connectorConfig: com.sforce.ws.ConnectorConfig, apiPath: String,
                               path: String, urlParameters: String = "",
                               httpHeaders: java.util.HashMap[String, String] = new java.util.HashMap[String, String]()
                                  ): Try[String] = {
        val endpointUrl = new URL(connectorConfig.getServiceEndpoint)
        //get protocol and domain, e.g.:  https://na1.salesforce.com/
        val domain = endpointUrl.getProtocol + "://" + endpointUrl.getHost + "/"

        val queryStr:String = if ("" != urlParameters) "?" + urlParameters else ""
        val url = domain + s"services/data/v${config.apiVersion}$apiPath" + path + queryStr
        val conn = connectorConfig.createConnection(new URL(url), httpHeaders, true)
        conn.setRequestProperty("Authorization", "Bearer " + getPartnerConnection.getSessionHeader.getSessionId)
        conn.setRequestProperty("Content-Type", "application/json")
        conn.setRequestMethod("GET")
        conn.setDoOutput(true)
        conn.setDoInput(true)
        //conn.connect()
        //conn.setReadTimeout(...)

        //send request
        val responseCode = conn.getResponseCode
        if (200 == responseCode) {
            val in = conn.getInputStream
            val text = conn.getContentEncoding match {
                case "gzip" => io.Source.fromInputStream(new GZIPInputStream(in))("UTF-8").mkString("")
                case _ => io.Source.fromInputStream(in)("UTF-8").mkString("")
            }
            //TODO consider saving into file, as whole stream may not fit in RAM
            in.close()
            Success(text)
        } else {
            if (401 == responseCode) { //Unauthorized
                throw new Session.UnauthorizedConnectionException(conn)
            } else {
                logger.error(s"Request Failed - URL=$url")
                logger.error(s"Response Code: $responseCode, Response Message: ${conn.getResponseMessage}")
                throw new Session.ConnectionException(conn)
            }
        }
    }

    private def withRetry(codeBlock: => Any) = {
        try {
            codeBlock
        } catch {
            case ex:com.sforce.ws.SoapFaultException if "INVALID_SESSION_ID" == ex.getFaultCode.getLocalPart =>
                logger.debug("Session is invalid or has expired. Will run the process again with brand new connection. ")
                logger.trace(ex)
                reset()
                //run once again
                codeBlock
            case ex:com.sforce.ws.ConnectionException =>
                //sometimes WSC library returns ConnectionException instead of SoapFaultException when session is invalid
                logger.trace(ex)
                reset()
                //run once again
                codeBlock
            case ex:Throwable =>
                throw ex
        }
    }

    /**
     * use this for sending REST requests, as opposed to withRetry{} which is for SOAP requests
     * @param codeBlock - unit of code to execute
     * @return
     */
    private def withRetryRest(codeBlock: => Any) = {
        try {
            codeBlock
        } catch {
            case ex:Session.UnauthorizedConnectionException =>
                logger.debug("Session is invalid or has expired. Will run the process again with brand new connection. ")
                logger.trace(ex)
                reset()
                //run once again
                codeBlock
            case ex: Session.ConnectionException =>
                logger.trace(ex)
                reset()
                //run once again
                codeBlock
            case ex:Throwable =>
                throw ex
        }
    }

    def reset() {
        sessionProperties.remove("session")
        storeSessionData()
        connectionPartner = None
        connectionMetadata = None
        connectionTooling = None
        connectionApex = None
    }

    def getUserId: String = {
        if (getData("UserInfo").isEmpty) {
            //load from server
            val userInfoResult = getUserInfo
            val userInfoMap = Map (
                "UserId" -> userInfoResult.getUserId,
                "OrganizationId" -> userInfoResult.getOrganizationId
            )
            setData("UserInfo", userInfoMap)
        }
        getData("UserInfo")("UserId").asInstanceOf[String]
    }
    /***************** PartnerConnection ********************************************/
    /**
     * @param path - e.x. /sobjects/ApexLog/id/Body/
     * @param urlParameters: param1=aaa&paramX=123...
     * @return
     */
    def getRestContentPartner(path: String, urlParameters: String = "",
                              httpHeaders: java.util.HashMap[String, String] = new java.util.HashMap[String, String]()): Option[String] = {
        val text = withRetryRest {
            val connectorConfig = getPartnerConnection.getConfig

            getRestContent(connectorConfig,"", path, urlParameters, httpHeaders) match {
                case Success(responseText) => responseText
                case Failure(ex) => throw ex
            }
        }.asInstanceOf[String]
        Some(text)
    }

    def getServerTimestamp = {
        withRetry {
            getPartnerConnection.getServerTimestamp
        }.asInstanceOf[com.sforce.soap.partner.GetServerTimestampResult]
    }

    def getUserInfo = {
        withRetry {
            getPartnerConnection.getUserInfo
        }.asInstanceOf[com.sforce.soap.partner.GetUserInfoResult]
    }

    def query(queryString: String):com.sforce.soap.partner.QueryResult = {
        val queryResult = withRetry {
            val conn = getPartnerConnection
            conn.query(queryString)
        }.asInstanceOf[com.sforce.soap.partner.QueryResult]
        queryResult
    }

    def queryMore(queryLocator: String):com.sforce.soap.partner.QueryResult = {
        val queryResult = withRetry {
            val conn = getPartnerConnection
            conn.queryMore(queryLocator)
        }.asInstanceOf[com.sforce.soap.partner.QueryResult]
        queryResult
    }

    def describeSObjects(sobjectApiNames: List[String]): List[com.sforce.soap.partner.DescribeSObjectResult] = {
        val describeResult = withRetry {
            val conn = getPartnerConnection
            val res = conn.describeSObjects(sobjectApiNames.toArray)
            res.toList
        }.asInstanceOf[List[com.sforce.soap.partner.DescribeSObjectResult]]
        describeResult
    }

    def describeGlobal: com.sforce.soap.partner.DescribeGlobalResult = {
        val describeResult = withRetry {
            val conn = getPartnerConnection
            val res = conn.describeGlobal()
            res
        }.asInstanceOf[com.sforce.soap.partner.DescribeGlobalResult]
        describeResult
    }

    /***************** MetadataConnection ********************************************/
    def retrieve(retrieveRequest: RetrieveRequest ):RetrieveResult = {
        val retrieveResult = withRetry {
            val conn = getMetadataConnection
            val _retrieveResult = waitRetrieve(conn, conn.retrieve(retrieveRequest))
            //val _retrieveResult = conn.checkRetrieveStatus(asyncResult.getId)
            _retrieveResult
        }.asInstanceOf[RetrieveResult]

        retrieveResult
    }

    def deploy(zipFile: Array[Byte], deployOptions: DeployOptions ):(DeployResult, String) = {
        var log = ""
        val deployResult = withRetry {
            val conn = getMetadataConnection
            //val asyncResult = wait(conn, conn.deploy(zipFile, deployOptions))
            val asyncResult = waitDeploy(conn, conn.deploy(zipFile, deployOptions))
            val _deployResult = conn.checkDeployStatus(asyncResult.getId, true)
            log = if (null != conn.getDebuggingInfo) conn.getDebuggingInfo.getDebugLog else ""
            _deployResult
        }.asInstanceOf[DeployResult]

        (deployResult, log)
    }

    def describeMetadata(apiVersion: Double ):DescribeMetadataResult = {
        val describeResult = withRetry {
            val conn = getMetadataConnection
            conn.describeMetadata(apiVersion)
        }.asInstanceOf[DescribeMetadataResult]
        describeResult
    }

    def listMetadata(queries: Array[ListMetadataQuery], apiVersion: Double ):Array[FileProperties] = {
        //sfdc allows only 3 queries per call, so have to micro batch calls
        def microBatch(queriesBatch: Array[ListMetadataQuery], conn: MetadataConnection,
                       propsSoFar: Array[FileProperties]):Array[FileProperties] = {
            queriesBatch match  {
                case Array() => //end of query list, return result
                    propsSoFar
                case Array(_, _*) =>
                    val _queries = queriesBatch.take(3)
                    logger.trace("About to process " + _queries.map(_.getType).mkString("; "))
                    val props = conn.listMetadata(_queries, apiVersion)
                    //sometimes SFDC returns props with fullname = "_Default" and type = null
                    //exclude those
                    val propsFiltered = props.filter(_.getType != null)
                    logger.trace("Props: " + propsFiltered.map(_.getType).mkString("; "))
                    microBatch(queriesBatch.drop(3), conn, propsFiltered ++ propsSoFar)

            }

        }
        val fileProperties = withRetry {
            val conn = getMetadataConnection
            //conn.listMetadata(queries, apiVersion)
            microBatch(queries, conn, Array[FileProperties]())
        }.asInstanceOf[Array[FileProperties]]
        fileProperties
    }

    /*
    def delete(metadata: Array[com.sforce.soap.metadata.Metadata] ):AsyncResult = {
        val deleteResult = withRetry {
            val conn = getMetadataConnection
            conn.delete(metadata)
        }.asInstanceOf[AsyncResult]
        deleteResult
    }
    */


    /***************** ApexConnection ********************************************/
    def executeAnonymous(apexCode: String ):(com.sforce.soap.apex.ExecuteAnonymousResult, String) = {
        var log = ""
        val executeAnonymousResult = withRetry {
            val conn = getApexConnection
            val res = conn.executeAnonymous(apexCode)
            log = if (null != conn.getDebuggingInfo) conn.getDebuggingInfo.getDebugLog else ""
            res

        }.asInstanceOf[com.sforce.soap.apex.ExecuteAnonymousResult]
        (executeAnonymousResult, log)
    }

    /***************** ToolingConnection ********************************************/

    /**
     * @param path - e.x. /sobjects/ApexLog/id/Body/
     * @param urlParameters: param1=aaa&paramX=123...
     * @return
     */
    def getRestContentTooling(path: String, urlParameters: String = "",
                              httpHeaders: java.util.HashMap[String, String] = new java.util.HashMap[String, String]()): Option[String] = {
        val text = withRetryRest {
            val connectorConfig = getToolingConnection.getConfig
            //convert this:
            //  https://domain/services/Soap/T/33.0/00Dg0000006S2Wp
            //to this:
            //  https://domain/services/data/v33.0/tooling/sobjects/ApexLog/id/Body/
            //val domainEndIndex = connectorConfig.getServiceEndpoint.indexOf("services/Soap/T")
            //val domain = connectorConfig.getServiceEndpoint.substring(0, domainEndIndex)
            getRestContent(connectorConfig, "/tooling", path, urlParameters, httpHeaders) match {
                case Success(responseText) => responseText
                case Failure(ex) => throw ex
            }
        }.asInstanceOf[String]
        Some(text)
    }

    def describeTooling:com.sforce.soap.tooling.DescribeGlobalResult = {
        val describeResult = withRetry {
            val conn = getToolingConnection
            conn.describeGlobal()
        }.asInstanceOf[com.sforce.soap.tooling.DescribeGlobalResult]
        describeResult
    }

    def createTooling(objects: Array[com.sforce.soap.tooling.SObject]):Array[com.sforce.soap.tooling.SaveResult] = {
        val saveResult = withRetry {
            val conn = getToolingConnection
            conn.create(objects)
        }.asInstanceOf[Array[com.sforce.soap.tooling.SaveResult]]
        saveResult
    }
    def updateTooling(objects: Array[com.sforce.soap.tooling.SObject]):Array[com.sforce.soap.tooling.SaveResult] = {
        val saveResult = withRetry {
            val conn = getToolingConnection
            conn.update(objects)
        }.asInstanceOf[Array[com.sforce.soap.tooling.SaveResult]]
        saveResult
    }

    def deleteTooling(ids: Array[String]):Array[com.sforce.soap.tooling.DeleteResult] = {
        val deleteResult = withRetry {
            val conn = getToolingConnection
            conn.delete(ids)
        }.asInstanceOf[Array[com.sforce.soap.tooling.DeleteResult]]
        deleteResult
    }
    def deleteTooling(id: String):Array[com.sforce.soap.tooling.DeleteResult] = {
        deleteTooling(Array(id))
    }

    def queryTooling(queryString: String):com.sforce.soap.tooling.QueryResult = {
        val queryResult = withRetry {
            val conn = getToolingConnection
            conn.query(queryString)
        }.asInstanceOf[com.sforce.soap.tooling.QueryResult]
        queryResult
    }
    def queryMoreTooling(queryLocator: String):com.sforce.soap.tooling.QueryResult = {
        val queryResult = withRetry {
            val conn = getToolingConnection
            conn.queryMore(queryLocator)
        }.asInstanceOf[com.sforce.soap.tooling.QueryResult]
        queryResult
    }

    def retrieveTooling(fields: List[String], xmlTypeName: String, ids: List[String]):Array[com.sforce.soap.tooling.SObject] = {
        val sobjects = withRetry {
            val conn = getToolingConnection
            conn.retrieve(fields.mkString(","), xmlTypeName, ids.toArray)
        }.asInstanceOf[Array[com.sforce.soap.tooling.SObject]]
        sobjects
    }

    def executeAnonymousTooling(apexCode: String ):(com.sforce.soap.tooling.ExecuteAnonymousResult, String) = {
        var log = ""
        val executeAnonymousResult = withRetry {
            val conn = getToolingConnection
            val res = conn.executeAnonymous(apexCode)
            log = if (null != conn.getDebuggingInfo) conn.getDebuggingInfo.getDebugLog else ""
            res

        }.asInstanceOf[com.sforce.soap.tooling.ExecuteAnonymousResult]
        (executeAnonymousResult, log)
    }

    def runTestsTooling(runTestsRequest: com.sforce.soap.tooling.RunTestsRequest ):(com.sforce.soap.tooling.RunTestsResult) = {
        val runTestsResult = withRetry {
            val conn = getToolingConnection
            val res = conn.runTests(runTestsRequest)
            res

        }.asInstanceOf[com.sforce.soap.tooling.RunTestsResult]
        runTestsResult
    }

    def runTestsAsyncTooling(classIds: List[String]):(String) = {
        val runTestsResult = withRetry {
            val conn = getToolingConnection
            val res = conn.runTestsAsynchronous(classIds.mkString(","))
            res

        }.asInstanceOf[String]
        runTestsResult
    }

    ////////////////////////////////////////////////////////////////////////////////////
    private val ONE_SECOND = 1000
    private val MAX_NUM_POLL_REQUESTS = config.getProperty("maxPollRequests").getOrElse[String]("100").toInt

    private def waitRetrieve(connection: MetadataConnection, asyncResult: AsyncResult): RetrieveResult = {
        val waitTimeMilliSecs = config.getProperty("pollWaitMillis").getOrElse("" + (ONE_SECOND * 5)).toInt
        var attempts = 0
        var lastReportTime = System.currentTimeMillis()

        val includeZip = true
        var retrieveResult =  connection.checkRetrieveStatus(asyncResult.getId, includeZip)
        while (!retrieveResult.isDone) {
            val reportAttempt = (System.currentTimeMillis() - lastReportTime) > (ONE_SECOND * 3)
            blocking {
                Thread.sleep(waitTimeMilliSecs)
            }
            //report only once every 3 seconds
            if (reportAttempt) {
                logger.info("waiting result, poll #" + attempts)
                lastReportTime = System.currentTimeMillis()
            } else {
                logger.trace("waiting result, poll #" + attempts)
            }
            attempts += 1
            retrieveResult =  connection.checkRetrieveStatus(asyncResult.getId, includeZip)
            if (!retrieveResult.isDone && ((attempts +1) > MAX_NUM_POLL_REQUESTS)) {
                throw new Exception("Request timed out.  If this is a large set " +
                    "of metadata components, check that the time allowed " +
                    "by --maxPollRequests is sufficient and --pollWaitMillis is not too short.")
            }

            logger.debug("Status is: " + retrieveResult.getStatus.toString)
        }
        if (!retrieveResult.isSuccess) {
            throw new Exception(retrieveResult.getStatus + " msg:" + retrieveResult.getMessages.mkString("\n"))
        }
        retrieveResult
    }

    private def waitDeploy(connection: MetadataConnection, asyncResult: AsyncResult): DeployResult = {
        val waitTimeMilliSecs = config.getProperty("pollWaitMillis").getOrElse("" + (ONE_SECOND * 5)).toInt
        var attempts = 0

        var lastReportTime = System.currentTimeMillis()
        val oldMessages = scala.collection.mutable.Set[String]()

        var deployResult =  connection.checkDeployStatus(asyncResult.getId, false)
        while (!deployResult.isDone) {
            val reportAttempt = (System.currentTimeMillis() - lastReportTime) > (ONE_SECOND * 3)
            blocking {
                Thread.sleep(waitTimeMilliSecs)
            }
            //report only once every 3 seconds
            if (reportAttempt) {
                logger.info("waiting result, poll #" + attempts)
                lastReportTime = System.currentTimeMillis()
            } else {
                logger.trace("waiting result, poll #" + attempts)
            }
            attempts += 1
            val fetchDetails = attempts % 3 == 0
            deployResult =  connection.checkDeployStatus(asyncResult.getId, fetchDetails)
            // Fetch in-progress details once for every 3 polls
            if (fetchDetails) {
                oldMessages ++= displayDeployProgress(connection, asyncResult.getId, oldMessages.toSet)
            }

            if (!deployResult.isDone && ((attempts +1) > MAX_NUM_POLL_REQUESTS)) {
                throw new Exception("Request timed out.  If this is a large set " +
                    "of metadata components, check that the time allowed " +
                    "by --maxPollRequests is sufficient and --pollWaitMillis is not too short.")
            }


            logger.debug("Status is: " + deployResult.getStatus.toString)
        }

        if (!deployResult.isSuccess && null != deployResult.getErrorStatusCode ) {
            throw new Exception(deployResult.getErrorStatusCode + " msg:" + deployResult.getErrorMessage)
        }
        deployResult
    }


    /**
     * when wait() is running during deploy() call we can show some useful progress information while user is waiting
     * @param connection - live MetadataConnection
     * @param asyncResultId - id of current async process
     * @param displayedMessages - messages displayed so far (used to make sure we do not show same message twice)
     * @return
     */
    private def displayDeployProgress(connection: MetadataConnection, asyncResultId: String, displayedMessages: Set[String]):Set[String] = {
        val oldMessages = scala.collection.mutable.Set[String]() ++ displayedMessages

        val _deployResult = connection.checkDeployStatus(asyncResultId, true)
        if (null != _deployResult.getStateDetail) {
            logger.info(_deployResult.getStateDetail)
        }
        if (_deployResult.getNumberComponentsTotal > 0) {
            val componentsMessage = "COMPONENTS Total/Deployed/Errors: " +
                s"${_deployResult.getNumberComponentsTotal}/${_deployResult.getNumberComponentsDeployed}/${_deployResult.getNumberComponentErrors}"
            if (!oldMessages.contains(componentsMessage)) {
                logger.info(componentsMessage)
                oldMessages += componentsMessage
            }
        }

        val deployDetails = _deployResult.getDetails
        if (null != deployDetails && null != deployDetails.getRunTestResult) {

            if (_deployResult.getNumberTestsTotal > 0) {
                val testMessage = "TESTS Total/Completed/Errors: "+
                    s"${_deployResult.getNumberTestsTotal}/${_deployResult.getNumberTestsCompleted}/${_deployResult.getNumberTestErrors}"
                if (!oldMessages.contains(testMessage)) {
                    logger.info(testMessage)
                    oldMessages += testMessage
                }
            }

            val runTestResult = deployDetails.getRunTestResult
            if (null != runTestResult ) {
                for (testFailure <- runTestResult.getFailures) {
                    val message = testFailure.getMessage
                    if (!oldMessages.contains(message)) {
                        logger.info(testFailure.getMethodName + ": " + message)
                        oldMessages += message
                    }
                }
            }
        }
        oldMessages.toSet
    }


}
