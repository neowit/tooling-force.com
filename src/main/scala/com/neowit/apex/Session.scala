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
import java.security.SecureRandom
import java.util.zip.{GZIPInputStream, GZIPOutputStream}

import com.neowit.utils._
import com.sforce.soap.partner.PartnerConnection
import com.sforce.soap.metadata._
import spray.json._

import scala.concurrent._
import collection.JavaConverters._
import java.io._

import com.neowit.TcpServer
import com.neowit.apex.LogUtils.{ApexLogInfoCreatorProvider, MetadataLogInfoCreatorProvider, ToolingLogInfoCreatorProvider}
import com.neowit.apex.actions.{ActionError, DescribeMetadata}
import com.neowit.auth.{LoginPasswordCredentials, Oauth2Credentials}

import scala.util.{Failure, Success, Try}

/**
 * manages local data store related to specific project
 */
object Session {

    case class ConnectionException(connection: HttpURLConnection) extends RuntimeException
    case class RestCallException(connection: HttpURLConnection) extends RuntimeException {
        private val jsonAst = Try(io.Source.fromInputStream(connection.getErrorStream)("UTF-8").getLines().mkString("")) match {
            case Success(responseDetails) => Try(responseDetails.parseJson) match {
                case Success(_ast) => Some(_ast)
                case _ => None
            }
            case _ => None
        }
        def getRestMessage: Option[String] = getRestResponseField("message")
        def getRestErrorCode: Option[String] = getRestResponseField("errorCode")

        private def getRestResponseField(fName: String): Option[String] = {
            jsonAst match {
                case Some(_ast) => Try(_ast.asInstanceOf[JsArray].elements.head.asJsObject().fields.getOrElse(fName, new JsString("")).asInstanceOf[JsString]) match {
                    case Success(message) => Some(message.value)
                    case _ => None
                }
                case None => None
            }
        }

    }
    case class UnauthorizedConnectionException(connection: HttpURLConnection) extends RuntimeException

    def apply(basicConfig: BasicConfig, isReadOnly: Boolean = true) = new Session(basicConfig, isReadOnly)
}

case class StoredSession(
                         sessionId: Option[String],
                         hash: Option[String],
                         serviceEndpoint: Option[String]
                         )

/**
 * Session has following responsibilities
 * 1. Maintains/stores persistent connection and can resue connection
 * 2. Maintains cache of some important information about project metadata (Pages, Classes, etc)
 *
 * @param basicConfig - main application config
 */
class Session(val basicConfig: BasicConfig, isReadOnly: Boolean = true) extends Logging {

    val config = new ConfigWithSession(basicConfig)

    private val sessionPropertiesOpt = config.lastSessionPropsOpt
    private var connectionPartner:Option[PartnerConnection] = None
    private var connectionMetadata:Option[MetadataConnection] = None
    private var connectionTooling:Option[com.sforce.soap.tooling.ToolingConnection] = None
    private var connectionApex:Option[com.sforce.soap.apex.SoapConnection] = None

    //when user wants to work with files from one org and deploy them in another org we can not use stored session
    lazy val callingAnotherOrg:Boolean = config.getProperty("callingAnotherOrg").getOrElse("false").toBoolean

    def getConfig: ConfigWithSession = config


    def storeSessionData(allowWrite: Boolean = false): Unit = {
        if (isReadOnly && !allowWrite) {
            throw new IllegalAccessError("Attempted to write in Read/Only session")
        }
        config.storeSessionProps()
    }

    def getSavedConnectionData :(Option[String], Option[String]) = {

        val emptySession = (None, None)
        if (!callingAnotherOrg) {
            val connectionData = getData("session")
            connectionData.get("hash") match {
                case Some(hash) if getHash == hash =>
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
        config.getAuthConfig match {
            case Some(credentials) => credentials.hashCode().toString
            case None =>
                // generate random number, to make sure it does nto match any existing (real) hash
                val random = new SecureRandom()
                val bytes = new Array[Byte](8)
                random.nextBytes(bytes).toString
        }
    }

    /*
    private def getHash(tokens: Oauth2Tokens): String = {
        val md5 = MessageDigest.getInstance("SHA-256")
        md5.reset()
        val str = tokens.id.getOrElse("")
        md5.digest(str.getBytes("UTF-8")).map(0xFF & _).map { "%02x".format(_) }.foldLeft(""){_ + _}
    }
    */


    override def hashCode(): Int = getHash.hashCode

    override def equals(p1: scala.Any): Boolean = {
        p1.isInstanceOf[Session] && this.hashCode() == p1.asInstanceOf[Session].hashCode()
    }

    def storeConnectionData(connectionConfig: com.sforce.ws.ConnectorConfig, allowWrite: Boolean = false): Unit = {
        if (isReadOnly && !allowWrite) {
            throw new IllegalAccessError("Attempted to write in Read/Only session")
        }
        sessionPropertiesOpt match {
            case Some(sessionProperties) =>
                if (!callingAnotherOrg) {
                    sessionProperties.setJsonData("session",
                        Map(
                            "sessionId" -> connectionConfig.getSessionId,
                            "serviceEndpoint" -> connectionConfig.getServiceEndpoint,
                            "hash" -> getHash
                        )
                    )
                    remove("UserInfo")
                } else {
                    remove("session")
                    remove("UserInfo")
                }
            case None =>
        }
        storeSessionData(allowWrite)
    }

    /*
    def storeConnectionData(tokens: Oauth2Tokens, environment: String, allowWrite: Boolean) {
        if (isReadOnly && !allowWrite) {
            throw new IllegalAccessError("Attempted to write in Read/Only session")
        }
        if (!callingAnotherOrg) {
            getStoredSession
            sessionProperties.setJsonData("session", Map(
                "sessionId" -> tokens.access_token.getOrElse(""),
                "refresh_token" -> tokens.refresh_token.getOrElse(""),
                "instance_url" -> tokens.instance_url.getOrElse(""),
                "serviceEndpoint" -> (tokens.instance_url.getOrElse("") + "/services/Soap/u/" + config.apiVersion + "/" + tokens.getOrgId.getOrElse("")),
                "authType" -> "oauth2",
                "hash" -> getHash,
                "env" -> environment //login.salesforce.com or test.salesforce.com
            ))
            sessionProperties.remove("UserInfo")
        } else {
            sessionProperties.remove("session")
            sessionProperties.remove("UserInfo")
        }
        storeSessionData(allowWrite)
    }
    */
    def setData(key: String, data: Map[String, Any]): AnyRef = {
        if (isReadOnly) {
            throw new IllegalAccessError("Attempted to modify Read/Only session")
        }
        sessionPropertiesOpt.map(_.setJsonData(key, data))
    }
    def getData(key: String): Map[String, Any] = {
        sessionPropertiesOpt.map(_.getJsonData(key)).getOrElse(Map.empty[String, Any])
    }
    def getData(file: File): Map[String, Any] = {
        getData(getKeyByFile(file))
    }

    def removeData(key: String): Unit = {
        if (isReadOnly) {
            throw new IllegalAccessError("Attempted to modify Read/Only session")
        }
        remove(key)
    }

    /**
     * find all file keys (i.e. those starting with "unpackaged/...") and if that key is not in keepKeys then reset all remote attributes
     * such as Id and LastModifiedDate
     * @param keepKeys - keys of files to keep
     * @return - original values of cleaned entries before reset
     */
    def resetData(keepKeys: Set[String]): Map[String, Any] = {
        if (isReadOnly) {
            throw new IllegalAccessError("Attempted to modify Read/Only session")
        }
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
        sessionPropertiesOpt match {
            case Some(sessionProperties) =>
                val names = sessionProperties.propertyNames()
                val prefix = "unpackaged/"
                val fileNameKeys = names.asScala.toList.filter(_.toString.startsWith(prefix)).map(_.toString)
                fileNameKeys
            case None => Nil
        }
    }

    /**
     * @param extraSrcFoldersToLookIn - set of extra folders where to search for project files
     *                                this parameter is useful when result will be merged from several folders
     *                                e.g. refresh result is a combination of project files + files brought by retrieve() and stored in temp folder
     * @return list of relative file paths (e.g. src/classes/MyClass.cls) that exist in session but do not exist locally as a file
     */
    def getDeletedLocalFilePaths(extraSrcFoldersToLookIn: List[File]):List[String] = {
        config.srcDirOpt match {
            case Some(srcDir) =>
                val allFiles = (srcDir :: extraSrcFoldersToLookIn).flatMap(FileUtils.listFiles(_, descentIntoFolders = true, includeFolders = true))
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
            case None => Nil
        }
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
    def normalizePath(filePath: String): String = FileUtils.normalizePath(filePath)

    private def containsKey(key: String): Boolean = {
        sessionPropertiesOpt.exists(_.containsKey(key))
    }
    private def remove(key: String): Unit = {
        sessionPropertiesOpt.foreach(_.remove(key))
    }
    /**
     * @param dirName - e.g. "classes"
     * @param fileName - e.g. "Messages.cls"
     * @return relative path in project folder, e.g. src/classes.MyClass.cls
     */
    def getRelativePath(dirName: String, fileName: String): Option[String] = {
        if (containsKey("unpackaged/" + dirName + "/" + fileName)) {
            Some("src" + File.separator + dirName + File.separator + fileName)
        } else {
            //looks like this file is not in session yet, last attempt, check if local file exists
            config.srcDirOpt match {
                case Some(srcDir) =>
                    val folder = new File(srcDir, dirName)
                    val file = new File(folder, fileName)
                    if (file.canRead)
                        Some(getRelativePath(file))
                    else
                        None
                case None => None
            }
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
        config.projectDirOpt match {
            case Some(_projectDir) =>
                DescribeMetadata.getDirAndSuffix(this, xmlName) match {
                    case Some((dirName, suffix)) =>
                        getRelativePath(dirName, nameWithoutExtension + "." + suffix) match {
                            case Some(relativePath) =>
                                val f = new File(_projectDir, relativePath)
                                if (f.canRead) Some(f) else None
                            case _ => None
                        }
                    case _ => None
                }
            case None => None
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
        if (containsKey(key)) {
            Some(key)
        } else {
            None
        }
    }

    //Windows does not support cp -p (preserve last modified date) copy so have to assume that copy of all project files
    //on refresh takes no longer than this number of seconds
    private val SESSION_TO_FILE_TIME_DIFF_TOLERANCE_SEC = if (config.isUnix) 0 else 1000 * 3

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

        val fileDiffBySessionData = if (useHashCheck) {
            val hashDifference = fileData.get(prefix + hashFieldName) match {
                case Some(storedHash) =>
                    if (useMD5Hash) FileUtils.getMD5Hash(file) != storedHash
                    else FileUtils.getCRC32Hash(file) != storedHash
                case None => true //file is not listed in session, so must be new
            }
            hashDifference
        } else {
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

        config.projectDirOpt match {
            case Some(projectDir) =>
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
            case None => Nil
        }
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
                      val _conn =
                      config.getAuthConfig match {
                          case Some(LoginPasswordCredentials(_, _, _)) =>
                              //login explicitly
                              Connection.createPartnerConnection(config)

                          case Some(credentials @ Oauth2Credentials(_)) =>
                              Connection.createPartnerConnectionWithRefreshToken(config, credentials) match {
                                  case Right(newConnection) => newConnection
                                  case Left(err) => throw new ConfigValueException(err)
                              }
                          case None =>
                              throw new ConfigValueException("Please provide username/password/serverurl configuration or login explicitly using 'login' command.")
                      }
                      storeConnectionData(_conn.getConfig, allowWrite = true)
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
        val classInfoProvider = new MetadataLogInfoCreatorProvider
        val logLevelInfos = LogUtils.getDebugHeaderLogInfos(config.debuggingHeaderConfigPath, this, classInfoProvider)
        debugHeader.setCategories(logLevelInfos)

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
        val classInfoProvider = new ToolingLogInfoCreatorProvider
        val logLevelInfos = LogUtils.getDebugHeaderLogInfos(config.debuggingHeaderConfigPath, this, classInfoProvider)
        debugHeader.setCategories(logLevelInfos)
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

        val debugHeader = new DebuggingHeader_element()
        val classInfoProvider = new ApexLogInfoCreatorProvider
        val logLevelInfos = LogUtils.getDebugHeaderLogInfos(config.debuggingHeaderConfigPath, this, classInfoProvider)
        debugHeader.setCategories(logLevelInfos)
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
                throw Session.UnauthorizedConnectionException(conn)
            } else {
                logger.error(s"Request Failed - URL=$url")
                logger.error(s"Response Code: $responseCode, Response Message: ${conn.getResponseMessage}")
                val ex = Session.RestCallException(conn)
                logger.error(s"REST Response Code: ${ex.getRestErrorCode}, REST Response Message: ${ex.getRestMessage}")
                Failure(ex)
            }
        }
    }
    /**
     * @param connectorConfig - e.g. getToolingConnection.getConfig
     * @param apiPath e.g. /tooling or blank for Partner API
     * @param path - API specific path, e.g. "/query/" for /query/?q=...
     * @param jsonBody - {"key1" : "value", ...}
     * @param httpHeaders - any extra HTTP headers
     * @return
     */
    private def postRestContent(connectorConfig: com.sforce.ws.ConnectorConfig, apiPath: String,
                               path: String, jsonBody: String,
                               httpHeaders: java.util.HashMap[String, String] = new java.util.HashMap[String, String]()
                                  ): Try[JsValue] = {

        //import spray.json._




        val endpointUrl = new URL(connectorConfig.getServiceEndpoint)
        //get protocol and domain, e.g.:  https://na1.salesforce.com/
        val domain = endpointUrl.getProtocol + "://" + endpointUrl.getHost + "/"

        val url = domain + s"services/data/v${config.apiVersion}$apiPath" + path
        val conn = connectorConfig.createConnection(new URL(url), httpHeaders, true)
        conn.setRequestProperty("Authorization", "Bearer " + getPartnerConnection.getSessionHeader.getSessionId)
        conn.setRequestProperty("Content-Type", "application/json")
        conn.setRequestMethod("POST")
        conn.setDoOutput(true)
        conn.setDoInput(true)
        conn.connect()
        val wr = new OutputStreamWriter(new GZIPOutputStream(conn.getOutputStream))
        wr.write(jsonBody)
        wr.flush()
        wr.close()
        //conn.setReadTimeout(...)

        //send request
        val responseCode = conn.getResponseCode
        if (200 == responseCode) {
            val in = conn.getInputStream
            val text = conn.getContentEncoding match {
                case "gzip" => io.Source.fromInputStream(new GZIPInputStream(in))("UTF-8").mkString("")
                case _ => io.Source.fromInputStream(in)("UTF-8").mkString("")
            }

            in.close()
            Success(text.parseJson)
        } else {
            if (401 == responseCode) { //Unauthorized
                throw Session.UnauthorizedConnectionException(conn)
            } else {
                logger.error(s"Request Failed - URL=$url")
                logger.error(s"Response Code: $responseCode, Response Message: ${conn.getResponseMessage}")
                val ex = Session.RestCallException(conn)
                logger.error(s"REST Response Code: ${ex.getRestErrorCode}, REST Response Message: ${ex.getRestMessage}")
                Failure(ex)
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
                reset(allowWrite = true)
                //run once again
                codeBlock
            case ex:com.sforce.ws.ConnectionException =>
                //sometimes WSC library returns ConnectionException instead of SoapFaultException when session is invalid
                logger.trace(ex)
                reset(allowWrite = true)
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
                reset(allowWrite = true)
                //run once again
                codeBlock
            case ex: Session.ConnectionException =>
                logger.trace(ex)
                reset(allowWrite = true)
                //run once again
                codeBlock
            case ex:Throwable =>
                throw ex
        }
    }

    def reset(allowWrite: Boolean = false): Unit = {
        remove("session")
        storeSessionData(allowWrite)
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

    def getServerTimestamp: com.sforce.soap.partner.GetServerTimestampResult = {
        withRetry {
            getPartnerConnection.getServerTimestamp
        }.asInstanceOf[com.sforce.soap.partner.GetServerTimestampResult]
    }

    def getUserInfo: com.sforce.soap.partner.GetUserInfoResult = {
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

//    def create(objects: Array[com.sforce.soap.partner.sobject.SObject]):Array[com.sforce.soap.partner.SaveResult] = {
//        val saveResult = withRetry {
//            val conn = getPartnerConnection
//            conn.create(objects)
//        }.asInstanceOf[Array[com.sforce.soap.partner.SaveResult]]
//        saveResult
//    }
//    def update(objects: Array[com.sforce.soap.partner.sobject.SObject]):Array[com.sforce.soap.partner.SaveResult] = {
//        val saveResult = withRetry {
//            val conn = getPartnerConnection
//            conn.update(objects)
//        }.asInstanceOf[Array[com.sforce.soap.partner.SaveResult]]
//        saveResult
//    }
//    def delete(ids: Array[String]):Array[com.sforce.soap.partner.DeleteResult] = {
//        val deleteResult = withRetry {
//            val conn = getPartnerConnection
//            conn.delete(ids)
//        }.asInstanceOf[Array[com.sforce.soap.partner.DeleteResult]]
//        deleteResult
//    }
//    def delete(id: String):Array[com.sforce.soap.partner.DeleteResult] = {
//        delete(Array(id))
//    }
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
                              httpHeaders: Map[String, String] = Map.empty): Option[String] = {

        val text = withRetryRest {
            val connectorConfig = getToolingConnection.getConfig
            val javaMapOfHeaders = new java.util.HashMap[String,String]
            httpHeaders.map{
                case (key, value) => javaMapOfHeaders.put(key, value)
            }

            getRestContent(connectorConfig, "/tooling", path, urlParameters, httpHeaders = javaMapOfHeaders) match {
                case Success(responseText) => responseText
                case Failure(ex) =>
                    throw ex
            }
        }.asInstanceOf[String]
        Some(text)
    }
    /**
     * @param path - e.x. /sobjects/ApexLog/id/Body/
     * @param jsonBody - {"key1" : "value", ...}
     * @return
     */
    def postRestContentTooling(path: String, jsonBody: String,
                              httpHeaders: java.util.HashMap[String, String] = new java.util.HashMap[String, String]()): Option[JsValue] = {
        val jsonAst = withRetryRest {
            val connectorConfig = getToolingConnection.getConfig
            postRestContent(connectorConfig, "/tooling", path, jsonBody, httpHeaders) match {
                case Success(responseText) => responseText
                case Failure(ex) => throw ex
            }
        }.asInstanceOf[JsValue]
        Some(jsonAst)
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

    def upsertTooling(fieldName: String, objects: Array[com.sforce.soap.tooling.SObject]):Array[com.sforce.soap.tooling.UpsertResult] = {
        val saveResult = withRetry {
            val conn = getToolingConnection
            conn.upsert(fieldName, objects)
        }.asInstanceOf[Array[com.sforce.soap.tooling.UpsertResult]]
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
        //var log = ""
        val runTestsResult = withRetry {
            val conn = getToolingConnection
            val res = conn.runTests(runTestsRequest)
            //API v36 - conn.getDebuggingInfo is always returned as null, regardless of  connection.__setDebuggingHeader settings
            //log = if (null != conn.getDebuggingInfo) conn.getDebuggingInfo.getDebugLog else ""
            res

        }.asInstanceOf[com.sforce.soap.tooling.RunTestsResult]
        runTestsResult
    }

//    def runTestsAsyncTooling(classIds: List[String]):(String) = {
//        val runTestsResult = withRetry {
//            val conn = getToolingConnection
//            val res = conn.runTestsAsynchronous(classIds.mkString(","))
//            res
//
//        }.asInstanceOf[String]
//        runTestsResult
//    }
//
    def runTestsAsyncTooling(classIds: String, testSuiteIds: String, maxFailedTests: Int):(String) = {
        val runTestsResult = withRetry {
            val conn = getToolingConnection
            val res = conn.runTestsAsynchronous(classIds, testSuiteIds,  maxFailedTests)
            res

        }.asInstanceOf[String]
        runTestsResult
    }

    ////////////////////////////////////////////////////////////////////////////////////
    private val ONE_SECOND = 1000
    private val MAX_NUM_POLL_REQUESTS = config.getProperty("maxPollRequests").getOrElse[String]("100").toInt

    private def waitRetrieve(connection: MetadataConnection, asyncResult: AsyncResult): RetrieveResult = {
        val waitTimeMilliSecs = config.getProperty("pollWaitMillis").getOrElse("" + (ONE_SECOND * 5)).toLong
        var attempts = 0
        var lastReportTime = System.currentTimeMillis()

        var retrieveResult: com.sforce.soap.metadata.RetrieveResult = null
        do {
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
            retrieveResult =  connection.checkRetrieveStatus(asyncResult.getId, false)
            if (!retrieveResult.isDone && ((attempts +1) > MAX_NUM_POLL_REQUESTS)) {
                throw new Exception("Request timed out.  If this is a large set " +
                    "of metadata components, check that the time allowed " +
                    "by --maxPollRequests is sufficient and --pollWaitMillis is not too short.")
            }

            logger.debug("Status is: " + retrieveResult.getStatus.toString)

        } while (!retrieveResult.isDone)

        if (!retrieveResult.isSuccess) {
            throw new Exception(retrieveResult.getStatus + " msg:" + retrieveResult.getMessages.mkString("\n"))
        } else {
            //finally retrieve ZIP
            Logging.repeatingInfo(logger,
                { retrieveResult =  connection.checkRetrieveStatus(asyncResult.getId, true) },
                "retrieving Metadata ZIP ...", scala.Console.out )(TcpServer.system.scheduler)
            //retrieveResult =  connection.checkRetrieveStatus(asyncResult.getId, true)
        }
        retrieveResult
    }

    private def waitDeploy(connection: MetadataConnection, asyncResult: AsyncResult): DeployResult = {
        val waitTimeMilliSecs = config.getProperty("pollWaitMillis").getOrElse("" + (ONE_SECOND * 5)).toLong
        var attempts = 0

        var lastReportTime = System.currentTimeMillis()
        val oldMessages = scala.collection.mutable.Set[String]()

        var deployResult:com.sforce.soap.metadata.DeployResult = null
        var fetchDetails = false

        do {
            blocking {
                Thread.sleep(waitTimeMilliSecs)
            }
            val reportAttempt = (System.currentTimeMillis() - lastReportTime) > (ONE_SECOND * 3)
            //report only once every 3 seconds
            if (reportAttempt) {
                logger.info("waiting result, poll #" + attempts)
                lastReportTime = System.currentTimeMillis()
            } else {
                logger.trace("waiting result, poll #" + attempts)
            }
            attempts += 1
            fetchDetails = attempts % 3 == 0
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
        } while (!deployResult.isDone)

        if (!fetchDetails) {
            // Get the final result with details if we didn't do it in the last attempt.
            deployResult =  connection.checkDeployStatus(asyncResult.getId, true)
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
