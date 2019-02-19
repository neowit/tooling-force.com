/*
 * Copyright (c) 2013 Andrey Gavrikov.
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

package com.neowit.utils

import java.util.Properties
import java.io.{File, FileWriter, OutputStream}
import java.nio.file.{Files, Paths}

import com.neowit.auth._
import com.neowit.response.ResponseWriter
import com.neowit.response.protocols.vim.ResponseWriterVim

//import com.typesafe.scalalogging.slf4j.Logging
import scala.collection.mutable.ListBuffer
import scala.annotation.tailrec

import spray.json._

class InvalidCommandLineException(msg: String)  extends IllegalArgumentException(msg: String)
class MissingRequiredConfigParameterException(msg:String) extends IllegalArgumentException(msg: String)

class ConfigValueException(msg:String) extends IllegalArgumentException(msg: String)


class BasicConfig extends Logging {
    private var out: OutputStream = Console.out
    def setOutputStream(out: OutputStream): Unit = { this.out = out}

    private var providedResponseWriter: Option[ResponseWriter] =  None
    private val internalResponseWriter =  new ResponseWriterVim(this.out)
    def setResponseWriter(writer: ResponseWriter): Unit = this.providedResponseWriter = Option(writer)
    lazy val getResponseWriter: ResponseWriter = providedResponseWriter.getOrElse(internalResponseWriter)

    def isUnix: Boolean = {
        val os = System.getProperty("os.name").toLowerCase
        os.contains("nux") || os.contains("mac")
    }

    type OptionMap = Map[String, String]
    private val mainProps = new Properties() with OptionProperties

    private var options:OptionMap = Map()

    def load(arglist: List[String]): Unit = {
        if (arglist.isEmpty) {
            throw new InvalidCommandLineException("Empty command line")
        }

        /**
         * @param pair: key="value"
         * @return (key, value)
         */
        def splitParam(pair: String):(String, String) = {
            if (pair.indexOf('=') < 0)
                throw new InvalidCommandLineException("Did not understand " + pair)

            val key = pair.takeWhile(ch => ch != '=')
            val value = pair.substring(key.length+1)
            if ("" == value) {
                throw new InvalidCommandLineException("Did not understand " + pair)
            }
            //some parameters may be enclosed in single or double quote-s, let's remove them
            val patternStartWithQuote = """^["|']""".r
            val patternEndsWithQuote = """["|']$""".r
            val cleanValue = patternEndsWithQuote.replaceFirstIn(patternStartWithQuote.replaceFirstIn(value, ""), "")

            (key, cleanValue)
        }

        @tailrec
        def nextOption(configFilePaths: ListBuffer[String], map: OptionMap, list: List[String]): (List[String], OptionMap) = {
            list match {
                case Nil => (configFilePaths.toList, map)
                case keyValuePair :: tail if keyValuePair.startsWith("--") =>
                    val (key, value) = splitParam(keyValuePair.drop(2))
                    key match {
                        case "config" => nextOption(configFilePaths += value, map, tail)
                        case "action" => nextOption(configFilePaths, map ++ Map("action" -> value), tail)
                        case _ => nextOption(configFilePaths, map ++ Map(key -> value), tail)
                    }
                case x =>
                    val msg = "failed to parse command line param: " + x
                    logger.debug(msg)
                    throw new InvalidCommandLineException(msg)
            }
        }

        logger.trace("arglist=" + arglist)
        val (configFilePaths:List[String], opts) = nextOption(ListBuffer[String](), Map(), arglist)
        options = opts
        //logger.debug(options)
        //merge config files
        if (configFilePaths.nonEmpty) {
            //require(!configFilePaths.isEmpty, "missing --config parameter")
            for (confPath <- configFilePaths) {
                val conf = new Properties()
                conf.load(FileUtils.readFile(confPath).bufferedReader())

                val keys = conf.keySet().iterator()
                while (keys.hasNext) {
                    val key = keys.next.toString
                    val value = conf.getProperty(key, "")
                    if ("" != value) {
                        //overwrite existing value
                        mainProps.setProperty(key, value)
                    }
                }
            }
        }

        responseFile match {
            case Some(f) =>
                setResponseWriter(new ResponseWriterVim(f))
            case None =>
        }
        //lastRunOutputFile
    }

    /**
     * can be used to add additional property in the middle of processing a command
     */
    def setProperty(key: String, value: String): Unit = {
        options += key -> value
    }

    /**
     * order of obtaining parameter value is as follows
     * 1. check java -Dkey=... command line parameter
     * 2. check application command line parameter: java .... tooling-force.com.jar --key=...
     * 3. check .properties file
     *
     * @param key name of property/parameter
     */
    def getProperty(key:String):Option[String] = {
        val cmdLineValue = scala.util.Properties.propOrNone( key ) match {
            case Some(s) => Some(s)
            case None => options.get(key)
        }
        val configValue = mainProps.getPropertyOption(key)
        val res = cmdLineValue match {
            case None => configValue match {
                case None => None
                case _ => configValue
            }
            case _ => cmdLineValue
        }
        res
    }
    def getRequiredProperty(key: String): String = {
        getProperty(key) match {
            case Some(s) if !s.isEmpty => s
            case _ =>  throw new MissingRequiredConfigParameterException(key +" is required")
        }
    }

    def help(): Unit = {
        println( """
 Command line utility for working with force.com Metadata and Tooling API.
 https://github.com/neowit/tooling-force.com

Command line parameters
 --help : show this text

 --action=<action-name> - action to perform
           run --help=<action-name> to display help for specific action

 --config="path to config.properties"
 [[--config="path to config.properties"]: (optional) more than one "--config" is supported, non blank parameters of later --config take precendence
 [--<any param from config file>=<value>]: (optional) all config parameters can be specified in both config file and command line.
                                                      Command line parameters take precedence
Example:
 java -jar "/path/to/tooling-force.com-0.1.jar" --action=refresh --config=/path/to/myconf.properties

OR if sfdc login/pass are in a different file
 java -jar "/path/to/tooling-force.com-0.1.jar" --action=refresh --config=/path/to/myconf.properties --config=/path/to/credentials.properties


In the following example username user@domain.com specified in the command line will be used,
regardless of whether it is also specified in config file or not
 java -jar "/path/to/tooling-force.com-0.1.jar" --action=refresh --config=/path/to/myconf.properties --sf.username=user@domain.com

                 """)
    }

    lazy val action: String = getRequiredProperty("action")

    private lazy val responseFile: Option[File] = {
        getProperty("responseFilePath") match {
            case Some(path) =>
                val f = new File(path)
                if (!f.exists()) {
                    // make sure path exists
                    f.getParentFile.mkdirs()
                    // create actual file
                    f.createNewFile()
                }
                Option(f)
            case None =>
                None
        }
    }

}

class Config(val basicConfig: BasicConfig) extends OAuth2JsonSupport with Logging{

    val apiVersion:Double = 45.0

    //make BasicConfig methods available in Config
    def load(arglist: List[String]): Unit = {
        basicConfig.load(arglist)
        //basicConfig.setResponseWriter(responseWriter)
    }
    def isUnix: Boolean = basicConfig.isUnix
    def getProperty(key:String):Option[String] = basicConfig.getProperty(key)
    def getRequiredProperty(key: String): String =  basicConfig.getRequiredProperty(key)
    def getRequiredPropertyOpt(key: String): Option[String] =  Option(basicConfig.getRequiredProperty(key))
    lazy val action: String = basicConfig.action
    //END BasicConfig methods

    //tempFolderPath - optional - if specified then use this folder instead of system generated
    lazy val tempFolderPath: Option[String] = getProperty("tempFolderPath")


    /*
    @deprecated("use debuggingHeaderConfig related logic", "0.3.6.0")
    lazy val logLevel = getProperty("logLevel") match {
        case Some(x) => Set("None", "Debugonly", "Db", "Profiling", "Callout", "Detail").contains(x)
            x
        case None => "None"
    }
    */


    //lazy val responseWriter = new ResponseWriter(responseFile)

}


class ConfigWithSfdcProject(override val basicConfig: BasicConfig) extends Config(basicConfig) {

    protected def projectPathOpt: Option[String] = getProperty("projectPath")
    def projectDirOpt: Option[File] = projectPathOpt.map(new File(_))
    /* path to src folder */
    //lazy val srcPathOpt: Option[String] = srcDirOpt.map(_.getAbsolutePath)
    lazy val srcDirOpt: Option[File] = {
        projectPathOpt match {
            case Some(_projectPath) =>
                val fSrc = new File(_projectPath, "src")
                if (!fSrc.isDirectory || !fSrc.canRead) {
                    throw new ConfigValueException("failed to detect 'src' folder in path:" + _projectPath)
                }
                Option(fSrc)
            case None => None
        }

    }

    def getAuthConfig: Option[AuthCredentials] = {
        getProperty("authConfigPath") match {
            case Some(authConfigPath) =>
                // user provided explicit auth config
                val path = Paths.get(authConfigPath)
                if (!Files.isReadable(path)) {
                    throw new ConfigValueException("Specified 'authConfigPath' does not point to readable file: " + authConfigPath)
                }
                AuthCredentials.load(path) match {
                    case Some(credentials) => Option(credentials)
                    case None =>
                        throw new ConfigValueException("Specified 'authConfigPath' does not point to supported auth type: " + authConfigPath)
                }
            case None =>
                // try to load login/pass/server from main configuration
                getProperty("sf.username").flatMap{ username =>
                    getProperty("sf.password").flatMap{ password =>
                        getProperty("sf.serverurl").map{serverUrl =>
                            LoginPasswordCredentials(username, password, serverUrl)
                        }
                    }
                }
        }
    }

    def saveAccessToken(tokens: Oauth2Tokens): Unit = {
        getProperty("authConfigPath") match {
            case Some(authConfigPath) =>
                // user provided explicit auth config
                val path = Paths.get(authConfigPath)
                if (!Files.isWritable(path)) {
                    throw new ConfigValueException("Specified 'authConfigPath' does not point to readable file: " + authConfigPath)
                }
                getAuthConfig match {
                    case Some(credentials @ Oauth2Credentials(_)) =>
                        val updatedTokens = credentials.tokens.copy(access_token = tokens.access_token)
                        FileUtils.writeFile(updatedTokens.toJson.prettyPrint, path.toFile)
                    case _ =>
                    //not oauth, do nothing
                }
            case None =>
                // do nothing
        }

    }

    /*
     * generates specified folders nested in the main outputFolder
     */
    /*
    def mkdirs(dirName: String): String = {
        srcDirOpt.map{srcDir =>
            val path = srcDir + File.separator + dirName
            //check that folder exists
            val f = new File(path)
            if (!f.isDirectory) {
                if (!f.mkdirs())
                    throw new RuntimeException("Failed to create folder: " + path)
            }
            path
        }
    }
    */

    lazy val debuggingHeaderConfigPath: Option[String] = getProperty("debuggingHeaderConfig") match {
        case Some(path) => Option(path)
        case None => None
    }

    def getLogFile: File = {
        getProperty("logFile") match {
            case Some(logPath) =>
                new File(logPath)
            case None => FileUtils.createTempFile("apex-", ".log")
        }

    }
}

class ConfigWithReadOnlySession (override val basicConfig: BasicConfig) extends ConfigWithSfdcProject(basicConfig) {

    lazy val isCheckOnly: Boolean = getProperty("checkOnly").contains("true")

    /**
      * by default CRC32 hash is used to detect file changes
      * but command line option --preferMD5=true can force MD5
      */
    lazy val useMD5Hash: Boolean = getProperty("preferMD5").contains("true")

    //path to folder where all cached metadata (session Id, last update dates, etc) stored
    lazy val sessionFolderOpt: Option[File] = {
        projectPathOpt match {
            case Some(_projectPath) =>
                val path = getProperty("sessionFolderPath").getOrElse[String](new File(_projectPath, ".vim-force.com").getAbsolutePath)
                val dir = new File(path)
                if (!dir.exists()) {
                    if (!dir.mkdirs())
                        throw new IllegalArgumentException("Failed to create folder: " + dir.getAbsolutePath + " for sessionFolderPath")
                }
                Option(dir)
            case None => None
        }
    }
    lazy val lastSessionPropsOpt: Option[JsonProperties] = {
        sessionFolderOpt.map{ sessionFolder =>
            val file = new File(sessionFolder, "session.properties")
            if (!file.exists) {
                file.createNewFile()
            }
            val props = new Properties() with JsonProperties
            props.load(FileUtils.readFile(file).bufferedReader())
            props
        }
    }
}

class ConfigWithSession(override val basicConfig: BasicConfig) extends ConfigWithReadOnlySession(basicConfig) {
    def storeSessionProps(): Unit = {
        for {
            sessionFolder <- sessionFolderOpt
            lastSessionProps <- lastSessionPropsOpt
        } yield {
            val writer = new FileWriter(new File(sessionFolder, "session.properties"))
            lastSessionProps.store(writer, "Session data\nThis is automatically generated file. Any manual changes may be overwritten.")
            writer.close()
            true
        }
    }
}
