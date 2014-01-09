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
import java.io.{PrintWriter, FileWriter, File}

//import com.typesafe.scalalogging.slf4j.Logging
import scala.collection.mutable.ListBuffer
import scala.annotation.tailrec

class InvalidCommandLineException(msg: String)  extends IllegalArgumentException(msg: String) {
    def this() {
        this(null)
    }
}
class MissingRequiredConfigParameterException(msg:String) extends IllegalArgumentException(msg: String)

class ConfigValueException(msg:String) extends IllegalArgumentException(msg: String)


object Config extends Logging {
    private var config: Config = new Config
    def getConfig = {
        config
    }
    def resetConfig = {
        //used in unit tests
        config = new Config
        config
    }
}

class Config extends Logging{
    val apiVersion:Double = 29.0
    type OptionMap = Map[String, String]
    private val mainProps = new Properties() with PropertiesOption

    private var options:OptionMap = Map()

    def load(args: Array[String]) {
        load(args.toList)
    }
    def load(arglist: List[String]) {
        if (arglist.isEmpty) {
            throw new InvalidCommandLineException
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
                case _ =>
                    throw new InvalidCommandLineException
            }
        }

        val (configFilePaths:List[String], opts) = nextOption(ListBuffer[String](), Map(), arglist)
        options = opts
        //logger.debug(options)
        //merge config files
        if (!configFilePaths.isEmpty) {
            //require(!configFilePaths.isEmpty, "missing --config parameter")
            for (confPath <- configFilePaths) {
                val conf = new Properties()
                conf.load(scala.io.Source.fromFile(confPath.toString).bufferedReader())

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

        //lastRunOutputFile
    }
    def getProperty(key:String):Option[String] = getProperty(key, None)

    def getProperty(key:String, defaultVal: Option[String]):Option[String] = {
        val cmdLineValue = options.get(key)
        val configValue = mainProps.getPropertyOption(key)
        val res = cmdLineValue match {
            case None => configValue match {
                case None => defaultVal
                case _ => configValue
            }
            case _ => cmdLineValue
        }
        res
    }
    def getRequiredProperty(key: String): Option[String] = {
        getProperty(key) match {
            case Some(s) if !s.isEmpty => Some(s)
            case _ =>  throw new MissingRequiredConfigParameterException(key +" is required")
        }
    }

    lazy val username = getRequiredProperty("sf.username").get
    lazy val password = getRequiredProperty("sf.password").get
    lazy val soapEndpoint = {
        val serverUrl = getRequiredProperty("sf.serverurl")
        serverUrl match {
            case Some(x) => x + "/services/Soap/u/" + apiVersion
            case None => null
        }
    }

    lazy val action = getRequiredProperty("action").get

    //path to folder where all cached metadata (session Id, las update dates, etc) stored
    lazy val sessionFolder = {
        val path = getRequiredProperty("sessionFolderPath").get
        val dir = new File(path)
        if (!dir.exists()) {
            if (!dir.mkdirs())
                throw new IllegalArgumentException("Failed to create folder: " + dir.getAbsolutePath + " for sessionFolderPath")
        }
        dir
    }
    lazy val lastSessionProps: PropertiesOption = {
        val file = new File(sessionFolder, "session.properties")
        if (!file.exists) {
            file.createNewFile()
        }
        val props = new Properties() with PropertiesOption
        props.load(scala.io.Source.fromFile(file).bufferedReader())
        props
    }
    def storeSessionProps() {
        val writer = new FileWriter(new File(sessionFolder, "session.properties"))
        lastSessionProps.store(writer, "Session data\nThis is automatically generated file. Any manual changes may be overwritten.")
    }


    /**
     * workPath can point to two things
     * either: full path to file (Class, Page, Component)
     * or: full path to the .../src/ folder which contains items to be deployed
     * @return
     */
    lazy val resourcePath = getRequiredProperty("resourcePath").get
    /* path to src folder */
    lazy val srcPath = srcDir.getAbsolutePath
    lazy val srcDir = {
        def getSrcFolder(file: File):File = {
            if (null == file) {
                throw new ConfigValueException("failed to detect SRC folder in path:" + resourcePath)
            }
            if (file.isDirectory && "src" == file.getName)
                file
            else {
                //check if current path points to the project folder and "src" is its first child
                val srcPath = file.getAbsolutePath + File.separator + "src"
                val f = new File(resourcePath)
                if (f.isDirectory && f.canRead) {
                    f
                } else {
                    getSrcFolder(file.getParentFile)
                }
            }
        }
        val file = new File(resourcePath)
        getSrcFolder(file)

    }
    //tempFolderPath - optional - if specified then use this folder instead of system generated
    lazy val tempFolderPath =  getProperty("tempFolderPath")
    //destinationFolderPath - required only for some operations
    //  folder where refresh results will be copied to
    lazy val destinationFolderPath = {
        action match {
            case "refresh" =>
                //for refresh destinationFolderPath is required
                getRequiredProperty("destinationFolderPath")
            case _ => getProperty("destinationFolderPath")
        }
    }

    lazy val isCheckOnly = getProperty("checkOnly") match {
      case Some(x) => "true" == x
      case None => false
    }

    def isDirectory(resourcePath: String) = {
        val f = new File(resourcePath)
        f.isDirectory && f.canRead
    }

    def help() {
        println( """
 Command line utility for working with force.com Tooling API.
 https://github.com/neowit/tooling-force.com

Command line parameters"
 --help : show this text
 --config : path to config.properties
 [[--config : path to config.properties]: (optional) more than one "--config" is supported, non blank parameters of later --config take precendence
 [--<any param from config file>]: (optional) all config parameters can be specified in both config file and command line. Command line parameters take precendence

Example:
 java -jar "/path/to/tooling-force.com-0.1.jar" --config /path/to/myconf.properties

OR if sfdc login/pass are in a different file
 java -jar "/path/to/tooling-force.com-0.1.jar" --config /path/to/myconf.properties --config /path/to/credentials.properties


In the following example username user@domain.com specified in the command line will be used,
regardless of whether it is also specified in config file or not
 java -jar "/path/to/tooling-force.com-0.1.jar" --config /path/to/myconf.properties --sf.username user@domain.com
                           """)
    }

    /*
     * generates specified folders nested in the main outputFolder
     */
    def mkdirs(dirName: String) = {
        val path = srcDir + File.separator + dirName
        //check that folder exists
        val f = new File(path)
        if (!f.isDirectory) {
            if (!f.mkdirs())
                throw new RuntimeException("Failed to create folder: " + path)
        }

        path
    }

    private var responseFileAccessed = false
    private lazy val responseFile = {
        val path = getRequiredProperty("responseFilePath").get
        val f = new File(path)
        if (!f.exists()) {
            f.createNewFile()
        }
        responseFileAccessed = true
        f
    }
    lazy val responseWriter: PrintWriter = new PrintWriter(responseFile)
    def responseWriterClose = {
        if (responseFileAccessed) {
            responseWriter.close()
        }
    }
}
