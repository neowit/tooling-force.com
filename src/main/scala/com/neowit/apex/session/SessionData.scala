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

package com.neowit.apex.session

import scala.util.parsing.json.{JSONArray, JSON, JSONObject}
import scala.Some
import com.neowit.utils.{Config, Logging}
import com.sforce.soap.metadata.{DescribeMetadataObject, DescribeMetadataResult}
import java.io.{PrintWriter, File}
import com.neowit.apex.metadata.{MetaXml, DescribeTask}
import com.sforce.soap.tooling.MetadataContainer
import com.sforce.soap.partner.sobject.SObject
import com.sforce.soap.partner.fault.InvalidSObjectFault

class SessionData (appConfig: Config, sfdcSession: SfdcSession)  extends Logging {

    type Data = Map[String, String]
    private lazy val sessionProperties = {
        appConfig.lastSessionProps
    }
    /**
     * convert key/value pairs into JSON and save in session file
     * @param data - map of values to save
     */
    def setData(key: String, data: Data) {
        sessionProperties.setProperty(key, JSONObject(data).toString())
    }
    /**
     * retrieve key/value pairs for given object name from session file
     * @return - map of values restored from session file
     */
    def getData(key: String): Data = {
        sessionProperties.getPropertyOption(key) match {
            case Some(x) => valueToDataMap(x)
            case None => Map()
        }
    }


    /**
     * getKeyByValue is the opposite of getData, i.e. returns first key which contains specified data in the specified field
     */
    def getKeyByValue(fName: String, fVal: String):Option[String] = {
        //find key of the element which contains given Id
        sessionProperties.keySet().toArray.find(key => Some(fVal) == getData(key.asInstanceOf[String]).get(fName)) match {
          case Some(x) => Option(x.asInstanceOf[String])
          case None => None
        }
    }
    private def valueToDataMap(s: String):Data = {
        JSON.parseFull(s) match {
            case Some(m)  => m.asInstanceOf[Data]
            case _ => Map()
        }
    }

    def getKeyById(id: String):Option[String] = {
        getKeyByValue("Id", id)
    }
    def setField(key: String, propName:String, value: String) {
        val x = getData(key)
        setData(key, x ++ Map(propName -> value))
    }

    /**
     * remove specified field from data map
     * @param propName - name of the field to be removed
     */
    def clearField(key: String, propName:String) {
        val data = getData(key)
        setData(key, data - "Id")
    }
    def remove(key: String) {
        sessionProperties.remove(key)
    }

    def getField(key: String, propName: String): Option[String] = {
        getData(key).get(propName)
    }

    def store() {
        appConfig.storeSessionProps()
    }

    private def getMetadataDescribeFile = {
        new File(appConfig.metaFolder, "describeMetadata-result.js")
    }

    def getNotNull(str: String) = if (null == str) "" else str

    def getNotNull(str: Array[String]) = if (null == str) List() else str

    def saveMetadataDescription[A](describeResult: DescribeMetadataResult) {

        logger.debug(describeResult.toString)
        val writer = new PrintWriter(getMetadataDescribeFile)
        try {
            for (describeObjectResult <- describeResult.getMetadataObjects) {

                logger.debug("XMLName=" + describeObjectResult.getXmlName)
                logger.debug("")
                val json = JSONObject(Map(
                    "XMLName" -> getNotNull(describeObjectResult.getXmlName),
                    "DirName" -> getNotNull(describeObjectResult.getDirectoryName),
                    "Suffix" -> getNotNull(describeObjectResult.getSuffix),
                    "HasMetaFile" -> describeObjectResult.isMetaFile,
                    "InFolder" -> describeObjectResult.isInFolder,
                    "ChildObjects" -> JSONArray(
                                        if (describeObjectResult.getChildXmlNames.isEmpty || List(null) == describeObjectResult.getChildXmlNames.toList)
                                        List()
                                        else describeObjectResult.getChildXmlNames.toList.filter(null != _))
                ))
                //writer.println(json.toString())
                //JSONFormat
                writer.println(json.toString())
            }
        } catch {
            case ex:Throwable =>
                ex.printStackTrace()
                //logger.debug(ex.getStackTrace)

        } finally {
            writer.close()
        }
    }

    private val metadataDescription = collection.mutable.HashMap[String, com.sforce.soap.metadata.DescribeMetadataObject]()

    def getMetadataDescription: Map[String, com.sforce.soap.metadata.DescribeMetadataObject] = {
        if (metadataDescription.isEmpty) {
            //load
            loadMetadataDescription()
        }
        metadataDescription.toMap
    }

    private def loadMetadataDescription() = {

        //try local file
        val f = getMetadataDescribeFile
        if (f.exists() && f.canRead && f.length() > 1) {
            //use locally stored data
            parseMetadataDescription(f, metadataDescription)
        } else {
            //load Org metadata and store in session locally
            new DescribeTask(sfdcSession).run(saveMetadataDescription[DescribeMetadataResult])
            val f = getMetadataDescribeFile
            parseMetadataDescription(f, metadataDescription)

        }

    }

    private def parseMetadataDescription(describeMetadataResultJs: File,
                                         storeResultTo: collection.mutable.HashMap[String, com.sforce.soap.metadata.DescribeMetadataObject] ) {

        class CC[T] { def unapply(a:Any):Option[T] = Some(a.asInstanceOf[T]) }
        object M extends CC[Map[String, Any]]
        object L extends CC[List[String]]
        object S extends CC[String]
        //object D extends CC[Double]
        object B extends CC[Boolean]
        //use locally stored data

        for( jsonString <- io.Source.fromFile(describeMetadataResultJs).getLines()) {
            val dmo = new DescribeMetadataObject()
            for {
                Some(M(map)) <- List(JSON.parseFull(jsonString))
                S(xmlName) = map("XMLName")
                S(dirName) = map("DirName")
                S(suffix) = map("Suffix")
                B(isMeta) = map("HasMetaFile")
                B(isInFolder) = map("InFolder")
                L(childObjects) = map("ChildObjects")
            } yield {
                dmo.setXmlName(xmlName)
                dmo.setDirectoryName(dirName)
                dmo.setSuffix(suffix)
                dmo.setMetaFile(isMeta)
                dmo.setInFolder(isInFolder)
                dmo.setChildXmlNames(childObjects.toArray[String])
                dmo
            }
            storeResultTo += dmo.getXmlName -> dmo
        }
    }

    def load() {
        //first check if we have a cached version
        val loadFromSFDC = sessionProperties.getPropertyOption("serviceEndpoint") match {
            case Some(x) => true
            case None => true
        }

        if (loadFromSFDC) {
            //init Last Modified Dates of all file types specified in package.xml
            //it seems that all types that have -meta.xml can be also queried
            val queriableTypes = getMetadataDescription.filter(_._2.isMetaFile).keySet
            val metaXml = new MetaXml(sfdcSession)
            for (mType <- metaXml.listMetadataTypes() if queriableTypes.contains(mType.xmlName)   ) {
                val typeName = mType.xmlName
                try {
                val queryResult = sfdcSession.getPartnerConnection.query("select " + mType.getQueriableFields.mkString(", ") + " from " + typeName)
                if (queryResult.getSize >0) {
                    do {
                        for (record: SObject <- queryResult.getRecords) {
                            val data = mType.getValueMap(record)
                            setData(mType.getKey(record), data)
                        }
                    }  while (!queryResult.isDone)
                }
                } catch {
                    case ex: InvalidSObjectFault => logger.debug(ex.getMessage + " SKIP.")
                }

            }
            store()
        }
    }

    def getExistingContainer: Option[MetadataContainer] = {

        if (None != getField("MetadataContainer", "Id")) {
            //check if we have cached Container data
            val containerData = getData("MetadataContainer")
            val container = new MetadataContainer
            container.setId(containerData("Id"))
            container.setName(containerData("Name"))
            Some(container)
        } else None

    }

}
