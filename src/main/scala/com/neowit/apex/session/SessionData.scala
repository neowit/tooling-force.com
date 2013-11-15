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

import scala.util.parsing.json.{JSON, JSONObject}
import com.sforce.soap.tooling._
import scala.Some
import com.neowit.utils.{Config, Logging}
import com.neowit.apex.tooling.{TypeHelpers}

/**
 * User: andrey
 * Date: 16/09/2013
 */
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
     * @param fName
     * @param fVal
     * @return
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


    def load() {
        //first check if we have a cached version
        val loadFromSFDC = sessionProperties.getPropertyOption("serviceEndpoint") match {
            case Some(x) => false
            case None => true
        }

        if (loadFromSFDC) {

            for (helper <- TypeHelpers.list) {

                val typeName = helper.typeName
                val queryResult = sfdcSession.getToolingConnection.query("select Id, Name, ApiVersion, LastModifiedDate from " + typeName)
                if (queryResult.getSize >0) {
                    do {
                        for (record: SObject <- queryResult.getRecords) {
                            val data = helper.getValueMap(record)
                            setData(helper.getKey(record), data)
                        }
                    }  while (!queryResult.isDone)
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
