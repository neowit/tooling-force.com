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

package com.neowit.apex.tooling

import com.sforce.soap.tooling.{SObject, MetadataContainer}

/**
 * User: andrey
 * Date: 24/09/2013
 */
trait ActionHandler {
    def act(sfdcSession: SfdcSession, sessionData: SessionData)

}
object ActionHandler {
    def getHandler(appConfig: Config) = {
        appConfig.getProperty("action") match {
          case Some("refresh") => new RefreshHandler(appConfig)
          case Some("save") => new SaveHandler(appConfig)
          case _ =>
              appConfig.help()
              throw new ConfigValueException("No supported operations found")
        }
    }
}

/**
 * refresh project files from SFDC and update sessionData
 */
class RefreshHandler(appConfig: Config) extends ActionHandler {

    def act(sfdcSession: SfdcSession, sessionData: SessionData) {
        //execute Refresh
        for (helper <- TypeHelpers.list) {

            val queryResult = sfdcSession.query(helper.getContentSOQL)
            if (queryResult.getSize >0) {
                do {
                    for (record: SObject <- queryResult.getRecords) {
                        val data = helper.getValueMap(record)
                        //save file content
                        helper.bodyToFile(appConfig, record)
                        //stamp file save time
                        val localMills = System.currentTimeMillis.toString
                        val key = helper.getKey(record)
                        sessionData.setData(key, data ++ Map("LastSyncDateLocal" -> localMills))

                    }
                }  while (!queryResult.isDone)
            }
        }
        val processor = Processor.getProcessor(appConfig)
        processor.deleteMetadataContainer(sfdcSession, sessionData)
        sessionData.store()

    }
}

/**
 * try to save Single or multiple files to SFDC
 */
class SaveHandler(appConfig: Config) extends ActionHandler {
    def act(sfdcSession: SfdcSession, sessionData: SessionData) {
        val processor = Processor.getProcessor(appConfig)
        processor.save(sfdcSession, sessionData)
    }

}
