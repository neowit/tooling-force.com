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

import com.neowit.utils.{PropertiesOption, JsonProperties, Logging, Config}
import com.sforce.soap.partner.PartnerConnection
import com.sforce.soap.metadata.{RetrieveRequest, AsyncResult, MetadataConnection}
import java.util.Properties

/**
 * manages local data store related to specific project
 */
object Session {
    def apply(appConfig: Config) = new Session(appConfig)
}

/**
 * Session has following responsibilities
 * 1. Maintains/stores persistent connection and can resue connection
 * 2. Maintains cache of some important information about project metadata (Pages, Classes, etc)
 *
 * @param config - main application config
 */
class Session(config: Config) extends Logging {
    private val sessionProperties = new Properties with PropertiesOption with JsonProperties {
        val jsonProps: PropertiesOption = config.lastSessionProps
    }
    private var connectionPartner:Option[PartnerConnection] = None
    private var connectionMetadata:Option[MetadataConnection] = None



    def storeSessionData() {
        config.storeSessionProps()
    }
    def getSavedConnectionData = {
        (sessionProperties.getString("sessionId"), sessionProperties.getString("serviceEndpoint"))
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
                      Connection.createPartnerConnection(config)
              }

        }
        connectionPartner = Some(conn)
        sessionProperties.setString("sessionId", conn.getConfig.getSessionId)
        sessionProperties.setString("serviceEndpoint", conn.getConfig.getServiceEndpoint)
        storeSessionData()

        conn
    }
    private def getMetadataConnection: MetadataConnection = {
        val conn = connectionMetadata match {
            case Some(connection) => connection
            case None =>
                connectionPartner match {
                  case Some(partnerConnection) =>
                      Connection.getMetadataConnection(config, partnerConnection)
                  case None =>
                      Connection.createMetadataConnection(config)
                }
        }
        connectionMetadata = Some(conn)
        conn
    }

    def withRetry(codeBlock: => Any) = {
        try {
            codeBlock
        } catch {
            case ex:com.sforce.ws.SoapFaultException if "INVALID_SESSION_ID" == ex.getFaultCode.getLocalPart =>
                logger.debug("Session is invalid or has expired. Will run the process again with brand new connection. ")
                logger.trace(ex)
                reset()
                //run once again
                codeBlock
            case ex:Throwable =>
                throw ex
        }
    }
    def reset() {
        sessionProperties.remove("sessionId")
        sessionProperties.remove("serviceEndpoint")
        storeSessionData()
        connectionPartner = None
    }

    def getServerTimestamp = {
        withRetry {
            getPartnerConnection.getServerTimestamp
        }.asInstanceOf[com.sforce.soap.partner.GetServerTimestampResult]
    }

    def retrieve(retrieveRequest: RetrieveRequest ):AsyncResult = {
        withRetry {
            getMetadataConnection.retrieve(retrieveRequest)
        }.asInstanceOf[AsyncResult]
    }


}
