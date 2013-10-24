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

import com.sforce.soap.tooling._
import scala.Some

/**
 * User: andrey
 * Date: 13/09/2013
 * wrapper around SoapConnection which can recover when selected operations are run with invalid and expired SFDC Session
 */
class SfdcSession (appConfig: Config) extends Logging{
    private var connectionTooling:ToolingConnection = null
    private var connectionPartner:PartnerConnection = null

    def getPartnerConnection = {
        if (null != connectionPartner)
            connectionPartner
        else
            connectionPartner = new PartnerConnection(this)

        connectionPartner
    }
    def getToolingConnection = {
        if (null != connectionTooling)
            connectionTooling
        else
            connectionTooling = new ToolingConnection(this)

        connectionTooling
    }

    private lazy val sessionProperties = {
        appConfig.lastSessionProps
    }
    def getSavedSessionData = {
        (sessionProperties.getPropertyOption("sessionId"), sessionProperties.getPropertyOption("serviceEndpoint"))
    }
    def storeSessionData() {
        appConfig.storeSessionProps()
    }
    def setProperty(key: String, value: String) {
        sessionProperties.setProperty(key, value)
    }
    def getProperty(key: String): Option[String] = {
        sessionProperties.getPropertyOption(key)
    }

    def reset() {
        sessionProperties.remove("sessionId")
        sessionProperties.remove("serviceEndpoint")
        storeSessionData()
        connectionTooling = null
        connectionPartner = null
    }

    def updateConnectionData(config: com.sforce.ws.ConnectorConfig) {
        sessionProperties.setProperty("sessionId", config.getSessionId)
        sessionProperties.setProperty("serviceEndpoint", config.getServiceEndpoint)
        appConfig.storeSessionProps()
    }
    def getConfig: Config = appConfig

}

trait GenericConnection extends Logging{
    
    def getSession: SfdcSession
    
    def getConnectionConfig (appConfig: Config) = {
        val config = new com.sforce.ws.ConnectorConfig()
        config.setUsername(appConfig.username)
        config.setPassword(appConfig.password)

        val endpoint = appConfig.soapEndpoint
        if (null != endpoint)
            config.setAuthEndpoint(endpoint)
        config.setServiceEndpoint(endpoint)

        config.setCompression(true)

        val proxyHost = appConfig.getProperty("http.proxyHost")
        val proxyPort = appConfig.getProperty("http.proxyPort")
        if (None != proxyHost && None != proxyPort)
            config.setProxy(proxyHost.get, proxyPort.get.toInt)

        val proxyUsername = appConfig.getProperty("http.proxyUsername")
        if (None != proxyUsername )
            config.setProxyUsername(proxyUsername.get)

        val proxyPassword = appConfig.getProperty("http.proxyPassword")
        if (None != proxyPassword )
            config.setProxyPassword(proxyPassword.get)

        val ntlmDomain = appConfig.getProperty("http.ntlmDomain")
        if (None != ntlmDomain )
            config.setNtlmDomain(ntlmDomain.get)

        val connectionTimeoutSecs = appConfig.getProperty("http.connectionTimeoutSecs")
        if (None != connectionTimeoutSecs )
            config.setConnectionTimeout(connectionTimeoutSecs.get.toInt * 1000)

        val readTimeoutSecs = appConfig.getProperty("http.readTimeoutSecs")
        if (None != readTimeoutSecs )
            config.setReadTimeout(readTimeoutSecs.get.toInt * 1000)

        config
    }
    /**
     * clear session data and force login next time
     */

    def withRetry(codeBlock: => Any) = {
        try {
            codeBlock
        } catch {
            case ex:ApiFault if ExceptionCode.INVALID_SESSION_ID == ex.getExceptionCode =>
                logger.debug("Session is invalid or has expired. Will run the process again with brand new connection. ")
                logger.trace(ex)
                getSession.reset()
                //run once again
                codeBlock
            case ex:Throwable =>
                throw ex
        }
    }

}

class PartnerConnection(session: SfdcSession) extends GenericConnection {
    def getSession: SfdcSession = session

    lazy val connection = {

        val connectionConfig = getConnectionConfig(session.getConfig)
        val conn = session.getSavedSessionData match {
            case (Some(sessionId), Some(serviceEndpoint)) =>
                //use cached data
                connectionConfig.setSessionId(sessionId)
                connectionConfig.setServiceEndpoint(serviceEndpoint)
                //connection = com.sforce.soap.partner.Connector.newConnection(config)
                com.sforce.soap.partner.Connector.newConnection(connectionConfig)
            case _ =>
                //login explicitly
                com.sforce.soap.partner.Connector.newConnection(connectionConfig)
            //config.setServiceEndpoint(config.getServiceEndpoint.replace("/services/Soap/u/", "/services/Soap/T/"))
        }
        session.updateConnectionData(connectionConfig)

        logger.info("##### Partner Connection #####")
        logger.info("Auth EndPoint: "+connectionConfig.getAuthEndpoint)
        logger.info("Service EndPoint: "+connectionConfig.getServiceEndpoint)
        logger.info("Username: "+connectionConfig.getUsername)
        //logger.info("SessionId: "+config.getSessionId)
        logger.debug("SessionId: "+connectionConfig.getSessionId)
        conn
    }


    def getServerTimestamp = {
        withRetry {
            connection.getServerTimestamp
        }.asInstanceOf[GetServerTimestampResult]
    }
    def query(soql: String):QueryResult = {
        withRetry {
            connection.query(soql)
        }.asInstanceOf[QueryResult]
    }
    def create(objects: Array[com.sforce.soap.partner.sobject.SObject]):Array[SaveResult] = {
        withRetry {
            //sort objects to avoid
            // System.TypeException: Cannot have more than 10 chunks in a single operation. Please rearrange the data to reduce chunking.
            val sortedByTypeObjects = objects.sortBy(obj => obj.getClass.getName)
            connection.create(sortedByTypeObjects)
        }.asInstanceOf[Array[SaveResult]]
    }
    def update(objects: Array[com.sforce.soap.partner.sobject.SObject]):Array[SaveResult] = {
        withRetry {
            connection.update(objects)
        }.asInstanceOf[Array[SaveResult]]
    }

    def delete(ids: Array[String]):Array[DeleteResult] = {
        withRetry {
            connection.delete(ids)
        }.asInstanceOf[Array[DeleteResult]]
    }
    def delete(id:String):DeleteResult = {
        withRetry {
            connection.delete(Array(id))
        }.asInstanceOf[Array[DeleteResult]].head
    }

}


class ToolingConnection(session: SfdcSession) extends GenericConnection {
    def getSession: SfdcSession = session

    lazy val connection = {

        val partnerConnectionConfig = session.getPartnerConnection.connection.getConfig

        //Tooling api can not connect on its own (something is wrong with the jar which wsc generates)
        //having to use a workaround - connect via SOAP and then use obtained session for tooling
        //val soapConnection = com.sforce.soap.partner.Connector.newConnection(config)
        val connectionConfig = getConnectionConfig(session.getConfig)
        connectionConfig.setSessionId(partnerConnectionConfig.getSessionId)
        connectionConfig.setServiceEndpoint(partnerConnectionConfig.getServiceEndpoint.replace("/services/Soap/u/", "/services/Soap/T/"))

        val toolingConnection = com.sforce.soap.tooling.Connector.newConnection(connectionConfig)
        //tooling api uses different service endpoint
        logger.info("##### Tooling Connection #####")
        logger.info("Auth EndPoint: "+connectionConfig.getAuthEndpoint)
        logger.info("Service EndPoint: "+connectionConfig.getServiceEndpoint)
        logger.info("Username: "+connectionConfig.getUsername)
        //logger.info("SessionId: "+config.getSessionId)
        logger.debug("SessionId: "+connectionConfig.getSessionId)
        toolingConnection
    }

    def query(soql: String):QueryResult = {
        withRetry {
            connection.query(soql)
        }.asInstanceOf[QueryResult]
    }
    def create(objects: Array[com.sforce.soap.tooling.SObject]):Array[SaveResult] = {
        withRetry {
            //sort objects to avoid
            // System.TypeException: Cannot have more than 10 chunks in a single operation. Please rearrange the data to reduce chunking.
            val sortedByTypeObjects = objects.sortBy(obj => obj.getClass.getName)
            connection.create(sortedByTypeObjects)
        }.asInstanceOf[Array[SaveResult]]
    }
    def update(objects: Array[com.sforce.soap.tooling.SObject]):Array[SaveResult] = {
        withRetry {
            connection.update(objects)
        }.asInstanceOf[Array[SaveResult]]
    }

    def delete(ids: Array[String]):Array[DeleteResult] = {
        withRetry {
            connection.delete(ids)
        }.asInstanceOf[Array[DeleteResult]]
    }
    def delete(id:String):DeleteResult = {
        withRetry {
            connection.delete(Array(id))
        }.asInstanceOf[Array[DeleteResult]].head
    }
}
