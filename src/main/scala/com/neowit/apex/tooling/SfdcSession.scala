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
    private var connection = getConnection

    private lazy val sessionProperties = {
        appConfig.lastSessionProps
    }
    private def getSavedSessionData = {
        (sessionProperties.getPropertyOption("sessionId"), sessionProperties.getPropertyOption("serviceEndpoint"))
    }
    private def updateConnectionData(config: com.sforce.ws.ConnectorConfig) {
        sessionProperties.setProperty("sessionId", config.getSessionId)
        sessionProperties.setProperty("serviceEndpoint", config.getServiceEndpoint)
        appConfig.storeSessionProps()
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

    /**
     * clear session data and force login next time
     */
    def reset() {
        sessionProperties.remove("sessionId")
        sessionProperties.remove("serviceEndpoint")
        connection = null
        storeSessionData()
    }
    def withRetry(codeBlock: => Any) = {
        try {
            codeBlock
        } catch {
            case ex:ApiFault if ExceptionCode.INVALID_SESSION_ID == ex.getExceptionCode =>
                logger.debug("Session is invalid or has expired. Will run the process again with brand new connection. ", ex)
                reset()
                //run once again
                codeBlock
            case ex:Throwable =>
                throw ex
        }
    }

    def query(soql: String):QueryResult = {
        withRetry {
            getConnection.query(soql)
        }.asInstanceOf[QueryResult]
    }
    def create(objects: Array[com.sforce.soap.tooling.SObject]):Array[SaveResult] = {
        withRetry {
            getConnection.create(objects)
        }.asInstanceOf[Array[SaveResult]]
    }
    def getServerTimestamp = {
        withRetry {
            getConnection.getServerTimestamp
        }.asInstanceOf[GetServerTimestampResult]
    }

    private def getConnection: SoapConnection = {
        if (null == connection) {
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

            getSavedSessionData match {
                case (Some(sessionId), Some(serviceEndpoint)) =>
                    //use cached data
                    config.setSessionId(sessionId)
                    config.setServiceEndpoint(serviceEndpoint)
                    true
                case _ =>
                    //login explicitly
                    com.sforce.soap.partner.Connector.newConnection(config)
                    config.setServiceEndpoint(config.getServiceEndpoint.replace("/services/Soap/u/", "/services/Soap/T/"))
                    updateConnectionData(config)
                    false
            }
            //Tooling api can not connect on its own (something is wrong with the jar which wsc generates)
            //having to use a workaround - connect via SOAP and then use obtained session for tooling
            //val soapConnection = com.sforce.soap.partner.Connector.newConnection(config)
            val toolingConnection = com.sforce.soap.tooling.Connector.newConnection(config)
            //tooling api uses different service endpoint
            logger.info("Auth EndPoint: "+config.getAuthEndpoint)
            logger.info("Service EndPoint: "+config.getServiceEndpoint)
            logger.info("Username: "+config.getUsername)
            logger.info("SessionId: "+config.getSessionId)
            connection = toolingConnection

        }
        connection
    }

}


