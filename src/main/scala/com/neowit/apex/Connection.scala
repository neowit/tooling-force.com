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

import com.neowit.utils.{BasicConfig, Config, Logging}

object Connection extends Logging {
    val CLIENT_NAME = AppVersion.APP_NAME + "/" + AppVersion.VERSION

    def initConnectorConfig(appConfig: BasicConfig): com.sforce.ws.ConnectorConfig = {
        val config = new com.sforce.ws.ConnectorConfig()

        config.setCompression(true)

        val proxyHost = appConfig.getProperty("http.proxyHost")
        val proxyPort = appConfig.getProperty("http.proxyPort")

        val proxyUsername = appConfig.getProperty("http.proxyUsername") match {
            case Some(s) => Some(s)
            case None => appConfig.getProperty("http.proxyUser")
        }
        if (None != proxyUsername )
            config.setProxyUsername(proxyUsername.get)

        val proxyPassword = appConfig.getProperty("http.proxyPassword")
        if (None != proxyPassword )
            config.setProxyPassword(proxyPassword.get)

        if (None != proxyHost && None != proxyPort) {
            config.setProxy(proxyHost.get, proxyPort.get.toInt)
            logger.debug("Proxy: " + proxyHost + ":" + proxyPort)
        }

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

    private def getConnectionConfig (appConfig: Config) = {
        val config = initConnectorConfig(appConfig.basicConfig)
        config.setUsername(appConfig.username)
        config.setPassword(appConfig.password)

        val endpoint = appConfig.soapEndpoint
        if (null != endpoint)
            config.setAuthEndpoint(endpoint)
        config.setServiceEndpoint(endpoint)

        config
    }

    private def setClient(connection: com.sforce.soap.partner.PartnerConnection): com.sforce.soap.partner.PartnerConnection = {
        val callOptions = new com.sforce.soap.partner.CallOptions_element()
        callOptions.setClient(CLIENT_NAME)
        connection.__setCallOptions(callOptions)
        connection
    }
    def createPartnerConnection(appConfig: Config):com.sforce.soap.partner.PartnerConnection = {
        logger.debug("Creating NEW Partner Connection")
        val connectionConfig = getConnectionConfig(appConfig)
        setClient(com.sforce.soap.partner.Connector.newConnection(connectionConfig))

    }
    def getPartnerConnection(appConfig: Config, sessionId: String, serviceEndpoint: String):com.sforce.soap.partner.PartnerConnection = {
        val connectionConfig = getConnectionConfig(appConfig)
        connectionConfig.setSessionId(sessionId)
        connectionConfig.setServiceEndpoint(serviceEndpoint)
        val connection = com.sforce.soap.partner.Connector.newConnection(connectionConfig)
        setClient(connection)
    }

    /**
     * create brand new connection
     * @param appConfig
     * @return
     */
    def createMetadataConnection(appConfig: Config):com.sforce.soap.metadata.MetadataConnection = {
        logger.debug("Creating NEW Metadata Connection")
        val partnerConnection = createPartnerConnection(appConfig)
        getMetadataConnection(appConfig, partnerConnection)
    }

    /**
     * initialise MetadataConnection based on existing PartnerConnection
     * @param appConfig
     * @param partnerConnection
     * @return
     */
    def getMetadataConnection(appConfig: Config,
                              partnerConnection: com.sforce.soap.partner.PartnerConnection):com.sforce.soap.metadata.MetadataConnection = {
        val partnerConnectionConfig = partnerConnection.getConfig

        val connectionConfig = getConnectionConfig(appConfig)
        //Metadata api can not connect on its own (something is wrong with the jar which wsc generates)
        //having to use a workaround - connect via SOAP and then use obtained session for metadata connection
        connectionConfig.setSessionId(partnerConnectionConfig.getSessionId)
        connectionConfig.setServiceEndpoint(partnerConnectionConfig.getServiceEndpoint.replace("/services/Soap/u/", "/services/Soap/m/"))

        val metadataConnection = com.sforce.soap.metadata.Connector.newConnection(connectionConfig)
        setClient(metadataConnection)
    }

    private def setClient(connection: com.sforce.soap.metadata.MetadataConnection): com.sforce.soap.metadata.MetadataConnection = {
        val callOptions = new com.sforce.soap.metadata.CallOptions_element()
        callOptions.setClient(CLIENT_NAME)
        connection.__setCallOptions(callOptions)
        connection
    }

    /**
     * create brand new connection
     * @param appConfig
     * @return
     */
    def createToolingConnection(appConfig: Config):com.sforce.soap.tooling.ToolingConnection = {
        logger.debug("Creating NEW Tooling Connection")
        val partnerConnection = createPartnerConnection(appConfig)
        getToolingConnection(appConfig, partnerConnection)
    }

    /**
     * initialise tooling Connection based on existing PartnerConnection
     * @param appConfig
     * @param partnerConnection
     * @return
     */
    def getToolingConnection(appConfig: Config,
                              partnerConnection: com.sforce.soap.partner.PartnerConnection):com.sforce.soap.tooling.ToolingConnection = {
        val partnerConnectionConfig = partnerConnection.getConfig

        val connectionConfig = getConnectionConfig(appConfig)
        //Tooling api can not connect on its own (something is wrong with the jar which wsc generates)
        //having to use a workaround - connect via SOAP and then use obtained session for Tooling connection
        connectionConfig.setSessionId(partnerConnectionConfig.getSessionId)
        connectionConfig.setServiceEndpoint(partnerConnectionConfig.getServiceEndpoint.replace("/services/Soap/u/", "/services/Soap/T/"))

        val toolingConnection = com.sforce.soap.tooling.Connector.newConnection(connectionConfig)
        setClient(toolingConnection)
    }
    private def setClient(connection: com.sforce.soap.tooling.ToolingConnection): com.sforce.soap.tooling.ToolingConnection = {
        val callOptions = new com.sforce.soap.tooling.CallOptions_element()
        callOptions.setClient(CLIENT_NAME)
        connection.__setCallOptions(callOptions)
        connection
    }

    /**
     * create brand new connection
     * @param appConfig
     * @return
     */
    def createApexConnection(appConfig: Config):com.sforce.soap.apex.SoapConnection = {
        logger.debug("Creating NEW Apex Connection")
        val partnerConnection = createPartnerConnection(appConfig)
        getApexConnection(appConfig, partnerConnection)
    }

    /**
     * initialise apex Connection based on existing PartnerConnection
     * @param appConfig
     * @param partnerConnection
     * @return
     */
    def getApexConnection(appConfig: Config,
                             partnerConnection: com.sforce.soap.partner.PartnerConnection):com.sforce.soap.apex.SoapConnection = {
        val partnerConnectionConfig = partnerConnection.getConfig

        val connectionConfig = getConnectionConfig(appConfig)
        //Apex api can not connect on its own (something is wrong with the jar which wsc generates)
        //having to use a workaround - connect via SOAP and then use obtained session for Apex connection
        connectionConfig.setSessionId(partnerConnectionConfig.getSessionId)
        connectionConfig.setServiceEndpoint(partnerConnectionConfig.getServiceEndpoint.replace("/services/Soap/u/", "/services/Soap/s/"))

        val apexConnection = com.sforce.soap.apex.Connector.newConnection(connectionConfig)
        setClient(apexConnection)
    }
    private def setClient(connection: com.sforce.soap.apex.SoapConnection): com.sforce.soap.apex.SoapConnection = {
        val callOptions = new com.sforce.soap.apex.CallOptions_element()
        callOptions.setClient(CLIENT_NAME)
        connection.__setCallOptions(callOptions)
        connection
    }
}

