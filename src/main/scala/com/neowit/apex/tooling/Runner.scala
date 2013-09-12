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

import com.sforce.soap.tooling.{ApexClass, SoapConnection}


object Runner extends Logging{
    val appConfig = Config.getConfig

    def main(args: Array[String]) {
        if (args.isEmpty) {
            appConfig.help()

        } else {
            try {
                appConfig.load(args.toList)
                run()
            } catch {
                case ex: InvalidCommandLineException => appConfig.help()
                case ex: MissingRequiredConfigParameterException => logger.error(ex.getMessage)
            } finally {
            }
        }
    }

    def run () {

        val session = new SfdcSession(appConfig)

        val connection = session.getConnection
        val cls = new ApexClass
        cls.setBody(
            """
              public class Messages {
                static {}
              }
            """)
        val saveResult = connection.create(Array(cls))
        logger.debug("saveResult=" + saveResult)



    }

    /**
     * OAuth password flow requires client secret which can not be published with the application
     * So having to cheat here and obtain session via SOAP API
     *
     */

    class SfdcSession (appConfig: Config) {
        var connection = getConnection

        def getConnection: SoapConnection = {
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

                //Tooling api can not connect on its own (something is wrong with the jar which wsc generates)
                //having to use a workaround - connect via SOAP and then use obtained session for tooling
                val soapConnection = com.sforce.soap.partner.Connector.newConnection(config)
                val toolingConnection = com.sforce.soap.tooling.Connector.newConnection(config)
                //tooling api uses different service endpoint
                config.setServiceEndpoint(config.getServiceEndpoint.replace("/services/Soap/u/", "/services/Soap/T/"))
                logger.info("Auth EndPoint: "+config.getAuthEndpoint)
                logger.info("Service EndPoint: "+config.getServiceEndpoint)
                logger.info("Username: "+config.getUsername)
                logger.info("SessionId: "+config.getSessionId)
                connection = toolingConnection

            }
            connection
        }

    }


}
