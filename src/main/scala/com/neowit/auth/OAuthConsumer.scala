/*
 * Copyright (c) 2017 Andrey Gavrikov.
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

package com.neowit.auth

import java.io.OutputStreamWriter
import java.net.{HttpURLConnection, URL, URLEncoder}
import java.util.zip.GZIPInputStream

import scala.util.Try
import com.neowit.apex.{Connection, Session}
import com.neowit.utils.{BasicConfig, Logging}

import scala.util.{Failure, Success}
//import scala.concurrent.ExecutionContext.Implicits.global

import spray.json._
/**
  * Author: Andrey Gavrikov
  */


/**
  *
  * @param appConfig - basic configuration
  * @param _environment - e.g. login.salesforce.com
  * @param callbackUrl - must be *exactly* the same url as configured in connected app settings
  */
class OAuthConsumer(appConfig: BasicConfig,
                    private val _environment: String,
                    consumerKey: String,
                    consumerSecret: String,
                    callbackUrl: String,
                    state: Option[String] = None
                   ) extends Logging with OAuth2JsonSupport{

    val environment: String = {
        _environment.replaceAll("^http[s]?://", "")
    }

    /**
      * @param environment - expects host name, without protocol or path
      * @return
      */
    private def getServerUrl(environment: String): Try[URL] = {
        Try(new URL("https://" + environment))
    }

    /**
      * Obtaining an Access Token in a Web Application (Web Server Flow)
      * Step 1 - obtain authorisation code
      * https://developer.salesforce.com/page/Digging_Deeper_into_OAuth_2.0_at_Salesforce.com#Obtaining_an_Access_Token_in_a_Web_Application_.28Web_Server_Flow.29
      * @return
      */
    def getLoginUrl: Try[URL] =  {
        val params = encodeUrlParams(
            Map(
                // response_type must be "code" to make sure that response string looks like
                //  "callback-url?param=..." - which allows parameter extraction on the server side
                // instead of "response_type=token" which results in:
                //  "callback-url#param=..." - which does not allow query string parameter extraction on server side
                "response_type" -> "code",
                "client_id" -> consumerKey,
                "redirect_uri" -> callbackUrl,
                "state" -> state.getOrElse(environment),
                "prompt" -> "login",
                "scope" -> "id api web refresh_token" //refresh_token must be requested explicitly
            )
        )
        getServerUrl(environment).flatMap{serverUrl =>
            val url = Try(new URL(serverUrl, "/services/oauth2/authorize?" + params))
            url
        }
    }

    private def encodeUrlParams(params: Map[String, String]): String = {
        val encodedParams =
            params.map{
                case (name, value) => name + "=" + URLEncoder.encode(value, "UTF8")
            }
        encodedParams.mkString("&")
    }

    /**
      * Obtaining an Access Token in a Web Application (Web Server Flow)
      * Step 2: get OAuth2 tokens using code obtained via explicit user login
      * @return
      */
    def getTokens(code: String): Option[Oauth2Tokens] = {
        //val serverUrl: String = getServerUrl(environment) + "/services/oauth2/token"
        getServerUrl(environment).flatMap{serverUrl =>

            val params =
                Map(
                    "code" -> code,
                    "grant_type" -> "authorization_code",
                    "client_id" -> consumerKey,
                    "client_secret" -> consumerSecret,
                    "redirect_uri" -> callbackUrl

                )
            Try(new URL(serverUrl, "/services/oauth2/token")).map{url =>
                sendPost(url, params).toOption
            }
        }.getOrElse(None)
    }

    /**
      * https://developer.salesforce.com/page/Digging_Deeper_into_OAuth_2.0_at_Salesforce.com#Token_Refresh
      * get OAuth2 tokens using refresh_token obtained at some point in the past
      * @return
      */
    def refreshTokens(refreshToken: String): Either[Throwable, Oauth2Tokens] = {
        //val serverUrl: String = getServerUrl(environment) + "/services/oauth2/token"
        getServerUrl(environment).flatMap{serverUrl =>
            val params =
                Map(
                    "refresh_token" -> refreshToken,
                    "grant_type" -> "refresh_token",
                    "client_id" -> consumerKey,
                    "client_secret" -> consumerSecret

                )

            sendPost(new URL(serverUrl, "/services/oauth2/token"), params)
        } match {
            case Success(tokens) => Right(tokens)
            case Failure(ex) => Left(ex)
        }
    }

    private def sendPost(url: URL, params: Map[String, String]):Try[Oauth2Tokens] = {
        def extractJsonResponse(conn: HttpURLConnection): Option[JsValue] = {
            Try{
                val in = conn.getInputStream
                val text = conn.getContentEncoding match {
                    case "gzip" => scala.io.Source.fromInputStream(new GZIPInputStream(in))("UTF-8").mkString("")
                    case _ => scala.io.Source.fromInputStream(in)("UTF-8").mkString("")
                }

                in.close()
                val json = text.parseJson
                logger.debug(json.prettyPrint)
                json
            }.toOption
        }
        val connectionConfig = Connection.initConnectorConfig(appConfig)
        val conn = connectionConfig.createConnection(url, new java.util.HashMap[String, String](), false)
        conn.setRequestMethod("POST")
        conn.setDoOutput(true)
        conn.setDoInput(true)
        conn.connect()
        //val wr = new OutputStreamWriter(new GZIPOutputStream(conn.getOutputStream))
        val wr = new OutputStreamWriter(conn.getOutputStream) // SFDC Auth does not like GZIP
        val urlParameters = encodeUrlParams(params)
        wr.write(urlParameters)
        wr.flush()
        wr.close()

        val responseCode = conn.getResponseCode
        if (200 == responseCode) {
            extractJsonResponse(conn) match {
                case Some(json) =>
                    Success(json.convertTo[Oauth2Tokens])
                case None =>
                    Failure(new RuntimeException("Failed to extract JSON response"))
            }
        } else {
            if (401 == responseCode) { //Unauthorized
                throw Session.UnauthorizedConnectionException(conn)
            } else {
                logger.error(s"Request Failed - URL=$url")
                logger.error(s"Response Code: $responseCode, Response Message: ${conn.getResponseMessage}")
                val ex = Session.ConnectionException(conn)
                extractJsonResponse(conn) match {
                    case Some(json) =>
                        logger.debug(json.prettyPrint)
                    case None =>
                }
                Failure(ex)
            }
        }
    }

}
