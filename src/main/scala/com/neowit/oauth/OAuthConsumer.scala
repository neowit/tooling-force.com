package com.neowit.oauth

import java.io.OutputStreamWriter
import java.net.{URL, URLEncoder}
import java.util.zip.GZIPInputStream

import scala.util.Try
import com.neowit.apex.{Connection, Session}
import com.neowit.utils.{BasicConfig, Logging}

import scala.util.{Failure, Success}
//import scala.concurrent.ExecutionContext.Implicits.global

import spray.json._
/**
  * Author: Andrey Gavrikov
  * Date: 05/12/2016
  */


class OAuthConsumer(appConfig: BasicConfig,
                    environment: String,
                    consumerKey: String,
                    consumerSecret: String,
                    callbackUrl: String) extends Logging with OAuth2JsonSupport{
    private def getServerUrl(environment: String): String = {

        "https://" + environment
    }

    /**
      * Step 1 - obtain authorisation code
      * @param environment - e.g. login.salesforce.com
      * @return
      */
    def getLoginUrl(environment: String): Try[URL] =  {
        val params = encodeUrlParams(
            Map(
                // response_type must be "code" to make sure that response string looks like
                //  "callback-url?param=..." - which allows parameter extraction on the server side
                // instead of "response_type=token" which results in:
                //  "callback-url#param=..." - which does not allow query string parameter extraction on server side
                "response_type" -> "code",
                "client_id" -> consumerKey,
                "redirect_uri" -> callbackUrl,
                "state" -> environment,
                "scope" -> "id api web refresh_token" //refresh_token must be requested explicitly
            )
        )
        val serverUrl: String = getServerUrl(environment)
        val url = Try(new URL(serverUrl + "/services/oauth2/authorize?" + params))
        url
    }

    private def encodeUrlParams(params: Map[String, String]): String = {
        val encodedParams =
            params.map{
                case (name, value) => name + "=" + URLEncoder.encode(value, "UTF8")
            }
        encodedParams.mkString("&")
    }

    def getTokens(env: String, code: String): Option[Oauth2Tokens] = {
        val serverUrl: String = getServerUrl(env) + "/services/oauth2/token"
        val params =
            Map(
                "code" -> code,
                "grant_type" -> "authorization_code",
                "client_id" -> consumerKey,
                "client_secret" -> consumerSecret,
                "redirect_uri" -> callbackUrl

            )

        sendPost(serverUrl, params).toOption
    }

    private def sendPost(url: String, params: Map[String, String]):Try[Oauth2Tokens] = {
        val connectionConfig = Connection.initConnectorConfig(appConfig)
        val conn = connectionConfig.createConnection(new URL(url), new java.util.HashMap[String, String](), false)
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
            val in = conn.getInputStream
            val text = conn.getContentEncoding match {
                case "gzip" => io.Source.fromInputStream(new GZIPInputStream(in))("UTF-8").mkString("")
                case _ => io.Source.fromInputStream(in)("UTF-8").mkString("")
            }

            in.close()
            val json = text.parseJson
            println(json.prettyPrint)
            Success(json.convertTo[Oauth2Tokens])
        } else {
            if (401 == responseCode) { //Unauthorized
                throw Session.UnauthorizedConnectionException(conn)
            } else {
                logger.error(s"Request Failed - URL=$url")
                logger.error(s"Response Code: $responseCode, Response Message: ${conn.getResponseMessage}")
                val ex = Session.ConnectionException(conn)
                Failure(ex)
            }
        }
    }

}
