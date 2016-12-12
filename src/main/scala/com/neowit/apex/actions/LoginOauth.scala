package com.neowit.apex.actions

import java.net.URL
import java.util.concurrent.TimeUnit

import com.neowit.oauth.{OAuth2JsonSupport, OAuthConsumer}
import com.neowit.utils.{ConnectedAppKeys, Logging, OsUtils, ResponseWriter}
import com.neowit.utils.ResponseWriter.Message
import fi.iki.elonen.NanoHTTPD
import fi.iki.elonen.NanoHTTPD.{IHTTPSession, Response}
import spray.json._

import scala.collection.JavaConversions._
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

/**
  * Author: Andrey Gavrikov
  */
class LoginOauth extends ApexActionWithWritableSession with OAuth2JsonSupport {

    override def getHelp: ActionHelp = new ActionHelp {
        override def getParamNames: List[String] = List("responseFilePath")

        override def getSummary: String = "Get salesforce.com session Id via OAuth2"

        override def getName: String = "login"

        override def getExample: String =
            """login --env=test.salesforce.com --projectPath='/path/to/project' --responseFilePath="/tmp/response.txt" --autoRefresh=true """

        override def getParamDescription(paramName: String): String = paramName match {
            case "env" =>
                """--env - salesforce environment URL.
                  |     e.g.
                  |      test.salesforce.com               # sandbox
                  |      login.salesforce.com              # production
                  |      prerellogin.pre.salesforce.com    # pre-release environment
                  |      na1-abc.def.salesforce.com        # custom environment
                  |
                """.stripMargin
            case "responseFilePath" => "--responseFilePath - path to file where operation result will be reported"
            case "projectPath" => "--projectPath - path to folder parent of ./src/ folder"
            case "autoRefresh" =>
                """--autoRefresh - [true|false]
                  |     Optional, default is 'true'.
                  |     When you make requests with expired access token, "tooling-force.com" will automatically attempt
                  |     to refresh your access token from Salesforce and re-try your original request...
                """.stripMargin
            case x => s"Parameter '$x' is not supported for this action"
        }

    }

    override protected def act(): Unit = {
        ConnectedAppKeys.load() match {
            case Some(keys) =>
                config.getRequiredProperty("env") match {
                    case Some(env) =>
                        val consumer = new OAuthConsumer(config.basicConfig, env, consumerKey = keys.consumerKey, consumerSecret = keys.consumerSecret, callbackUrl = keys.callbackUrl)
                        val directUrl = consumer.getLoginUrl.getOrElse("").toString
                        val shortUrl = WebServer.getStartPagePath(keys.getPort)
                        OsUtils.openUrl(directUrl, shortUrl)
                        println("If web browser failed to open automatically please follow this url: " + shortUrl)
                        // start server
                        val callbackPath = new URL(keys.callbackUrl).getPath
                        val server = new WebServer(keys.getPort, callbackPath, consumer, onResponseCallback(consumer))
                        while (server.isAlive) {
                            Thread.sleep(1000)
                        }
                    case None =>
                }
            case None =>
                config.responseWriter.println("RESULT=FAILURE")
                val message = new Message(ResponseWriter.ERROR, "Invalid jar file, missing consumer key/secret configuration")
                config.responseWriter.println(message)
        }

    }

    /**
      * callback should only be called when request was made exactly to CALLBACK_PATH
      * if request is made to a different path, e.g. favicon or something, then there is no need to call callback
      */
    private def isOkToCallback(callbackPath: String)(session: IHTTPSession): Boolean = {
        session.getUri.contains(callbackPath)
    }

    private def onResponseCallback(consumer: OAuthConsumer)(server: WebServer, urlParams: Map[String, List[String]]): Future[Unit] = {
        Future {

            urlParams.get("error") match {
                case Some(values) =>
                    val error = values.head
                    config.responseWriter.println("RESULT=FAILURE")
                    urlParams.get("error_description") match {
                        case Some(strings) =>
                            val errorDescription = strings.head
                            val message = new Message(ResponseWriter.ERROR, error + ": "  + errorDescription)
                            config.responseWriter.println(message)
                        case _ =>
                            val message = new Message(ResponseWriter.ERROR, error)
                            config.responseWriter.println(message)
                    }
                case None =>
                    val code = urlParams.get("code") match {
                        case Some(values) => values.head
                        case None => ""
                    }
                    val env = urlParams.get("state") match {
                        case Some(values) => values.head
                        case None => ""
                    }

                    if (code.nonEmpty) {
                        // next step is to extract session id via call to: https://login.salesforce.com/services/oauth2/token,
                        // see here: https://developer.salesforce.com/page/Digging_Deeper_into_OAuth_2.0_at_Salesforce.com#The_Salesforce.com_Identity_Service
                        consumer.getTokens(code) match {
                            case Some(tokens) =>
                                println("loaded tokens")
                                session.storeConnectionData(tokens, env, allowWrite = true)
                                config.responseWriter.println("RESULT=SUCCESS")
                                config.responseWriter.println(tokens.toJson.prettyPrint)
                            case None =>
                                config.responseWriter.println("RESULT=FAILURE")
                        }

                    }
            }

            // give server a chance to send output message
            delayedExecution(1) {
                println("stopping server")
                server.stop()
            }
        }

    }

    private def delayedExecution(delaySec: Long) (codeUnit: => Unit): Future[Unit] = {
        Future.successful {
            TimeUnit.SECONDS.sleep(delaySec)
            codeUnit
        }
    }


}

private object WebServer {
    private val START_PAGE_PATH: String = "/startOAuth"

    def getStartPagePath(port: Int): String = {
        s"http://localhost:$port$START_PAGE_PATH"
    }
    //TODO - give user link to this page instead of the long link with all tokens
    private def getAuthStartPage(consumer: OAuthConsumer): String = {
        consumer.getLoginUrl.toOption  match {
            case Some(linkUrl) =>
                val html =
                    s"""
                       |<html><body>
                       |<a href="$linkUrl">Click here to start Authentication process</a>
                       |</body></html>
                    """.stripMargin
                html
            case None =>
                val html =
                    s"""
                       |<html><body>
                       |<h1>Error. Unable to determine salesforce auth URL.</h1>
                       |</body></html>
                    """.stripMargin
                html
        }
    }

    def serveStartPage(consumer: OAuthConsumer): Response = {
        NanoHTTPD.newFixedLengthResponse(getAuthStartPage(consumer))
    }

    def serveOAuthCompletePage(session: IHTTPSession): Response = {
        val result = if (null == session.getParameters.get("error")) "Successful" else "Failed"

        if (null != session.getParameters.get("error")){
            val error = session.getParameters.get("error").head
            val errorDescription =
                if (null != session.getParameters.get("error_description")) {
                    session.getParameters.get("error_description").head
                } else {
                    ""
                }
            NanoHTTPD.newFixedLengthResponse(getAuthErrorPageContent(error, errorDescription))
        } else {

            NanoHTTPD.newFixedLengthResponse(getAuthSuccessPageContent)
        }
    }

    private def getAuthSuccessPageContent: String = {
       """
         |<html>
         |    <head>
         |        <style>
         |        .centered {
         |            position: fixed;
         |            top: 5%;
         |            left: 50%;
         |            transform: translate(-50%, -5%);
         |        }
         |        </style>
         |    </head>
         |    <body>
         |        <div class="centered">
         |            <h1>tooling-force.com</h1>
         |            <h3>Authorisation Successful</h3>
         |            <p>You may now close this window</p>
         |        </div>
         |    </body>
         |</html>
         |
       """.stripMargin
    }

    private def getAuthErrorPageContent(error: String, errorDescription: String): String = {
        s"""
          |<html>
          |    <head>
          |        <style>
          |        .centered {
          |            position: fixed;
          |            top: 5%;
          |            left: 50%;
          |            transform: translate(-50%, -5%);
          |        }
          |        </style>
          |    </head>
          |    <body>
          |        <div class="centered">
          |            <h1>tooling-force.com</h1>
          |            <h3>Authorisation FAILED</h3>
          |            <p>$error</p>
          |            <p>$errorDescription</p>
          |        </div>
          |    </body>
          |</html>
          |
        """.stripMargin
    }


    def serveUndefinedPathPage(session: IHTTPSession): Response = {

        val html =
            s"""
               |<html><body>
               |No handler for specified URL: ${session.getUri }
               |</body></html>
            """.stripMargin
        NanoHTTPD.newFixedLengthResponse(html)
    }
}
private class WebServer(port: Int,
                        oauth2CallBackPath: String,
                        consumer: OAuthConsumer,
                        onResponseCallback: (WebServer, Map[String, List[String]]) => Future[Unit]
                       ) extends NanoHTTPD(port) with Logging {

    start(NanoHTTPD.SOCKET_READ_TIMEOUT, false)

    logger.debug(s"\nRunning! Expecting requests at: http://localhost:$port/ \n")

    override def serve(session: IHTTPSession): Response = {
        session.getUri match {
            case WebServer.START_PAGE_PATH => // e.g. "/startOAuth"
                WebServer.serveStartPage(consumer)

            case `oauth2CallBackPath` => // e.g. "/_oauth_callback"
                // initiate asynchronous callback
                onResponseCallback(this, session.getParameters.mapValues(_.toList).toMap)
                WebServer.serveOAuthCompletePage(session)

            case _ =>
                WebServer.serveUndefinedPathPage(session)
        }

    }

}
