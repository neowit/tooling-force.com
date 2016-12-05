package com.neowit.apex.actions

import java.util.concurrent.TimeUnit

import com.neowit.oauth.{OAuth2JsonSupport, OAuthConsumer, WebServer}
import com.neowit.utils.{ConnectedAppKeys, ResponseWriter}
import com.neowit.utils.ResponseWriter.Message
import fi.iki.elonen.NanoHTTPD.IHTTPSession
import spray.json._

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

/**
  * Author: Andrey Gavrikov
  */
class LoginOauth extends ApexActionWithWritableSession with OAuth2JsonSupport {
    private val PORT = 9000

    private val CALLBACK_PATH = "_oauth_callback"
    private val CALLBACK_URL = s"http://localhost:$PORT/$CALLBACK_PATH"

    override def getHelp: ActionHelp = new ActionHelp {
        override def getParamNames: List[String] = List("responseFilePath")

        override def getSummary: String = "Get salesforce.com session Id via OAuth2"

        override def getName: String = "login"

        override def getExample: String = "login --env=test.salesforce.com --projectPath='/path/to/project' --responseFilePath='/tmp/response.txt' "

        override def getParamDescription(paramName: String): String = paramName match {
            case "env" =>
                """--env - salesforce environment URL.
                  |     e.g.
                  |      - sandbox: test.salesforce.com'
                  |      - production: login.salesforce.com'
                  |     """.stripMargin
            case "responseFilePath" => "--responseFilePath - path to file where operation result will be reported"
            case "projectPath" => "--projectPath - path to folder parent of ./src/ folder"
            case x => s"Parameter '$x' is not supported for this action"
        }

    }

    override protected def act(): Unit = {
        ConnectedAppKeys.load() match {
            case Some(keys) =>
                config.getRequiredProperty("env") match {
                    case Some(env) =>
                        val consumer = new OAuthConsumer(config.basicConfig, env, consumerKey = keys.consumerKey, consumerSecret = keys.consumerSecret, callbackUrl = CALLBACK_URL)
                        println(consumer.getLoginUrl(env).getOrElse("").toString)
                        // start server
                        val server = new WebServer(PORT, isOkToCallback, onResponseCallback(consumer))
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
    private def isOkToCallback(session: IHTTPSession): Boolean = {
        session.getUri.contains(CALLBACK_PATH)
    }

    private def onResponseCallback(consumer: OAuthConsumer)(server: WebServer, urlParams: Map[String, List[String]]): Future[Unit] = {
        Future {

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
                consumer.getTokens(env, code) match {
                    case Some(tokens) =>
                        println("loaded tokens")
                        session.storeConnectionData(tokens, allowWrite = true)
                        config.responseWriter.println("RESULT=SUCCESS")
                        config.responseWriter.println(tokens.toJson.prettyPrint)
                    case None =>
                        config.responseWriter.println("RESULT=FAILURE")
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
