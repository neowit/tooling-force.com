package com.neowit.apex.actions

import java.io.File
import java.net.URL
import java.util.concurrent.TimeUnit

import com.neowit.auth.{OAuth2JsonSupport, OAuthConsumer}
import com.neowit.utils._
import com.neowit.utils.ResponseWriter.Message
import com.neowit.webserver.{EmbeddedJetty, Oauth2Handler}
import spray.json._

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

/**
  * Author: Andrey Gavrikov
  */
class LoginOauth extends ApexAction with OAuth2JsonSupport {

    override def getHelp: ActionHelp = new ActionHelp {
        override def getParamNames: List[String] = List("env", "responseFilePath", "saveAuthPath")

        override def getSummary: String = "Get salesforce.com session Id via OAuth2"

        override def getName: String = "login"

        override def getExample: String =
            """--action=login --env="test.salesforce.com" --responseFilePath="/tmp/response.txt" --saveAuthPath="/tmp/secret/oauth2/my-project" """

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
            case "saveAuthPath" => "--saveAuthPath - path to file where authentication result should be saved"
            case x => s"Parameter '$x' is not supported for this action"
        }

    }

    private var server: Option[EmbeddedJetty] = None

    override protected def act(): Unit = {
        ConnectedAppKeys.load() match {
            case Some(keys) =>
                config.getRequiredProperty("env") match {
                    case Some(env) =>
                        val consumer = new OAuthConsumer(config.basicConfig, env, consumerKey = keys.consumerKey, consumerSecret = keys.consumerSecret, callbackUrl = keys.callbackUrl)
                        val directUrl = consumer.getLoginUrl.getOrElse("").toString
                        val shortUrl = Oauth2Handler.getStartPagePath(keys.getPort)
                        if (OsUtils.openUrl(directUrl)) {
                            // in case if open fails
                            println("If web browser failed to open automatically please follow this url: " + shortUrl)
                        } else {
                            // unsupported OS, provide alternative (short) url to open manually
                            val message = new Message(ResponseWriter.WARN, "Open this URL in your web browser: " + shortUrl)
                            config.responseWriter.println(message)
                            //println("Open this URL in your web browser: " + shortUrl)
                        }

                        // start server
                        val callbackPath = new URL(keys.callbackUrl).getPath
                        val handler = new Oauth2Handler(consumer, callbackPath, onResponseCallback(consumer))
                        server = Option(new EmbeddedJetty(keys.getPort, handler))
                        while (server.get.isAlive) {
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

    private def getOutputFile: Option[File] = {
        config.getProperty("saveAuthPath") match {
            case Some(saveAuthPath) =>
                val file = new File(saveAuthPath)
                // make sure file does not exist
                FileUtils.delete(file)
                // make sure all path folders exist
                file.getParentFile.mkdirs()
                Option(file)
            case None =>
                None
        }
    }

    private def onResponseCallback(consumer: OAuthConsumer)(urlParams: Map[String, List[String]]): Future[Unit] = {
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
                        getOutputFile match {
                            case Some(outputFile) =>
                                // next step is to extract session id via call to: https://login.salesforce.com/services/oauth2/token,
                                // see here: https://developer.salesforce.com/page/Digging_Deeper_into_OAuth_2.0_at_Salesforce.com#The_Salesforce.com_Identity_Service
                                consumer.getTokens(code) match {
                                    case Some(tokens) =>
                                        println("loaded tokens")
                                        FileUtils.writeFile(tokens.toJson.prettyPrint, outputFile)
                                        //session.storeConnectionData(tokens, env, allowWrite = true)
                                        config.responseWriter.println("RESULT=SUCCESS")
                                        config.responseWriter.println(tokens.toJson.prettyPrint)
                                    case None =>
                                        config.responseWriter.println("RESULT=FAILURE")
                                }
                            case None =>
                                config.responseWriter.println("RESULT=FAILURE")
                                val message = new Message(ResponseWriter.ERROR, "Missing or invalid value of --saveAuthPath parameter.")
                                config.responseWriter.println(message)
                        }

                    }
            }

            // give server a chance to send output message
            delayedExecution(1) {
                println("stopping server")
                server.foreach(_.stop())
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

