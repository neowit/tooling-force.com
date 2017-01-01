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

package com.neowit.apex.actions

import java.io.File
import java.net.URL
import java.util.UUID

import com.neowit.auth.{OAuth2JsonSupport, OAuthConsumer}
import com.neowit.utils._
import com.neowit.response.ErrorMessage
import com.neowit.webserver.{EmbeddedJetty, Oauth2Handler}
import spray.json._

import scala.concurrent.{ExecutionContext, Future, Promise}
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

    override protected def act()(implicit ec: ExecutionContext): Future[ActionResult] = {
        val outputFile: File = getOutputFile match {
            case Some(file) => file
            case None =>
                //config.responseWriter.println("RESULT=FAILURE")
                //val message = new Message(ERROR, "Missing or invalid value of --saveAuthPath parameter.")
                //config.responseWriter.println(message)
                return Future.successful(ActionFailure(ErrorMessage("Missing or invalid value of --saveAuthPath parameter.")))
        }

        ConnectedAppKeys.load() match {
            case Some(keys) =>
                config.getProperty("env") match {
                    case Some(env) =>
                        val handlerUuid = UUID.randomUUID()
                        val handlerId = handlerUuid.toString
                        val consumer =
                            new OAuthConsumer(
                                config.basicConfig,
                                env,
                                consumerKey = keys.consumerKey,
                                consumerSecret = keys.consumerSecret,
                                callbackUrl = keys.callbackUrl,
                                state = Option(handlerId) // use request state parameter as unique request identifier
                            )
                        val directUrl = consumer.getLoginUrl.getOrElse("").toString
                        val shortUrl = Oauth2Handler.getStartPagePath(keys.getPort)
                        if (OsUtils.openUrl(directUrl)) {
                            // in case if open fails
                            println("If web browser failed to open automatically please follow this url: " + shortUrl)
                        } else {
                            // unsupported OS, provide alternative (short) url to open manually
                            //val message = WarnMessage("Open this URL in your web browser: " + shortUrl)
                            println("Open this URL in your web browser: " + shortUrl)
                        }

                        // start server
                        EmbeddedJetty.start(keys.getPort) match {
                            case Right(_) =>
                                val callbackPath = new URL(keys.callbackUrl).getPath
                                val onCompletePromise = Promise[ActionResult]()
                                val handler = new Oauth2Handler(consumer, callbackPath, onResponseCallback(outputFile, consumer, onCompletePromise))
                                EmbeddedJetty.addHandler(handlerId, handler)
                                // wait for process to complete
                                /*
                                while (EmbeddedJetty.hasHandler(handlerId)) {
                                    Thread.sleep(1000)
                                }
                                EmbeddedJetty.stop()
                                */
                                onCompletePromise.future
                            case Left(err) =>
                                //config.responseWriter.println("RESULT=FAILURE")
                                //val message = new Message(ERROR, err)
                                //config.responseWriter.println(message)
                                Future.successful(ActionFailure(ErrorMessage(err)))
                        }

                    case None =>
                        Future.successful(ActionFailure("Missing required parameter: --env"))
                }
            case None =>
                //config.responseWriter.println("RESULT=FAILURE")
                //val message = new Message(ERROR, "Invalid jar file, missing consumer key/secret configuration")
                //config.responseWriter.println(message)
                Future.successful(ActionFailure(ErrorMessage("Invalid jar file, missing consumer key/secret configuration")))
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

    private def onResponseCallback(outputFile: File, consumer: OAuthConsumer, onCompletePromise: Promise[ActionResult])(urlParams: Map[String, List[String]]): Future[Unit] = {
        Future {

            val actionResult =
                urlParams.get("error") match {
                    case Some(values) =>
                        val error = values.head
                        //config.responseWriter.println("RESULT=FAILURE")
                        val errorMessage =
                            urlParams.get("error_description") match {
                                case Some(strings) =>
                                    val errorDescription = strings.head
                                    val message = ErrorMessage(error + ": "  + errorDescription)
                                    //config.responseWriter.println(message)
                                    message
                                case _ =>
                                    val message = ErrorMessage(error)
                                    //config.responseWriter.println(message)
                                    message
                            }
                        ActionFailure(errorMessage)
                    case None =>
                        val code = urlParams.get("code") match {
                            case Some(values) => values.head
                            case None => ""
                        }

                        if (code.nonEmpty) {
                            consumer.getTokens(code) match {
                                case Some(tokens) =>
                                    println("loaded tokens")
                                    FileUtils.writeFile(tokens.toJson.prettyPrint, outputFile)
                                    //config.responseWriter.println("RESULT=SUCCESS")
                                    //config.responseWriter.println(tokens.toJson.prettyPrint)
                                    ActionSuccess(tokens.toJson.prettyPrint)
                                case None =>
                                    //config.responseWriter.println("RESULT=FAILURE")
                                    ActionFailure()
                            }
                        } else {
                            ActionFailure("Unexpected response returned: response missing 'code' parameter")
                        }
                }

            urlParams.get("state") match {
                case Some(handlerId :: _) =>
                    EmbeddedJetty.removeHandler(handlerId)
                case _ =>
                    // response missing state parameter - something must have gone wrong, can not stop server explicitly
            }
            onCompletePromise.success(actionResult)
        }

    }



}

