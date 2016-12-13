package com.neowit.auth

import java.net.URL
import java.nio.file.Path
import java.util.Properties

import com.neowit.utils.{FileUtils, OptionProperties}
import spray.json.JsonParser

import scala.util.Try

/**
  * Author: Andrey Gavrikov
  */

object AuthCredentials extends OAuth2JsonSupport {
    /**
      * @param path path to file with authentication credentials
      *             .properties for login/pass
      *             json file with oauth2 tokens obtained via "login" call
      * @return if None then provided file either not readable or does not point to supported auth file type
      */
    def load(path: Path): Option[AuthCredentials] = {
        FileUtils.getExtension(path) match {
            case "properties" => // this looks like login/pass
                Try{
                    val props = new Properties() with OptionProperties
                    props.load(FileUtils.readFile(path.toFile).bufferedReader())
                    props
                }.toOption match {
                    case Some(props) =>
                        props.getPropertyOption("sf.username").flatMap{ username =>
                            props.getPropertyOption("sf.password").flatMap{ password =>
                                props.getPropertyOption("sf.serverurl").map{serverUrl =>
                                    LoginPasswordCredentials(username, password, serverUrl)
                                }
                            }
                        }
                    case None =>
                        None
                }

            case _ => //assume oauth2
                Try{
                    val source = FileUtils.readFile(path)
                    val doc = source.getLines().mkString
                    val jsonAst = JsonParser(doc)
                    val tokens = jsonAst.convertTo[Oauth2Tokens]
                    Oauth2Credentials(tokens)
                }.toOption
        }
    }
}

sealed trait AuthCredentials

case class LoginPasswordCredentials(username: String, password: String, serverurl: String) extends AuthCredentials {
    def getSoapEndpoint(apiVersion: Double): String = {
        serverurl + "/services/Soap/u/" + apiVersion
    }
}
case class Oauth2Credentials(tokens: Oauth2Tokens) extends AuthCredentials {
    def getSoapEndpoint(apiVersion: Double): String = {
        tokens.instance_url.map(_ + "/services/Soap/u/" + apiVersion).getOrElse("")
    }

    /**
      * @return e.g. https://login.salesforce.com
      */
    def getLoginUrl: Option[String] = {
        tokens.getLoginUrl
    }

    /**
      * @return e.g. login.salesforce.com
      */
    def getEnvironment: Option[String] = {
        getLoginUrl match {
            case Some(url) => Try(new URL(url).getHost).toOption
            case None => None
        }
    }
}
