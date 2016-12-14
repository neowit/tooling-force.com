package com.neowit.webserver

/**
  * Author: Andrey Gavrikov
  * Date: 15/12/2016
  */
import javax.servlet.http.{HttpServletRequest, HttpServletResponse}

import com.neowit.auth.OAuthConsumer
import org.eclipse.jetty.server.Request
import org.eclipse.jetty.server.handler.AbstractHandler

import scala.concurrent.Future

import scala.collection.JavaConversions._

class Oauth2Handler(consumer: OAuthConsumer, oauth2CallBackPath: String, onResponseCallback: (Map[String, List[String]]) => Future[Unit]) extends AbstractHandler {

    import Oauth2Handler._

    override def handle(target: String, baseRequest: Request, request: HttpServletRequest, response: HttpServletResponse): Unit = {
        response.setContentType("text/html; charset=utf-8")

        val html =
        target match {
            case START_PAGE_PATH =>
                response.setStatus(HttpServletResponse.SC_OK)
                getAuthStartPage(consumer)
            case `oauth2CallBackPath` =>
                // initiate asynchronous callback
                onResponseCallback(request.getParameterMap.mapValues(_.toList).toMap)
                response.setStatus(HttpServletResponse.SC_OK)
                getOauthCompletePage(request)
            case _ =>
                response.setStatus(HttpServletResponse.SC_BAD_REQUEST)
                s"No handler for url: $target"
        }
        val out = response.getWriter

        out.println(html)
        baseRequest.setHandled(true)

    }
}

object Oauth2Handler {
    private val START_PAGE_PATH: String = "/startOAuth"

    def getStartPagePath(port: Int): String = {
        s"http://localhost:$port$START_PAGE_PATH"
    }
    //give user link to this page instead of the long link with all tokens
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

    def getOauthCompletePage(request: HttpServletRequest): String = {
        if (null != request.getParameter("error")){
            val error = request.getParameter("error")
            val errorDescription =
                if (null != request.getParameter("error_description")) {
                    request.getParameter("error_description")
                } else {
                    ""
                }
            getAuthErrorPageContent(error, errorDescription)
        } else {
            getAuthSuccessPageContent
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

}
