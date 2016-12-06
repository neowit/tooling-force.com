package com.neowit.oauth

import java.io.IOException

import fi.iki.elonen.NanoHTTPD
import fi.iki.elonen.NanoHTTPD._

import scala.util.Try
import scala.collection.JavaConversions._
import scala.concurrent.Future


object WebServer {
    private val DEFAULT_PORT = 9000

    def dummyCallback(server: WebServer, urlParams: Map[String, List[String]]): Future[Unit] = {
        Future.successful(Unit)
    }
    def isOkToCallback(session: IHTTPSession): Boolean = {
        true
    }

    def main(args: Array[String]) {
        val port = if (args.isEmpty) DEFAULT_PORT else Try(args(0).toInt).getOrElse(DEFAULT_PORT)

        try {
            new WebServer(port, isOkToCallback, dummyCallback)
        } catch {
            case ioe: IOException => System.err.println("Couldn't start server:\n" + ioe)
        }
    }
}

class WebServer(port: Int,
                isOkToCallback: IHTTPSession => Boolean,
                onResponseCallback: (WebServer, Map[String, List[String]]) => Future[Unit]) extends NanoHTTPD(port) {

    start(NanoHTTPD.SOCKET_READ_TIMEOUT, false)

    println(s"\nRunning! Expecting requests at: http://localhost:$port/ \n")

    override def serve(session: IHTTPSession): Response = {
        val result = if (null == session.getParameters.get("error")) "Successful" else "Failed"
        var msg = s"<html><body><h1>Authorisation $result</h1>\n"
        if (null != session.getParameters.get("error")) {
            msg += "<p>" + session.getParameters.get("error") + "</p>"
            if (null != session.getParameters.get("error_description")) {
                msg += "<p>" + session.getParameters.get("error_description") + "</p>"
            }
        } else {
            msg += "<p>You can close this window</p>"
        }
        if (isOkToCallback(session)) {
            // initiate asynchronous callback
            onResponseCallback(this, session.getParameters.mapValues(_.toList).toMap)
        }

        NanoHTTPD.newFixedLengthResponse(msg + "</body></html>\n")
    }

}

