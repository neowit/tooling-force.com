package com.neowit.webserver

import org.eclipse.jetty.server.{Handler, Server}

//http://www.eclipse.org/jetty/documentation/current/embedding-jetty.html
class EmbeddedJetty(port: Int, handler: Handler ) {

    val server = new Server(port)
    server.setStopAtShutdown(true)

    server.setHandler(handler)

    server.start()
    //server.join() // commented out because do not need to join current thread

    def isAlive: Boolean = {
        server.isRunning || server.isStarting
    }

    def stop(): Unit =
        server.stop()
}


