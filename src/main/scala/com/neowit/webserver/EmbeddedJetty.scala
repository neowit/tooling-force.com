package com.neowit.webserver

import org.eclipse.jetty.server.{Handler, Server}

class EmbeddedJetty(port: Int,
                    handler: Handler
                   ) {

    val server = new Server(port)
    server.setStopAtShutdown(true)

    server.setHandler(handler)

    server.start()
    //server.join()

    def isAlive: Boolean = server.isRunning || server.isStarting
    def stop(): Unit =
        server.stop()
}


