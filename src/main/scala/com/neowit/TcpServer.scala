package com.neowit


import java.net._
import java.io._

import akka.actor.Actor
import akka.actor.ActorSystem
import akka.actor.Props
import com.neowit.apex.actions.{AsyncAction}
import com.neowit.utils.BasicConfig

case class Message(socket: Socket)
case class Ping(socket: Socket)
case class Shutdown(socket: Socket)
case class Command(socket: Socket, params: Map[String, Any])

//this is for testing only
object MyServer extends App{
    val server = new TcpServer(8888, 30000)
    server.start()
}

class ServerStart (basicConfig: BasicConfig) extends AsyncAction(basicConfig: BasicConfig) {
    override def act(): Unit = {
        val port = basicConfig.getProperty("port").getOrElse("8888").toInt
        val timeoutMillis = basicConfig.getProperty("timeoutMillis").getOrElse("30000").toInt
        val server = new TcpServer(port, timeoutMillis)
        server.start()
    }

    override def getParamNames: List[String] = List("port", "timeoutMillis")

    override def getSummary: String = "start server on specified local port"

    override def getName: String = "serverStart"

    override def getExample: String = "tooling-force.com.jar --action=serverStart --port=8888 --timeoutMillis=30000"

    override def getParamDescription(paramName: String): String = {
        paramName match {
            case "port" => "Port number, e.g. 8888"
            case "timeoutMillis" =>
                """Number of milliseconds the server will wait new connection.
                  |Once last command is completed and if no new connections is established within 'timeoutMillis'
                  |the server will shut itself down""".stripMargin
            case _ => "unsupported parameter"
        }
    }
}

object TcpServer {
    val system = ActorSystem("TcpServerSystem")
    var isShutdownReceived = false
    def shutdown() {
        system.shutdown()
        println("shutdown ActorSystem")
    }
}

class TcpServer (port: Int, timeoutMillis: Int){
    val serverSocket = new ServerSocket(port)

    def start() {
        var count = 0
        while (!TcpServer.isShutdownReceived) {
            if (!TcpServer.isShutdownReceived) {
                println("Awaiting connection...")
                ///wait until a client connects to the socket or timeout is reached
                serverSocket.setSoTimeout(timeoutMillis) //in milliseconds
            }
            try {
                val clientSocket = serverSocket.accept()
                if (!TcpServer.isShutdownReceived) {
                    // default Actor constructor
                    //val helloActor = system.actorOf(Props[EchoActor], name = "helloactor")
                    val handlerActor = TcpServer.system.actorOf(Props[CommandParser])
                    //helloActor ! "hello"
                    handlerActor ! new Message(clientSocket)
                    count = count + 1
                } else {
                    TcpServer.shutdown()
                }
            } catch {
                case ex: java.net.SocketTimeoutException =>
                    serverSocket.close()
                    TcpServer.isShutdownReceived = true
                    println("accept() - reached timeout, closed socket.")
                    TcpServer.shutdown()
            }
            //scala.sys.exit(0) //this causes exception when run with: sbt run
        }
    }
}

class CommandParser extends Actor {
    def processCommand(socket: Socket) {

        val out = new PrintWriter(socket.getOutputStream, true) //use to communicate back to the client
        //val in = new BufferedReader(new InputStreamReader(socket.getInputStream))
        println("Client connected from " + socket.getInetAddress + ":" + socket.getPort)
        //parse command
        //io.Source.fromInputStream(socket.getInputStream).getLines().foreach(str => println("line=" + str))
        val inputLines = io.Source.fromInputStream(socket.getInputStream).getLines().toList
        if (!inputLines.isEmpty) {
            val processorActor = TcpServer.system.actorOf(Props[CommandProcessor])
            inputLines(0) match {
                case "ping" => processorActor ! new Ping(socket)
                case "shutdown" => processorActor ! new Shutdown(socket)
                case _ => new Command(socket, Map("commandLine" -> inputLines(0)))
            }
        }
        //parser actor is no longer needed, stop it
        context.stop(self)
    }

    def receive = {
        case Message(socket) => processCommand(socket)
        case _ => println("huh?")
    }
}

class CommandProcessor extends Actor {
    def receive = {
        case Ping(socket) => ping(socket)
        case Shutdown(socket) => shutdown(socket)
        case Command(socket, params) => processCommand(socket, params)
        case _ => println("huh?")
    }

    /**
     * must be called as the very last call of current Actor, when everything is done
     * @param socket
     */
    def done(socket: Socket) {
        println("Disconnect " + socket.getInetAddress + ":" + socket.getPort)
        socket.close()
        context.stop(self)
    }
    /**
     * ping command checks if server is On and keeps it alive (i.e. resets accept() timeout)
     * @param socket
     */
    def ping(socket: Socket) {
        val out = new PrintWriter(socket.getOutputStream, true)
        out.println("pong back at you")
        out.close()
        done(socket)
    }

    def shutdown(socket: Socket) {
        val out = new PrintWriter(socket.getOutputStream, true)
        out.println("server is shutting down")
        println("server is shutting down")
        out.close()
        TcpServer.isShutdownReceived = true
        val clientSocket = new Socket(socket.getLocalAddress, socket.getLocalPort)
        clientSocket.close()
        done(socket)
    }
    /**
     * generic command
     * @param socket
     * @param params
     */
    def processCommand(socket: Socket, params: Map[String, Any]) {
        println("received command: " + params("commandLine"))

        val out = new PrintWriter(socket.getOutputStream, true)
        out.println("your command have been processed")
        out.close()
        done(socket)
    }

}


