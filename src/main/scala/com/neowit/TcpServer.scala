package com.neowit


import java.net._
import java.io._

import akka.actor.Actor
import akka.actor.ActorSystem
import akka.actor.Props
import com.neowit.apex.actions.{ActionHelp, AsyncAction}
import com.neowit.apex.Executor

case class Message(socket: Socket)
case class Ping(socket: Socket)
case class Shutdown(socket: Socket)
case class Command(socket: Socket, commandLine: String)

//this is for testing only
object MyServer extends App{
    val server = new TcpServer(8888, 30000)
    server.start()
}

class ServerStart  extends AsyncAction {
    override protected def act(): Unit = {
        val port = basicConfig.getProperty("port").getOrElse("8888").toInt
        val timeoutMillis = basicConfig.getProperty("timeoutSec").getOrElse("30").toInt * 1000
        val server = new TcpServer(port, timeoutMillis)
        server.start()
    }
    override def getHelp: ActionHelp = new ActionHelp {
        override def getParamNames: List[String] = List("port", "timeoutMillis")

        override def getSummary: String = "start server on specified local port"

        override def getName: String = "serverStart"

        override def getExample: String = "tooling-force.com.jar --action=serverStart --port=8888 --timeoutMillis=30000"

        override def getParamDescription(paramName: String): String = {
            paramName match {
                case "port" => "--port - Port number, e.g. 8888"
                case "timeoutSec" =>
                    """--timeoutSec - Number of seconds the server will wait for new connections.
                  |Once last command is completed and if no new connections is established within 'timeoutMillis'
                  |the server will shut itself down""".stripMargin
            case _ => ""
        }
        }
    }

    //implement if need to execute some logic only after main action is complete, e.g. persist data to disk
    override protected def finalise(): Unit = {}
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
        if (inputLines.nonEmpty) {
            val processorActor = TcpServer.system.actorOf(Props[CommandProcessor])
            inputLines(0) match {
                case "ping" => processorActor ! new Ping(socket)
                case "shutdown" => processorActor ! new Shutdown(socket)
                case _ => processorActor ! new Command(socket, inputLines(0))
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
        case Command(socket, commandLine) => processCommand(socket, commandLine)
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
     * @param commandLine
     */
    def processCommand(socket: Socket, commandLine: String) {
        println("received command: " + commandLine)

        val commandLineArgs:Array[String] = commandLine match {
            case x if !x.isEmpty =>
              //command line looks like: --key2="value 1" --key2=value2 --key3="value 33"
              val args1 = x.trim.toString.split("--").filterNot(_.isEmpty).map(s => "--" + s.trim)
              //args1: Array("--key2="value 1"", "--key2=value2", "--key3='value 33'")
              //need to get rid of quotes around values with spaces
              args1.map(s => {
                  val two = s.split('=')
                  two(0) + "=" + removeQuotes(removeQuotes(two(1), '"'), '\'')
              })
          case _ => Array()
        }

        val out = new PrintStream(socket.getOutputStream, true)
        //redirect system out and err to show messages on the client rather than server
        System.setOut(out)
        System.setErr(out)

        val runner = new Executor()
        runner.execute(commandLineArgs)

        //out.println("your command have been processed")
        out.close()
        done(socket)
    }

    /**
     * convert
     * --key2="value 1"
     * into
     * --key2=value 1
     * @param str - key="value" string
     * @return
     */
    private def removeQuotes(str: String, quote: Char) = {
        val left = if (!str.isEmpty && quote == str(0)) str.substring(1) else str
        val right = if (!left.isEmpty && quote == left(left.length-1)) left.substring(0, left.length-1) else left
        right
    }
}


