package com.neowit.utils

import scala.util.parsing.json.JSONObject
import java.net.{NetworkInterface, InetAddress, URL}
import java.io.OutputStreamWriter

class UsageReporter(basicConfig: BasicConfig) extends Logging {
    private def encodePostData(data: Map[String, String]) = JSONObject(data).toString()

    def getIp = {
        val u = new URL("http://ip.42.pl/raw")
        val conn = u.openConnection()
        //conn.setConnectTimeout(HttpRequestTimeout)
        conn.connect()

        val ip: String =
            try {
                io.Source.fromInputStream(conn.getInputStream).mkString
            } catch {
                case ex:Throwable => "unknown"
            }
        ip
    }
    private def reportUsage() {
        val localAddress = InetAddress.getLocalHost
        val ni = NetworkInterface.getByInetAddress( localAddress )
        val mac = ni.getHardwareAddress.map("%02X" format _).mkString("-")

        val data = Map(
            "action" -> basicConfig.action,
            "ip" -> getIp,
            "mac" -> mac,
            "os" -> System.getProperty("os.name")

        )
        logger.trace("usage report: " + data)
        val url = new URL("https://usage-developer-edition.eu0.force.com/services/apexrest/usage")
        val conn = url.openConnection
        conn.setConnectTimeout(0)
        conn.setRequestProperty("Content-Type", "application/json")
        //conn.setConnectTimeout(HttpRequestTimeout)
        conn.setDoOutput(true)
        conn.connect()
        val wr = new OutputStreamWriter(conn.getOutputStream)
        wr.write(encodePostData(data))
        wr.flush()
        wr.close()
        val in = conn.getInputStream
        logger.trace("usage service responded" + io.Source.fromInputStream( in ).mkString(""))
        in.close()
    }
    def report() {
        if (0 != basicConfig.getProperty("u").getOrElse(1)) {
            try {
                reportUsage()
            } catch {
                case x: Throwable => //usage reports are not important enough to do anything about them
            }
        }
    }

}
