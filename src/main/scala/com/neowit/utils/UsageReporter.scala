package com.neowit.utils

import com.neowit.apex.actions.AppVersion

import scala.util.parsing.json.JSONObject
import java.net.{NetworkInterface, InetAddress, URL}
import java.io.OutputStreamWriter
import java.security.MessageDigest

/**
 * this class sends anonymised usage statistics
 * @param basicConfig
 */
class UsageReporter(basicConfig: BasicConfig) extends Logging {
    private def encodePostData(data: Map[String, String]) = JSONObject(data).toString()

    private def reportUsage() {
        val localAddress = InetAddress.getLocalHost
        val ni = NetworkInterface.getByInetAddress( localAddress )
        val hardwareAddress = ni.getHardwareAddress
        //get hash of mac address
        val macHash = if (null != hardwareAddress) {
            val mac = hardwareAddress.map("%02X" format _).mkString("-")
            val md5 = MessageDigest.getInstance("MD5")
            md5.digest(mac.getBytes).mkString
        } else {
            "unknown"
        }

        val data = Map(
            "action" -> basicConfig.action,
            "macHash" -> macHash,
            "os" -> System.getProperty("os.name"),
            "version" -> AppVersion.VERSION
        )
        logger.trace("usage report: " + data)

        setProxy() //set proxy if defined
        val url = new URL("https://usage-developer-edition.eu0.force.com/services/apexrest/usage")
        val conn = url.openConnection
        //conn.setConnectTimeout(0)
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
        if ("0" != basicConfig.getProperty("reportUsage").getOrElse("1")) {
            try {
                reportUsage()
            } catch {
                case x: Throwable => //usage reports are not important enough to do anything about them
            }
        }
    }

    private def setProxy() {
        val proxyHost = basicConfig.getProperty("http.proxyHost")
        val proxyPort = basicConfig.getProperty("http.proxyPort")

        if (None != proxyHost && None != proxyPort) {
            System.setProperty("https.proxyHost", proxyHost.get)
            System.setProperty("https.proxyPort", proxyPort.get)
        }
        val proxyUsername = basicConfig.getProperty("http.proxyUsername") match {
            case Some(s) => Some(s)
            case None => basicConfig.getProperty("http.proxyUser")
        }
        if (None != proxyUsername )
            System.setProperty("https.proxyUser", proxyUsername.get)

        val proxyPassword = basicConfig.getProperty("http.proxyPassword")
        if (None != proxyPassword )
            System.setProperty("https.proxyPassword", proxyPassword.get)
    }

}
