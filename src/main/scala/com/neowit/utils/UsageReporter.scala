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

package com.neowit.utils

import com.neowit.apex.{Connection, AppVersion}

import spray.json._
import DefaultJsonProtocol._
import java.io.OutputStreamWriter
import java.security.MessageDigest
import java.net._

/**
 * this class sends anonymised usage statistics
 * @param basicConfig - application config
 */
class UsageReporter(basicConfig: BasicConfig) extends Logging {
    private def encodePostData(data: Map[String, String]) = data.toJson.toString()

    private def reportUsage(): Unit = {
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

        val proxyType = basicConfig.getProperty("http.proxyHost") match {
            case Some(x) => basicConfig.getProperty("http.proxyUsername") match {
                case Some(y) => "Authenticated"
                case None => "Un-authenticated"
            }
            case None => "None"
        }

        val data = Map(
            "action" -> basicConfig.action,
            "macHash" -> macHash,
            "os" -> System.getProperty("os.name"),
            "version" -> AppVersion.VERSION,
            "proxyType" -> proxyType
        )
        logger.trace("usage report: " + data)

        val url = new URL("https://usage-developer-edition.eu9.force.com/services/apexrest/usage")
        val connectionConfig = Connection.initConnectorConfig(basicConfig)
        val conn = connectionConfig.createConnection(url, new java.util.HashMap[String, String](), false)
        //conn.setConnectTimeout(0)
        conn.setRequestProperty("Content-Type", "application/json")
        conn.setRequestMethod("POST")
        conn.setDoOutput(true)
        conn.setDoInput(true)
        conn.connect()
        val wr = new OutputStreamWriter(conn.getOutputStream)
        wr.write(encodePostData(data))
        wr.flush()
        wr.close()
        val in = conn.getInputStream
        logger.trace("usage service responded: " + io.Source.fromInputStream( in ).mkString(""))
        in.close()
    }
    def report(): Unit = {
        if ("0" != basicConfig.getProperty("reportUsage").getOrElse("1")) {
            try {
                reportUsage()
            } catch {
                case x: Throwable => //usage reports are not important enough to do anything about them
                    logger.trace("", x)
            }
        }
    }
}
