package com.neowit.utils

/**
  * Author: Andrey Gavrikov
  */
object OsUtils {

    sealed trait OS
    case object Mac extends OS
    case object Win extends OS
    case object Linux extends OS
    case object Unknown extends OS

    // inspired by stackoverflow answer:
    // http://stackoverflow.com/questions/5226212/how-to-open-the-default-webbrowser-using-java
    def openUrl(url: String, unknownOsUrl: String): Unit = {
        getOs match {
            case Win =>
                openUrlInWindows(url)
            case Mac =>
                openUrlInMac(url)
            case Linux =>
                openUrlInLinux(url)
            case Unknown =>
                println("Open this URL in your web browser: " + unknownOsUrl)
        }
    }

    def getOs: OS = {
        val os = System.getProperty("os.name").toLowerCase
        if (null != os) {
            if (os.indexOf("win") >=0) {
                return Win
            } else if (os.indexOf( "mac" ) >= 0) {
                return Mac

            } else if (os.indexOf( "nix") >=0 || os.indexOf( "nux") >=0) {
                return Linux
            }
        }
        Unknown

    }

    private def openUrlInWindows(url: String): Unit = {
        val rt = Runtime.getRuntime
        rt.exec("rundll32 url.dll,FileProtocolHandler " + url)
    }

    private def openUrlInLinux(url: String): Unit = {
        val rt = Runtime.getRuntime
        val browsers = Seq("epiphany", "firefox", "mozilla", "konqueror", "netscape", "opera", "links", "lynx")
        val cmd = new StringBuffer()
        for (i <- browsers.indices) {
            cmd.append(
                (if (i == 0) "" else " || ") + browsers(i) + " \"" + url + "\" "
            )
        }
        rt.exec(Array("sh", "-c", cmd.toString))
    }

    private def openUrlInMac(url: String): Unit = {
        val rt = Runtime.getRuntime
        rt.exec(Array("open", url))
    }
}
