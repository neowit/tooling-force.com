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
    /**
      * attempts to open given url in web browser
      * @param url url to open
      * @return true if known/supported OS
      */
    def openUrl(url: String): Boolean = {
        val commandsOpt =
            getOs match {
                case Win =>
                    Option(Array("cmd", "/c", "start", url.replaceAll("&", "^&")))
                case Mac =>
                    Option(Array("open", url))
                case Linux =>
                    Option(Array("xdg-open", url))
                case Unknown =>
                    None
            }
        commandsOpt match {
            case Some(commands) =>
                val rt = Runtime.getRuntime
                rt.exec(commands)
                true
            case None =>
                false
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
}
