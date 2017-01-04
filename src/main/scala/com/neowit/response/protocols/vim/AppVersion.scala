/*
 *
 *  * Copyright (c) 2017 Andrey Gavrikov.
 *  * this file is part of tooling-force.com application
 *  * https://github.com/neowit/tooling-force.com
 *  *
 *  * This program is free software: you can redistribute it and/or modify
 *  * it under the terms of the GNU Lesser General Public License as published by
 *  * the Free Software Foundation, either version 3 of the License, or
 *  * (at your option) any later version.
 *  *
 *  * This program is distributed in the hope that it will be useful,
 *  * but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  * GNU Lesser General Public License for more details.
 *  *
 *  * You should have received a copy of the GNU Lesser General Public License
 *  * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 */

package com.neowit.response.protocols.vim

import com.neowit.response.{AppVersionResult, InfoMessage, MessageDetailMap}

class AppVersion(writer: ResponseWriterVim) extends VimProtocol[AppVersionResult] {
    def send(result: AppVersionResult): Unit = {
        val versionMessage = writer.send(InfoMessage(result.appName + " - version: " + result.appVersion + "; SFDC API Version: " + result.sfdcApiVersion))
        val mb = 1024*1024
        val runtime = Runtime.getRuntime
        writer.println( MessageDetailMap(versionMessage, Map("type" -> "DEBUG", "text" -> s"Used Memory: ${(runtime.totalMemory - runtime.freeMemory) / mb} MB")))
        writer.println( MessageDetailMap(versionMessage, Map("type" -> "DEBUG", "text" -> s"Free Memory: ${runtime.freeMemory / mb} MB")))
        writer.println( MessageDetailMap(versionMessage, Map("type" -> "DEBUG", "text" -> s"Total Memory: ${runtime.totalMemory / mb} MB")))
        writer.println( MessageDetailMap(versionMessage, Map("type" -> "DEBUG", "text" -> s"Max Memory: ${runtime.maxMemory / mb} MB")))
    }
}
