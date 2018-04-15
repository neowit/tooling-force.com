/*
 * Copyright (c) 2018 Andrey Gavrikov.
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

package com.neowit.response.protocols.lsp

import com.neowit.apex.actions.ActionSuccess
import com.neowit.response._

class AppVersion(writer: ResponseWriterLsp) extends LspProtocol[AppVersionResult] {
    def send(result: AppVersionResult): Unit = {
        val msg =
            result.appName + " - version: " + result.appVersion +
                "; SFDC API Version: " + result.sfdcApiVersion +
                "; java: " + result.javaVersion +
                "; OS: " + result.os

        writer.sendResponse(ActionSuccess(msg))

        val mb = 1024*1024
        val runtime = Runtime.getRuntime

        writer.debug( s"Used Memory: ${(runtime.totalMemory - runtime.freeMemory) / mb} MB")
        writer.debug( s"Free Memory: ${runtime.freeMemory / mb} MB")
        writer.debug( s"Total Memory: ${runtime.totalMemory / mb} MB")
        writer.debug( s"Max Memory: ${runtime.maxMemory / mb} MB")
    }
}
