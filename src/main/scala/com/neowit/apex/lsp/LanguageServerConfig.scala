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

package com.neowit.apex.lsp

import com.neowit.apex.AppVersion
import scopt.OptionParser

/**
  * Created by Andrey Gavrikov 
  */
case class LanguageServerConfig(
                                   communicationMethod:String = "socket",
                                   host:String = "localhost",
                                   port:Int = -1,
                                   orgAlias: String = "",
                                   cliPath: String = ""
                               )

object LanguageServerConfig {
    // configure
    val parser: OptionParser[LanguageServerConfig] = new scopt.OptionParser[LanguageServerConfig]("LanguageServerLauncher") {
        head(AppVersion.APP_NAME, AppVersion.VERSION)

        opt[String]('c', "communicationMethod").action( (x, c) =>
            c.copy(communicationMethod = x) ).text("Connection Type - socket | stdio")
        opt[String]('h', "host").action( (x, c) =>
            c.copy(host = x) ).text("Host address. Ignored if Connection Type is not socket")
        opt[Int]('p', "port").action( (x, c) =>
            c.copy(port = x) ).text("Socket Port. Ignored if Connection Type is not socket")
            //.required()
            .validate(p =>
                if (p > 0 && p <= 65535) success else failure("invalid 'port' ")
            )
        opt[String]("orgAlias").action( (x, c) =>
            c.copy(orgAlias = x) )
            .text("Salesforce CLI org alias")
            .validate(p => if (p.nonEmpty) success else failure("'orgAlias' must not be empty"))
        opt[String]("cliPath").action( (x, c) =>
            c.copy(cliPath = x) ).text("Salesforce CLI executable path")

    }

    def parse(args: Array[String], init: LanguageServerConfig): Option[LanguageServerConfig] = {
       parser.parse(args, init)
    }
}
