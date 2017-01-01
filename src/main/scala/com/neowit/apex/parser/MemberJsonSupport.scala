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

package com.neowit.apex.parser

import java.nio.file.Path

import org.antlr.v4.runtime.ParserRuleContext
import spray.json._

/**
  * Author: Andrey Gavrikov
  * Date: 22/11/2016
  */
object MemberJsonSupport {

    case class Location ( file: Path, line: Option[Int], column: Option[Int], identity: String )

    object Location extends DefaultJsonProtocol {
        def apply(filePath: Path, ctx: ParserRuleContext, identity: String): Option[Location] = {
            Option(Location(filePath, Option(ctx.getStart.getLine), Option(ctx.getStart.getCharPositionInLine), identity))
        }

        implicit object LocationJsonFormat extends RootJsonFormat[Location] {
            def write(c: Location) =
                Map(
                    "filePath" -> c.file.toString.toJson,
                    "line" -> c.line.getOrElse(-1).toJson,
                    "column" -> c.column.getOrElse(-1).toJson,
                    "identity" -> c.identity.toJson
                ).toJson

            def read(value: JsValue) = value match {
                case _ => deserializationError("Location deserialisation is NOT supported")
            }
        }
    }
}
