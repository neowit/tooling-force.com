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
