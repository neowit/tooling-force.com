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

package com.neowit.apex.actions.tooling

import com.neowit.apex.completion.models.ApexMethod
import spray.json.DefaultJsonProtocol

/**
  * Author: Andrey Gavrikov (westbrook)
  * Date: 25/05/2016
  */
trait StandardApexModelJson extends DefaultJsonProtocol {
    implicit val ApexStandardParameterFormat = jsonFormat2(ApexStandardParameter)
    implicit val ApexStandardPropertyFormat = jsonFormat2(ApexStandardProperty)
    implicit val ApexStandardMethodFormat = jsonFormat6(ApexStandardMethod)
    implicit val ApexStandardConstructorFormat = jsonFormat3(ApexStandardConstructor)
    implicit val ApexStandardMemberFormat = jsonFormat3(ApexStandardNamespaceClass)
    implicit val ApexJsonFormat = jsonFormat1(ApexJson)

}

// "Apex" -> Map[EmptyStackException, ApexStandardNamespaceClass]
case class ApexJson(publicDeclarations: Map[String, Map[String, ApexStandardNamespaceClass]])
// "EmptyStackException", "System", "Database"
case class ApexStandardNamespaceClass(constructors: List[ApexStandardConstructor], methods: List[ApexStandardMethod], properties: List[ApexStandardProperty])

case class ApexStandardConstructor(name: Option[String], parameters: List[ApexStandardParameter], references: List[String]) {
    def toApexCtor(className: String): ApexMethod = {
        ApexMethod(
            s = "0",
            n = name.getOrElse(className),
            v = "public",
            p = parameters.map(_p => _p.`type` + ": " + _p.name),
            h = "", // SFDC does not provide help text at the moment
            r = "",
            d = getDefinition(className)
        )
    }
    private def getDefinition(className: String): String = {
        val visibility = "public " // SFDC only publishes public methods
        val params = parameters.map(_p => _p.`type` + ": " + _p.name).mkString(", ")

        visibility + " " + name.getOrElse(className) + "(" + params + ")"
    }

}

case class ApexStandardMethod(argTypes: List[String], isStatic: Boolean, name: String, parameters: List[ApexStandardParameter], references: List[String], returnType: String) {
    def toApexMethod: ApexMethod = {
        ApexMethod(
            s = if (isStatic) "1" else "0",
            n = name,
            v = "public",
            p = parameters.map(_p => _p.`type`),
            h = "", // SFDC does not provide help text at the moment
            r = returnType,
            d = getDefinition
        )
    }
    private def getDefinition: String = {
        val _static = if (isStatic) "static " else ""
        val visibility = "public " // SFDC only publishes public methods
        val params = parameters.map(_p => _p.`type` + " " + _p.name).mkString(", ")

        _static + visibility + returnType + " " + name + "(" +params+ ")"
    }

}
case class ApexStandardProperty(name: String, references: List[String])
case class ApexStandardParameter(name: String, `type`: String)

