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

package com.neowit.apex.completion.models

import com.neowit.apex.parser.Member
import com.neowit.apexscanner.symbols.SymbolKind
import spray.json.{DefaultJsonProtocol, JsValue, JsonFormat}

object SoqlModelJsonProtocol extends DefaultJsonProtocol {
    implicit val soqlTypeFormat: JsonFormat[SoqlType] = lazyFormat(jsonFormat(SoqlType, "name", "methods", "tag", "fqn"))
    implicit val soqlFunction: JsonFormat[SoqlFunction] = lazyFormat(jsonFormat(SoqlFunction, "n", "v", "p", "h", "r", "d", "scope"))
}
object SoqlModel extends ModelBase {

    private val NAMESPACES = List("SOQL" )

    private val memberByNamespace: Map[String, ApexModelMember] = load()

    override def getNameSpaces: List[String] = NAMESPACES


    override def getNamespaceInstance(namespace: String): GenericNamespace = new SoqlNamespace(namespace)

    override def getMemberByNamespace: Map[String, ApexModelMember] = memberByNamespace

    def getMember(fqn: String): Option[Member] = {
        if (fqn.indexOf(".") > 0) {
            //this is probably a fully qualified name
            val types = getMembers(fqn.split("\\.")(0))

            types.find(_.getSignature.toLowerCase == fqn.toLowerCase) match {
                case Some(member) => Some(member)
                case None => None
            }
        } else {
            //add namespace name
            getMember("SOQL." + fqn)
        }
    }
}

class SoqlNamespace(name: String) extends GenericNamespace(name) {
    import com.neowit.apex.completion.models.SoqlModelJsonProtocol._

    private var isDoneLoading = false
    override def isLoaded:Boolean = isDoneLoading
    override def loadMembers(): Unit = {
        isDoneLoading = true //must do it here because loadFile calls getChild and isDoneLoading = false causes infinite loop
        super.loadMembers()
        loadFile("hand-made/SOQL")
    }

    override protected def loadTypes(types: Map[String, JsValue], overwriteChildren: Boolean): Unit = {

        for (typeName <- types.keys) {
            val typeJson = types(typeName)
            //println("typeName=" + typeName)
            val apexModelMember = typeJson.convertTo[SoqlType]

            apexModelMember.setParent(this)
            addChild(apexModelMember, overwriteChildren)

        }
    }
    override def getKind: Option[SymbolKind] = Option(SymbolKind.Namespace)
}

case class SoqlType(name: String,  methods: Option[List[SoqlFunction]], tag: String, fqn: String) extends ApexModelMember {

    override def getIdentity: String = name
    override def getSignature: String = fqn
    override def isStatic: Boolean = false

    private var isDoneLoading = false
    override def isLoaded:Boolean = isDoneLoading
    override def loadMembers(): Unit = {
        isDoneLoading = true //must do it here because loadFile calls getChild and isDoneLoading = false causes infinite loop
        methods match {
            case Some(_methods) =>
                for (method <- _methods) {
                    //method.setParent(this)
                    addChild(method)
                }
            case None =>
        }
    }
    override def getKind: Option[SymbolKind] = Option(SymbolKind.SObject)
}

case class SoqlFunction(n: String, v: String, p: List[String], h: String, r: String, d: String, scope: List[String]) extends ApexModelMember {

    override def getIdentity: String = n + p.mkString(", ")

    /**
     * for most member types Identity is unique (for Methods, SOQL functions, and Inner Classes it is not)
     */
    override def getIdentityToDisplay: String = n
    override def getKind: Option[SymbolKind] = Option(SymbolKind.Function)
    override def getSignature: String = d
    override def isStatic: Boolean = true
    override def getDoc: String = h

    override def getType: String = r

    def isInScope(scopeName: String): Boolean =
        scope.map(_.toLowerCase).contains(scopeName.toLowerCase)


    private var isDoneLoading = false
    override def isLoaded:Boolean = isDoneLoading
    override def loadMembers(): Unit = {
        isDoneLoading = true //must do it here because loadFile calls getChild and isDoneLoading = false causes infinite loop
        super.loadMembers()
    }
}
