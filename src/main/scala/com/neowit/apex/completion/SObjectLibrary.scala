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

package com.neowit.apex.completion

import com.neowit.apex.Session
import com.neowit.apex.parser.Member
import com.neowit.apexscanner.ast.QualifiedName
import com.neowit.apexscanner.{Project, symbols}
import com.neowit.apexscanner.extlib.CodeLibrary
import com.neowit.apexscanner.nodes._
import com.neowit.apexscanner.symbols.SymbolKind

/**
  * Created by Andrey Gavrikov 
  */
class SObjectLibrary(session: Session) extends CodeLibrary {
    private var _isLoaded = false
    override def getName: String = "SObjectLibrary"

    override def load(project: Project): CodeLibrary = {
        DatabaseModel.getModelBySession(session) match {
            case Some(dbModel) =>
                dbModel.getSObjectMembers.foreach(m => addByQualifiedName(sObjectToAstNode(m)))
                _isLoaded = true
            case None =>
        }
        this
    }

    override def isLoaded: Boolean = _isLoaded

    private def sObjectToAstNode(m: Member): AstNode with HasQualifiedName = {
        val node = m match {
            case mm:SObjectMember => sObjectMemberToAstNode(mm)
            case mm:SObjectRelationshipFieldMember => sObjectRelationshipFieldMemberToAstNode(mm)
            case mm:SObjectFieldMember => sObjectFieldMemberToAstNode(mm)
        }
        node
    }

   private def sObjectMemberToAstNode(sobject: SObjectMember): AstNode with HasQualifiedName = {
       val cls = SObjectNode(sobject)

       cls.addChildToAst(IdentifierNode(sobject.getIdentity, Range.INVALID_LOCATION))

       /*
       sobject.getChildren.foreach{childMember =>
           val node = sObjectToAstNode(childMember)
           cls.addChildToAst(node)
       }
       */
       cls
   }
    private def sObjectFieldMemberToAstNode(m: SObjectFieldMember): AstNode with HasQualifiedName = {
        val n = SObjectFieldNode(Option(m.getIdentity), ValueTypeSimple(QualifiedName(Array(m.getType))))
        n
    }
    private def sObjectRelationshipFieldMemberToAstNode(relField: SObjectRelationshipFieldMember): AstNode with HasQualifiedName = {
        val cls = SObjectNode(relField)
        cls.addChildToAst(IdentifierNode(relField.getIdentity, Range.INVALID_LOCATION))

        /*
        relField.getChildren.foreach{childMember =>
            val node = sObjectToAstNode(childMember)
            cls.addChildToAst(node)
        }
        */
        cls
    }

    private case class SObjectNode(sobject: DatabaseModelMember) extends ClassLike {
        override def symbolKind: SymbolKind = SymbolKind.Class

        override def getValueType: Option[ValueType] = Option(ValueTypeClass(QualifiedName(Array(sobject.getIdentity))))

        override def range: Range = Range.INVALID_LOCATION

        override def nodeType: AstNodeType = ClassNodeType

        override protected def resolveDefinitionImpl(): Option[AstNode] = Option(this)


        override def name: Option[String] = Option(sobject.getIdentity)

        override def children: Seq[AstNode] = {
            if (!sobject.isLoaded) {
                sobject.getChildren.foreach{childMember =>
                    val node = sObjectToAstNode(childMember)
                    this.addChildToAst(node)
                }
            }
            super.children
        }
    }

    private case class SObjectFieldNode(override val name: Option[String], valueType: ValueType) extends VariableLike with ClassOrInterfaceBodyMember { self =>
        override def getClassOrInterfaceNode: ClassLike = {
            findParentInAst(p => p.nodeType == ClassNodeType || p.nodeType == InterfaceNodeType ) match {
                case Some(n: ClassLike) => n
                case n => throw new NotImplementedError("getClassOrInterfaceNode support for this element is not implemented: " + n)
            }
        }

        override protected def getSelf: AstNode = self

        override def range: Range = Range.INVALID_LOCATION

        override def nodeType: AstNodeType = ClassVariableNodeType

        override protected def resolveDefinitionImpl(): Option[AstNode] = Option(this)

        override def symbolName: String = name.getOrElse("")

        override def symbolKind: SymbolKind = SymbolKind.Field

        override def parentSymbol: Option[symbols.Symbol] = Option(getClassOrInterfaceNode)

        override def symbolIsStatic: Boolean = modifiers.exists(_.modifierType == ModifierNode.STATIC)

        override def symbolValueType: Option[String] = getValueType.map(_.qualifiedName.toString)



        override def getValueType: Option[ValueType] = Option(valueType)

        override def qualifiedName: Option[QualifiedName] = {
            name match {
                case Some(_name) =>
                    getClassOrInterfaceNode.qualifiedName match {
                        case Some(className) => Option(QualifiedName(className.components ++ Array(_name)))
                        case None => None
                    }
                case None => None
            }
        }
    }
}
