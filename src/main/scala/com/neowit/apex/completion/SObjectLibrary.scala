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
import com.neowit.apexscanner.nodes.soql.SoqlQueryNode
import com.neowit.apexscanner.symbols.SymbolKind

/**
  * Created by Andrey Gavrikov 
  */
class SObjectLibrary(session: Session) extends CodeLibrary with AstNode with IsTypeDefinition {
    private var _isLoaded = false
    override def getName: String = SoqlQueryNode.LIBRARY_NAME
    private val _sobjectNodes = new collection.mutable.ListBuffer[AstNode]

    override def load(project: Project): CodeLibrary = {
        DatabaseModel.getModelBySession(session) match {
            case Some(dbModel) =>
                dbModel.getSObjectMembers.foreach{m =>
                    val node = sObjectToAstNode(m)
                    _sobjectNodes += node
                    addByQualifiedName(node)
                }
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
       val cls = SObjectNode(this, sobject)

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
        val cls = SObjectRelationshipFieldNode(this, relField)
        cls.addChildToAst(IdentifierNode(relField.getIdentity, Range.INVALID_LOCATION))

        /*
        relField.getChildren.foreach{childMember =>
            val node = sObjectToAstNode(childMember)
            cls.addChildToAst(node)
        }
        */
        cls
    }


    //////////////////////////////////////////////////////////////////////////////////////////////////
    override def range: Range = Range.INVALID_LOCATION
    override def nodeType: AstNodeType = InterfaceNodeType

    //override standard AstNode.findChildrenInAst because we need
    override def findChildrenInAst(filter: (AstNode) => Boolean, recursively: Boolean = false): Seq[AstNode] = {
        _sobjectNodes.filter(filter)
    }

    override def getValueType: Option[ValueType] = Option(ValueTypeSimple(QualifiedName(getName)))

    override def qualifiedName: Option[QualifiedName] = Option(QualifiedName(getName))

    override protected def resolveDefinitionImpl(): Option[AstNode] = Option(this)

    // override standard getByQualifiedName in order to allow retrieval by name: "SObjectLibrary"
    override def getByQualifiedName(qualifiedName: QualifiedName): Option[AstNode] = {
        if (1 == qualifiedName.length && qualifiedName.getFirstComponent == getName) {
            // requested whole SObjectLibrary
            Option(this)
        } else {
            super.getByQualifiedName(qualifiedName)
        }
    }
    //////////////////////////////////////////////////////////////////////////////////////////////////

    private abstract class SObjectNodeBase(library: SObjectLibrary, sobject: DatabaseModelMember) extends ClassLike {
        override def symbolKind: SymbolKind = SymbolKind.Class

        override def getValueType: Option[ValueType] = Option(ValueTypeClass(QualifiedName(Array(sobject.getType))))

        override def range: Range = Range.INVALID_LOCATION

        override def nodeType: AstNodeType = ClassNodeType

        override protected def resolveDefinitionImpl(): Option[AstNode] = Option(this)

        override def supportsInnerClasses: Boolean = false

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

        override def symbolIsStatic: Boolean = false
    }

    private case class SObjectNode(library: SObjectLibrary, sobject: SObjectMember) extends SObjectNodeBase(library, sobject) {
        // Child relationships will be recorded inside AstNode: SObjectChildRelationshipNode
        // SObjectRelationshipsContainerNode is not a Symbol (so can not be returned as part of ListCompletions)
        // SObjectRelationshipsContainerNode has QualifiedName like (Account._Relationships)
        override def children: Seq[AstNode] = {
            if (!sobject.isLoaded) {
                super.children // force children load and get added to AST
                /*
                sobject.getChildRelationships.foreach{childRelationship =>
                    val relName = childRelationship.getRelationshipName
                    if (null != relName && !relName.isEmpty) {
                        val sObjectMember = new SObjectMember(childRelationship.getChildSObject, sobject.session)
                        val node = SObjectChildRelationshipNode(library, this, childRelationship.getRelationshipName, sObjectMember)
                        this.addChildToAst(node)
                    }

                }
                */
            }
            super.children
        }
    }

    private case class SObjectChildRelationshipNode(library: SObjectLibrary, sobjectNode: SObjectNode,
                                                    relationshipName: String, childSObject: SObjectMember)
                                                    extends AstNode with IsTypeDefinition {

        override def qualifiedName: Option[QualifiedName] = sobjectNode.qualifiedName.map(parentQName => QualifiedName(parentQName, relationshipName))

        override def getValueType: Option[ValueType] = Option(ValueTypeClass(QualifiedName(childSObject.getType)))

        override def range: Range = Range.INVALID_LOCATION

        override def nodeType: AstNodeType = SObjectChildRelationshipNodeType

        override protected def resolveDefinitionImpl(): Option[AstNode] = Option(this)
    }

    private case class SObjectRelationshipFieldNode(library: SObjectLibrary, relField: SObjectRelationshipFieldMember) extends SObjectNodeBase(library, relField) {
        override def getValueType: Option[ValueType] = {
            val referenceTo = relField.getReferenceTo
            if (null != referenceTo && 1 == referenceTo.length) {
                val relatedSobjectType = referenceTo(0)
                Option(ValueTypeClass(QualifiedName(Array(relatedSobjectType))))
            } else {
               // looks like this is a polymorphic field (e.g. Event.Owner) and can point to more than 1 object type
                None
            }
        }

        override def children: Seq[AstNode] = {
            getValueType match {
                case Some(valueType) =>
                    library.getByQualifiedName(valueType.qualifiedName).map(_.children).getOrElse(super.children)
                case None => super.children
            }
        }
        override def symbolKind: SymbolKind = SymbolKind.Field
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

        //override def symbolIsStatic: Boolean = modifiers.exists(_.modifierType == ModifierNode.STATIC)
        override def symbolIsStatic: Boolean = false

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
