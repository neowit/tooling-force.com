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


import scala.collection.mutable
import spray.json._
import DefaultJsonProtocol._
import MemberJsonSupport._
import com.neowit.apexscanner.symbols.Symbol

trait AnonymousMember {
    private var parent: Option[AnonymousMember] = None

    def isParentChangeAllowed: Boolean = false

    def setParent(member: AnonymousMember): Unit = {
        if (parent.isDefined && !isParentChangeAllowed) {
            throw new IllegalAccessError("can not set parent of Member twice")
        } else {
            parent = Some(member)
        }
    }
    def getParent: Option[AnonymousMember] = parent

    /**
     * find parent which matches specified filter
     * @param filter - condition
     * @return
     */
    def findParent(filter: (AnonymousMember) => Boolean): Option[AnonymousMember] = {
        getParent match {
            case Some(parentMember) =>
                if (filter(parentMember)) Some(parentMember) else parentMember.findParent(filter)
            case None => None
        }
    }


    def getSuperType: Option[String] = None

    def getFullSuperType: Option[String] = None

    private val children = mutable.HashMap[String, Member]()
    protected def clearChildren(): Unit = children.clear()

    def addChild(member: AnonymousMember): Unit = {
        addChild(member, overwrite = false)
    }
    def addChild(member: AnonymousMember, overwrite: Boolean = false): Unit = {
        member.getParent match {
            case Some(_parent) if _parent.equals(this) => //do nothing
            case _ => member.setParent(this)
        }
        //do not add AnonymousMember to the default list of children
        member match {
            case m: Member =>
                try {
                    val canAdd = overwrite || (getChild(m.getIdentity.toLowerCase, withHierarchy = false) match {
                        case Some(_existingMember) =>
                            //this child already exists, do not overwrite
                            false
                        case None => true
                    })

                    if (canAdd) {
                        children.+=(m.getIdentity.toLowerCase -> m)
                    }
                } catch {
                    case ex:Throwable =>
                        this match {
                            case m: Member =>
                                println("failed to add member of parent: " + m.getIdentity)
                                println(ex.getStackTrace)
                            case _ =>
                                println("failed to add member of parent")
                        }
                }
            case _ => //do nothing
        }
    }

    def getChildren: List[Member] = {
        children.values.toList
    }


    def getChild(identity : String, withHierarchy: Boolean = true): Option[Member] = {

        getChildInternal(identity, withHierarchy) match {
            case Some(childMember) => Some(childMember)
            case None =>
                None
        }
    }

    protected def getChildInternal(identity: String, withHierarchy: Boolean = true): Option[Member] = {
        children.get(identity.toLowerCase)
    }
}

object Member {
    def symbolToMember(symbol: Symbol): Member = {
        new Member {
            override def getLocation: Option[MemberJsonSupport.Location] = {

                //     case class Location ( file: Path, line: Option[Int], column: Option[Int], identity: String )
                val location =
                    MemberJsonSupport.Location(
                        symbol.symbolLocation.path,
                        Option(symbol.symbolLocation.range.start.line),
                        Option(symbol.symbolLocation.range.start.col),
                        symbol.symbolName
                    )
                Option(location)
            }

            override def isStatic: Boolean = symbol.symbolIsStatic

            override def getSignature: String = symbol.symbolLabel

            override def getType: String = symbol.symbolValueType.getOrElse("")

            override def getIdentity: String = symbol.symbolName
        }
    }
}

trait Member extends AnonymousMember {

    /**
     * @return
     * for class it is class name
     * for method it is method name + string of parameter types
     * for variable it is variable name
     * etc
     */
    def getIdentity:String

    /**
      * @return position of this member in the file system and inside file
      */
    def getLocation: Option[Location]

    /**
      * for cases where member with same name resides in different locations (e.g. methods of one class with same name but different parameters)
      */
    def getLocations: List[Location] = {
        getLocation match {
            case Some(location) => List(location)
            case None => Nil
        }
    }

    def serialise: JsValue = {
        getLocations.toJson
    }

    /**
     * for most member types Identity is unique (for Methods and Inner Classes it is not)
     */
    def getIdentityToDisplay:String = getIdentity

    def getSignature:String
    def getType: String

    //e.g. MyClass.InnerClass
    def getFullType: String = getType

    def getDoc: String = " " //TODO - implement documentation retrieval

    def getVisibility: String = "private" //TODO

    def isStatic: Boolean

    /*
    def getPath: String = {

        def resolveFullPath(currentPath: String): String = getParent match {
            case Some(member) => member.getPath + "." + currentPath
            case None => currentPath
        }
        val res = resolveFullPath(getIdentity)
        res
    }
    */

    override def toString: String = {
        //val txt = getIdentity
        val txt = getSignature + " => type=" + getType + "; id=" + getIdentity
        val chlds = new StringBuilder()
        for (x <- getChildren) {
            chlds.++= (x.toString())
        }
        if (chlds.nonEmpty )
            txt + ": \n - " + chlds.toString()
        else
            txt + "\n - "
    }

    def toJson: JsValue = {
        val data = Map("identity" -> getIdentityToDisplay, "realIdentity" -> getIdentity,
            "signature" -> getSignature, "type" -> getType,
            "visibility" -> getVisibility, "doc" -> getDoc)
        //JSONObject(data)
        data.toJson
    }

    override def equals(o: Any): Boolean = {
        o.isInstanceOf[Member] && this.getClass == o.getClass &&
            this.getIdentity.toLowerCase == o.asInstanceOf[Member].getIdentity.toLowerCase
    }
    override def hashCode = getIdentity.toLowerCase.hashCode

}

case class CompoundMember(members: List[Member]) extends Member {
    /**
      * @return
      * for class it is class name
      * for method it is method name + string of parameter types
      * for variable it is variable name
      * etc
      */
    override def getIdentity: String = members.head.getIdentity

    /**
      * @return position of this member in the file system and inside file
      */
    override def getLocation: Option[Location] = None

    /**
      * for cases where member with same name resides in different locations (e.g. methods of one class with same name but different parameters)
      */
    override def getLocations: List[Location] = members.filter(_.getLocation.isDefined).map(_.getLocation.get)

    override def getSignature: String = members.head.getSignature

    override def getType: String = members.head.getType

    override def isStatic: Boolean = members.head.isStatic
}
/**
 * some types (e.g. enum) have default members, defined on the system level, in addition to user defined members
 * @param parent - member to which this built-in member belongs
 * @param identity - e.g. "getChild"
 * @param displayIdentity e.g. "getChild()"
 * @param retType - e.g. "SomeClass"
 * @param signature - e.g. "public SomeClass getChild(Integer)
 * @param isStaticMember - true if this member is static
 */
class BuiltInMember(parent: AnonymousMember, identity: String, displayIdentity: String, retType: String,
                    signature: Option[String] = None, isStaticMember: Boolean = false) extends Member {
    setParent(parent)

    override def getIdentity: String = identity
    override def getIdentityToDisplay: String = displayIdentity

    override def getType: String = retType

    override def getSignature: String = signature match {
        case Some(s) => s
        case None => "public " + getType + " " + getIdentityToDisplay
    }

    override def isStatic: Boolean = isStaticMember

    /**
      * @return position of this member in the file system and inside file
      */
    override def getLocation: Option[Location] = None
}

class BuiltInMethodMember(parent: Member, identity: String, displayIdentity: String, retType: String,
                          params: List[String] = List(),
                          signature: Option[String] = None, isStaticMember: Boolean = false)
    extends BuiltInMember(parent, identity, displayIdentity, retType, signature, isStaticMember) {

    override def getSignature: String = signature match {
        case Some(s) => s
        case None => "public " + getType + " " + getIdentityToDisplay + "(" + params.mkString(", ") + ")"
    }

    /**
      * @return position of this member in the file system and inside file
      */
    override def getLocation: Option[Location] = None
}

