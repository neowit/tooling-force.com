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

package com.neowit.apex.completion

import com.neowit.TcpServer
import com.neowit.apex.Session
import com.neowit.apex.parser.Member
import com.neowit.apex.parser.MemberJsonSupport._
import com.neowit.utils.Logging
import akka.actor.Actor
import akka.actor.Props
import com.sforce.soap.partner.FieldType

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.FiniteDuration
import scala.util.{Failure, Success, Try}

object DatabaseModel {
    def REFRESH_INTERVAL_SECONDS = 30 //content of previously loaded fields will be refreshed over this period of time

    private val modelBySession = new collection.mutable.HashMap[Session, DatabaseModel]

    /**
      * @param sObjectApiName - full SObject API Name
      * @return
      *          if sObjectApiName looks like Something_\_Name[_\_c] then
      *          return: Name[_\_c]
      *          otherwise return sObjectApiName as is
      */
    def stripNamespace(sObjectApiName: String): String = {
        getNamespacePrefix(sObjectApiName) match {
            case Some(namespace) => sObjectApiName.substring(namespace.length + 2) // +2 to include "_ _" after namespace
            case None => sObjectApiName
        }
    }

    /**
      * used in sortWith()
      */
    def hasNamespacePrefix(sObjectApiName: String): Int = {
        if (getNamespacePrefix(sObjectApiName).isDefined) {
            //has namespace prefix
            1
        } else {
            0
        }
    }

    def getNamespacePrefix(sObjectApiName: String): Option[String] = {
        val namespaceEnd = sObjectApiName.indexOf("__")
        if (namespaceEnd > 0 && (sObjectApiName.length > namespaceEnd + 3) && ('c' != sObjectApiName.charAt(namespaceEnd + 3))) {
            //has namespace prefix
            Option(sObjectApiName.substring(0, namespaceEnd))
        } else {
            None
        }
    }

    def getModelBySession(session: Session): Option[DatabaseModel] = {
        modelBySession.result().get(session) match {
          case Some(model) => Some(model)
          case None => //load model first
                val model = new DatabaseModel(session)
                modelBySession += session -> model
                Some(model)
        }
    }
    def removeModelBySession(session: Session): Option[DatabaseModel] = {
        modelBySession.remove(session)
    }

    def sortNamespaceLast[A](collection: Array[A], getApiName: A => String): Array[A] = {
        collection.sortWith((left, right) => hasNamespacePrefix(getApiName(left)) - hasNamespacePrefix(getApiName(right)) < 0)
    }

    def getNameFromSobjectDescribeResult(sobjectDescribeResult: com.sforce.soap.partner.DescribeGlobalSObjectResult): String = {
        sobjectDescribeResult.getName
    }
}

class DatabaseModel(session: Session) {
    import DatabaseModel._

    private val (memberBySobjectType: Map[String, DatabaseModelMember], sObjectMembers: List[DatabaseModelMember]) = load()

    def getSObjectMember(typeName: String): Option[DatabaseModelMember] = {
        memberBySobjectType.get(typeName.toLowerCase)
    }

    def getSObjectMembers: List[Member] = sObjectMembers

    def load(): (Map[String, DatabaseModelMember], List[DatabaseModelMember]) = {
        val _memberBySobjectType = Map.newBuilder[String, DatabaseModelMember]
        val _sObjectMembers = List.newBuilder[DatabaseModelMember]
        val describeGlobalResult = session.describeGlobal
        val knownApiNames = new collection.mutable.HashSet[String]()

        // sort result such as types without namespace go first
        /*
        val sobjectsSortedByNoNamespaceFirst =
                describeGlobalResult.getSobjects.sortWith((left, right) => hasNamespacePrefix(left.getName) - hasNamespacePrefix(right.getName) < 0)
        */
        val sobjectsSortedByNoNamespaceFirst = sortNamespaceLast(describeGlobalResult.getSobjects, getNameFromSobjectDescribeResult)

        for (describeGlobalSObjectResult <- sobjectsSortedByNoNamespaceFirst) {
            val sObjectApiName = describeGlobalSObjectResult.getName
            val sObjectMember = new SObjectMember(sObjectApiName, session)

            val sObjectApiNameLowerCase = sObjectApiName.toLowerCase
            _memberBySobjectType += sObjectApiNameLowerCase -> sObjectMember
            _sObjectMembers += sObjectMember
            knownApiNames += sObjectApiNameLowerCase

            // store by name without Namespace, but *only* if this name has not already been registered
            // by similarly named object which does not have a Namespace
            val withoutNamespace = stripNamespace(sObjectApiNameLowerCase)
            if (sObjectApiName != withoutNamespace && !knownApiNames.contains(withoutNamespace)) {
                _memberBySobjectType += sObjectApiNameLowerCase -> sObjectMember
                _sObjectMembers += sObjectMember
            }
        }
        (_memberBySobjectType.result(), _sObjectMembers.result())
    }

}

case class RefreshMessage(dbModelMember: DatabaseModelMember)

class DatabaseModelRefreshActor extends Actor with Logging {
    def receive = {
        case RefreshMessage(dbModel) => runRefresh(dbModel)
        case _ => println("DatabaseModelRefreshActor: huh?")
    }

    def runRefresh(dbModelMember: DatabaseModelMember): Unit = {
        logger.trace("refreshing DB Model: " + dbModelMember.getSignature)
        dbModelMember.refresh()
    }
}

trait DatabaseModelMember extends Member {
    def isLoaded: Boolean

    /**
      * DatabaseModelMember members do not come from local file and can not have location
      */
    override def getLocation: Option[Location] = None

    /**
     * all DatabaseModelMember children are always NON static
     */
    override def isStatic: Boolean = false

    def loadMembers(): Unit = {  }
    def refresh(): Unit = {  }

    override def getChildren: List[Member] = {
        if (!isLoaded) {
            loadMembers()
        }
        super.getChildren
    }

    override def getChild(identity: String, withHierarchy: Boolean): Option[Member] = {
        if (!isLoaded) {
            loadMembers()
        }
        super.getChild(identity, withHierarchy)
    }

    def scheduleRefresh(): Unit = {
        val databaseModelRefreshActor = TcpServer.system.actorOf(Props[DatabaseModelRefreshActor])
        //val duration = FiniteDuration(DatabaseModel.REFRESH_INTERVAL_SECONDS, scala.concurrent.duration.SECONDS)
        val duration = FiniteDuration(15, scala.concurrent.duration.SECONDS)
        TcpServer.system.scheduler.scheduleOnce(duration, databaseModelRefreshActor, new RefreshMessage(this))
    }

}

/**
 * description of SObject, like Account
 * @param sObjectApiName - full API name of Standard or Custom Object
 * @param session - valid SFDC session config
 */
class SObjectMember(val sObjectApiName: String, val session: Session) extends DatabaseModelMember {
    private var isDoneLoading = false
    override def isLoaded: Boolean = isDoneLoading

    private var childRelationships: Array[com.sforce.soap.partner.ChildRelationship] = Array()

    override def getType: String = sObjectApiName //SObject API Name here

    override def getSignature: String = sObjectApiName

    /**
     * @return
     * for class it is class name
     * for method it is method name + string of parameter types
     * for variable it is variable name
     * etc
     */
    override def getIdentity: String = getType

    override def loadMembers(): Unit = {
        isDoneLoading = true
        //session.retrieve()
        Try(session.describeSObjects(List(sObjectApiName))) match {
            case Success(describeSObjectResults) =>
                if (describeSObjectResults.nonEmpty) {
                    val describeSobjectResult = describeSObjectResults.head
                    childRelationships = describeSobjectResult.getChildRelationships
                    for (field <- describeSobjectResult.getFields) {
                        val fMember = new SObjectFieldMember(field)
                        addChild(fMember, overwrite = true)
                        if (fMember.isReference) {
                            //add artificial child
                            addChild(new SObjectRelationshipFieldMember(field), overwrite = true)
                        }
                    }
                    extendSObject()
                }
                scheduleRefresh()

            case Failure(error) => //ignore errors because we are in auto-complete mode
                isDoneLoading = false
                println(error)
        }

    }

    /**
     * every DB object extends SObject, so we need to add SObject methods to each DB Object type
     */
    private def extendSObject(): Unit = {
        /*
        ApexModel.getSystemTypeMember("SObject") match {
          case Some(member) => member.getChildren.map(m => this.addChild(m))
          case None =>
        }
        */
    }

    override def refresh(): Unit = {
        isDoneLoading = false
        //clearChildren()
        loadMembers()
    }
    def getChildRelationships: Array[com.sforce.soap.partner.ChildRelationship]  = {
        childRelationships
    }

    override def toString: String = this.getIdentity
}

class SObjectFieldMember(field: com.sforce.soap.partner.Field) extends DatabaseModelMember {
    override def isLoaded: Boolean = true //field does not have children to load

    override def getType: String =
        field.getSoapType.name().replaceFirst("_", "") //some fields have type like "_double"


    override def getSignature: String = {
        val lengthStr = if (field.getLength > 0) " [" + field.getLength + "]" else ""
        field.getLabel + " (" + field.getType.name().replaceFirst("_", "") + lengthStr + ")"
    }

    override def getIdentity: String = field.getName //Field API Name here

    override def getDoc: String =
        if (null != field.getInlineHelpText) field.getInlineHelpText else ""


    def isReference:Boolean = field.getType == FieldType.reference

}

class SObjectRelationshipFieldMember(field: com.sforce.soap.partner.Field) extends SObjectFieldMember(field) {

    override def toString: String = {
        this.getIdentity // super.toString causes StackOverflow for some reason, even though it also calls getIdentity
    }

    override def getIdentity: String = {
        if (null != field.getRelationshipName)
            field.getRelationshipName
        else {
            //polymorphic fields do not have value in field.getRelationshipName
            field.getName.replaceAll("(?i)id$", "") //(?i) is to make regex case insensitive
        }
    } //Field API Name here

    override def getSignature: String = {
        field.getLabel + " (reference to: " + field.getReferenceTo.mkString(",") + ")"
    }

    /**
      * @return
      *     for Account.Parent - return: Array(Account)
      *     for polymorphic fields like Event.Owner - return: Array(User, Group)
      */
    def getReferenceTo: Array[String] = field.getReferenceTo

    override def getChild(identity: String, withHierarchy: Boolean): Option[Member] = {
        getParent match {
            case Some(sobjectMember: SObjectMember) =>
                DatabaseModel.getModelBySession(sobjectMember.session) match {
                    case Some(dbModel) => dbModel.getSObjectMember(field.getReferenceTo.head) match {
                        case Some(relatedSobjectMember) =>
                            return relatedSobjectMember.getChild(identity, withHierarchy)
                        case None =>
                    }
                    case None =>
                }
            case _ =>
        }
        None
    }

    override def getChildren: List[Member] = {
        //load related object members
        getParent match {
            case Some(sobjectMember: SObjectMember) =>
                DatabaseModel.getModelBySession(sobjectMember.session) match {
                    case Some(dbModel) => dbModel.getSObjectMember(field.getReferenceTo.head) match {
                        case Some(relatedSobjectMember) =>
                            return relatedSobjectMember.getChildren
                        case None =>
                    }
                    case None =>
                }
            case _ =>
        }
        List()
    }
}
