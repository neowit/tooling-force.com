package com.neowit.apex.completion

import com.neowit.TcpServer
import com.neowit.apex.Session
import com.neowit.apex.completion.models.ApexModel
import com.neowit.apex.parser.Member
import com.neowit.utils.Logging
import akka.actor.Actor
import akka.actor.Props
import com.sforce.soap.partner.FieldType

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.FiniteDuration
import scala.util.{Failure, Success, Try}

object DatabaseModel {
    def REFRESH_INTERVAL_SECONDS = 30 //content of previously loaded fields will be refreshed over this period of time

    private val modelBySession = Map.newBuilder[Session, DatabaseModel]
    def getModelBySession(session: Session): Option[DatabaseModel] = {
        modelBySession.result().get(session) match {
          case Some(model) => Some(model)
          case None => //load model first
                val model = new DatabaseModel(session)
                modelBySession += session -> model
                Some(model)
        }
    }
}

class DatabaseModel(session: Session) {

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
        val sobjectsSortedByNoNamespaceFirst =
                describeGlobalResult.getSobjects.sortWith((left, right) => hasNamespacePrefix(left.getName) - hasNamespacePrefix(right.getName) < 0)

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

    /**
     * @param sObjectApiName - full SObject API Name
     * @return
     *          if sObjectApiName looks like Something__Name[__c] then
     *          return: Name[__c]
     *          otherwise return sObjectApiName as is
     */
    private def stripNamespace(sObjectApiName: String): String = {
        val namespaceEnd = sObjectApiName.indexOf("__")
        if (namespaceEnd > 0 && (sObjectApiName.length > namespaceEnd + 3) && ('c' != sObjectApiName.charAt(namespaceEnd + 3))) {
            //has namespace prefix
            sObjectApiName.substring(namespaceEnd + 2)
        } else {
            sObjectApiName
        }
    }

    private def hasNamespacePrefix(sObjectApiName: String): Int = {
        val namespaceEnd = sObjectApiName.indexOf("__")
        if (namespaceEnd > 0 && (sObjectApiName.length > namespaceEnd + 3) && ('c' != sObjectApiName.charAt(namespaceEnd + 3))) {
            //has namespace prefix
            1
        } else {
            0
        }

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
    protected def isLoaded: Boolean

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
    override protected def isLoaded: Boolean = isDoneLoading

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
        ApexModel.getSystemTypeMember("SObject") match {
          case Some(member) => member.getChildren.map(m => this.addChild(m))
          case None =>
        }

    }

    override def refresh(): Unit = {
        isDoneLoading = false
        //clearChildren()
        loadMembers()
    }
    def getChildRelationships: Array[com.sforce.soap.partner.ChildRelationship]  = {
        childRelationships
    }
}

class SObjectFieldMember(field: com.sforce.soap.partner.Field) extends DatabaseModelMember {
    override protected def isLoaded: Boolean = true //field does not have children to load

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
