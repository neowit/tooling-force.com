package com.neowit.apex.completion

import com.neowit.TcpServer
import com.neowit.apex.Session
import com.neowit.apex.parser.Member

import com.sforce.soap.metadata.ListMetadataQuery

import akka.actor.Actor
import akka.actor.Props
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

    private val memberBySobjectType: Map[String, DatabaseModelMember] = load()

    def getSObjectMember(typeName: String): Option[DatabaseModelMember] = {
        memberBySobjectType.get(typeName.toLowerCase)
    }

    def load(): Map[String, DatabaseModelMember] = {
        val _memberBySobjectType = Map.newBuilder[String, DatabaseModelMember]
        val apiVersion = session.getConfig.apiVersion
        val query = new ListMetadataQuery()
        query.setType("CustomObject")
        Try(session.listMetadata(Array(query), apiVersion)) match {
            case Success(fileProperties) =>
                for (fileProp <- fileProperties) {
                    //val typeName = fileProp.getType //SObject
                    val sObjectApiName = fileProp.getFullName //Actual SObject API Name
                    val sObjectMember = new SObjectMember(sObjectApiName, session)
                    _memberBySobjectType += sObjectApiName.toLowerCase -> sObjectMember

                    //resourcesByXmlTypeName = addToMap(resourcesByXmlTypeName, typeName, resourceName)
                }
            case Failure(error) => throw error
        }
        _memberBySobjectType.result()
    }

}

case class RefreshMessage(dbModelMember: DatabaseModelMember)

class DatabaseModelRefreshActor extends Actor {
    def receive = {
        case RefreshMessage(dbModel) => runRefresh(dbModel)
        case _ => println("DatabaseModelRefreshActor: huh?")
    }

    def runRefresh(dbModelMember: DatabaseModelMember): Unit = {
        println("refreshing DB Model")
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
class SObjectMember(sObjectApiName: String, session: Session) extends DatabaseModelMember {
    private var isDoneLoading = false
    override protected def isLoaded: Boolean = isDoneLoading

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
                    for (field <- describeSobjectResult.getFields) {
                        val fMember = new SObjectFieldMember(field)
                        addChild(fMember)
                    }
                }
                scheduleRefresh()

            case Failure(error) => //ignore errors because we are in auto-complete mode
                isDoneLoading = false
                println(error)
        }

    }

    override def refresh(): Unit = {
        isDoneLoading = false
        clearChildren()
        loadMembers()

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
}
