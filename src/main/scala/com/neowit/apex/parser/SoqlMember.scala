package com.neowit.apex.parser

import com.neowit.apex.Session
import com.neowit.apex.completion.models.SoqlModel
import com.neowit.apex.completion.{SObjectMember, DatabaseModel, DatabaseModelMember}
import com.neowit.apex.parser.antlr.SoqlParser.ObjectTypeContext
import com.sforce.soap.partner.ChildRelationship
import org.antlr.v4.runtime.Token

trait SoqlMember extends Member {
    override def isStatic: Boolean = false
    override def toString: String = getIdentity
}

class FromTypeMember(objectTypeToken: Token, session: Session) extends SoqlMember {

    def this(ctx: ObjectTypeContext, session: Session) {
        this(ctx.Identifier().getSymbol, session)
    }
    override def getIdentity: String = objectTypeToken.getText

    override def getType: String = getIdentity

    override def getSignature: String = getIdentity

    private def getSObjectMember: Option[DatabaseModelMember] = DatabaseModel.getModelBySession(session) match {
        case Some(dbModel) => dbModel.getSObjectMember(getIdentity)
        case _ => None
    }

    override def getChildren: List[Member] = {
        getSObjectMember match {
            case Some(sobjectMember) =>
                sobjectMember.getChildren
            case None => Nil
        }
    }

    override def getChild(identity: String, withHierarchy: Boolean): Option[Member] = {
        getSObjectMember match {
            case Some(sobjectMember) =>
                sobjectMember.getChild(identity)
            case None => None
        }
    }

    def getChildRelationships: Array[com.sforce.soap.partner.ChildRelationship]  = {
        getSObjectMember match {
            case Some(member) if member.isInstanceOf[SObjectMember] =>
                val sobjectMember = member.asInstanceOf[SObjectMember]
                //force sobject member to load its children (including relationships)
                sobjectMember.getChildren
                //return actual relationships
                sobjectMember.asInstanceOf[SObjectMember].getChildRelationships
            case None => Array()
        }
    }

    def getChildRelationship(identity: String): Option[com.sforce.soap.partner.ChildRelationship] = {
        getChildRelationships.find(_.getRelationshipName.toLowerCase == identity.toLowerCase)
    }
}

class SubqueryFromTypeMember(relationshipName: String, parentFromMember: FromTypeMember, session: Session) extends SoqlMember {
    /**
     * @return
     * for class it is class name
     * for method it is method name + string of parameter types
     * for variable it is variable name
     * etc
     */
    override def getIdentity: String = relationshipName

    override def getType: String = parentFromMember.getType + "." + getIdentity

    override def getSignature: String = getIdentity


    override def getChildren: List[Member] = {
        getSObjectMember match {
            case Some(member) => member.getChildren
            case None => Nil
        }
    }

    override def getChild(identity: String, withHierarchy: Boolean): Option[Member] = {
        getSObjectMember match {
            case Some(member) => member.getChild(identity, withHierarchy = false)
            case None => None
        }
    }

    private def getSObjectMember: Option[DatabaseModelMember] = {
        parentFromMember.getChildRelationship(getIdentity) match {
            case Some(relationship) =>
                DatabaseModel.getModelBySession(session) match {
                    case Some(dbModel) => dbModel.getSObjectMember(relationship.getChildSObject)
                    case _ => None
                }
            case None => None
        }
    }
}

class DBModelMember(session: Session) extends Member {
    /**
     * @return
     * for class it is class name
     * for method it is method name + string of parameter types
     * for variable it is variable name
     * etc
     */
    override def getIdentity: String = "APEX_DB_MODEL"

    override def getType: String = getIdentity

    override def getSignature: String = getIdentity

    override def isStatic: Boolean = true

    override def getChildren: List[Member] = DatabaseModel.getModelBySession(session) match {
        case Some(dbModel) => dbModel.getSObjectMembers
        case None => Nil
    }

    override def getChild(identity: String, withHierarchy: Boolean): Option[Member] = {
        DatabaseModel.getModelBySession(session) match {
            case Some(dbModel) => dbModel.getSObjectMember(getIdentity)
            case None => None
        }
    }

    override def toString: String = getIdentity
}

/**
 * artificial proxy which contains list of relationships for specific object type
 */
class ChildRelationshipsContainerMember(fromTypeMember: FromTypeMember) extends SoqlMember {

    override def getIdentity: String = fromTypeMember.getIdentity + "CHILD_RELATIONSHIPS"

    override def getType: String = getIdentity

    override def getSignature: String = getIdentity

    override def getChildren: List[Member] = fromTypeMember.getChildRelationships.toList.filter(null != _.getRelationshipName).map(new ChildRelationshipMember(_))

    override def getChild(identity: String, withHierarchy: Boolean): Option[Member] = super.getChild(identity, withHierarchy)
}

class ChildRelationshipMember(relationship: ChildRelationship) extends SoqlMember {

    override def getIdentity: String = relationship.getRelationshipName

    override def getType: String = getIdentity

    override def getSignature: String = getIdentity + " -> " + relationship.getChildSObject
}

