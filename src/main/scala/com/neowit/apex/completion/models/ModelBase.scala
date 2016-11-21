package com.neowit.apex.completion.models

import com.neowit.apex.parser.Member
import com.neowit.apex.parser.MemberJsonSupport._
import spray.json.{JsValue, JsonParser}

trait ModelBase {
    def getNameSpaces: List[String]
    def getMemberByNamespace: Map[String, ApexModelMember]
    def getNamespaceInstance(namespace: String): GenericNamespace

    def load(): Map[String, ApexModelMember] = {
        val memberByNamespace = Map.newBuilder[String, GenericNamespace]
        for (namespace <- getNameSpaces) {
            memberByNamespace += (namespace.toLowerCase -> getNamespaceInstance(namespace))
        }

        memberByNamespace.result()
    }

    def getNamespace(namespace: String): Option[Member] = {
        getMemberByNamespace.get(namespace.toLowerCase)
    }

    def getMembers(namespace: String): List[Member] = {
        if (namespace.indexOf(".") > 0) {
            //this is most likely something like ApexPages.StandardController, i.e. type ApexPages.StandardController in namespace "ApexPages"
            val types = getMembers(namespace.split("\\.")(0))
            return types.find(_.getSignature.toLowerCase == namespace.toLowerCase) match {
                case Some(member) => member.getChildren
                case None => List()
            }
        }
        val members = getNamespace(namespace) match {
            case Some(member) => member.getChildren
            case none => List()
        }

        members
    }

}

trait ApexModelMember extends Member {
    //this is for cases when we load stuff from System.<Class> into appropriate <Class> namespace
    //e.g. System.Database methods are loaded into Database namespace
    override def isParentChangeAllowed: Boolean = true

    override def getVisibility: String = "public"
    override def getType: String = getIdentity

    protected def isLoaded: Boolean = true

    /**
      * ApexModelMember members do not come from local file and can not have location
      */
    override def getLocation: Option[Location] = None

    override def getChildren: List[Member] = {
        if (!isLoaded) {
            loadMembers()
        }
        super.getChildren
    }
    def getStaticChildren: List[Member] = {
        getChildren.filter(_.isStatic)
    }

    def loadMembers(): Unit = {  }

    override def getChild(identity: String, withHierarchy: Boolean): Option[Member] = {
        if (!isLoaded) {
            loadMembers()
        }
        super.getChild(identity, withHierarchy)
    }
}

abstract class GenericNamespace(name: String) extends ApexModelMember {

    override def getIdentity: String = name

    override def getSignature: String = name

    override def isStatic: Boolean = true

    protected def loadTypes(types: Map[String, JsValue], overwriteChildren: Boolean)

    protected def loadFile(filePath: String, overwriteChildren: Boolean = false): Unit = {
        val is = getClass.getClassLoader.getResource("apex-doc/" + filePath + ".json")
        if (null == is) {
            return
        }
        val doc = scala.io.Source.fromInputStream(is.openStream())("UTF-8").getLines().mkString
        val jsonAst = JsonParser(doc)
        val types = jsonAst.asJsObject.fields //Map[typeName -> type description JSON]

        loadTypes(types, overwriteChildren)
    }

}

