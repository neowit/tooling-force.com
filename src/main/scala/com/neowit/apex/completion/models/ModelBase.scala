package com.neowit.apex.completion.models

import com.neowit.apex.parser.Member
import spray.json.{JsValue, JsonParser}

trait ModelBase {
    def getNameSpaces: List[String]
    def getMemberByNamespace: Map[String, ApexModelMember]
    def getNamespaceInstance(namespace: String): Namespace

    def load(): Map[String, ApexModelMember] = {
        val memberByNamespace = Map.newBuilder[String, ApexModelMember]
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

trait Namespace extends ApexModelMember {
    def getName: String

    override def getIdentity: String = getName

    override def getSignature: String = getIdentity
    override def isStatic: Boolean = true

    protected def loadTypes(types: Map[String, JsValue], overwriteChildren: Boolean = false)

    protected def loadFile(filePath: String, overwriteChildren: Boolean = false): Unit = {
        val is = getClass.getClassLoader.getResource("apex-doc/" + filePath + ".json")
        if (null == is) {
            return
        }
        val doc = scala.io.Source.fromInputStream(is.openStream())("UTF-8").getLines().mkString
        val jsonAst = JsonParser(doc)
        val types = jsonAst.asJsObject.fields //Map[typeName -> type description JSON]
        loadTypes(types)

    }
}

