package com.neowit.apex.completion

import com.neowit.apex.parser.Member

import scala.xml.Node

object ApexModel {

    def load(): Map[String, ApexModelMember] = {
        val memberByNamespace = Map.newBuilder[String, ApexModelMember]
        val is = getClass.getClassLoader.getResource("apexModel.xml")
        val doc = xml.XML.load(is)
        for(namespace <- doc \ "namespace" ) {
            val name = (namespace \ "@name").text.toLowerCase
            //println("namespace=" + name)
            memberByNamespace += (name -> new ApexNamespace(namespace))
        }
        memberByNamespace.result()
    }

    private val memberByNamespace: Map[String, ApexModelMember] = load()

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

        //check if we can load this from "System"
        val systemMembers = getSystemMembers(namespace)
        members ++ systemMembers
    }

    def getInstanceMembers(typeName: String): List[Member] = {
        ApexModel.getMembers("(instance methods)").find(_.getIdentity.toLowerCase == typeName.toLowerCase) match {
          case Some(_type) =>
              _type.getChildren
          case None => List()
        }
    }

    def getNamespace(namespace: String): Option[Member] = {
        memberByNamespace.get(namespace.toLowerCase)
    }
    def getSystemMembers(systemType: String): List[Member] = {
        memberByNamespace.get("system") match {
            case Some(system) =>
                system.getChildren.find(_.getIdentity.toLowerCase == systemType.toLowerCase) match {
                    case Some(typeMember) =>
                        typeMember.getChildren
                    case None => List()
                }
            case None => List()
        }

    }

}

trait ApexModelMember extends Member {
    def getApexType: Node

    override def getIdentity: String = (getApexType \ "@name").text

    override def getSignature: String = getIdentity

    override def getVisibility: String = "public"

    override def getPath: String = getParent match {
      case Some(member) => member.getIdentity
      case None => "system"
    }

    protected def getAttribute(node: Node, attrName: String): String = {
        node.attribute("doc") match {
            case Some(nodeSequence) => nodeSequence.toList.head.text
            case None => ""
        }
    }
    protected def childrenNames: List[String]
    private var isLoaded = false
    def loadMembers(): Unit = {
        for(childType <- childrenNames) {
            for (node <- getApexType \ childType) {
                val member = node match {
                    case ApexType(_) => new ApexType(node)
                    case ApexMethod(_) => new ApexMethod(node)
                }
                member.parent = Some(this)
                this.addChild(member)
            }
        }
        isLoaded = true
    }

    override def getChildren: List[Member] = {
        if (!isLoaded) {
            loadMembers()
        }
        super.getChildren
    }
}
class ApexNamespace(namespace: Node) extends ApexModelMember {
    override def getApexType: Node = namespace

    override def isStatic: Boolean = true
    override protected def childrenNames = List("type")

}

object ApexType {
    def unapply(node: Node): Option[Node] = {
        if ("type" == node.label) {
            Some(node)
        } else {
            None
        }
    }
}
class ApexType(apexType: Node) extends ApexModelMember {
    override def getApexType: Node = apexType

    override def isStatic: Boolean = true
    override protected def childrenNames = List("method")
}


object ApexMethod {
    def unapply(node: Node): Option[Node] = {
        if ("method" == node.label) {
            Some(node)
        } else {
            None
        }
    }
}
class ApexMethod(apexMethod: Node) extends ApexModelMember {
    override def getApexType: Node = apexMethod

    override def getIdentity: String = (getApexType \ "@name").text

    override protected def childrenNames = List()

    override def getSignature: String = {
        val static = if (isStatic) "static " else ""
        static + getVisibility + " " + getType + " " + getIdentity + "(" + getParams.map(getParamSignature).mkString(", ") +")"
    }

    override def isStatic: Boolean = "true" == (getApexType \ "@isStatic").text

    def getParams: List[Node] = (apexMethod \ "param").toList

    private def getParamSignature(node : Node): String = {
        (node \ "@type").text + " " + getAttribute(node, "doc")
    }

    override def getDoc: String = getAttribute(getApexType, "doc")

    override def getType: String = (getApexType \ "@returnType").text
}

