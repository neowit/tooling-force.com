package com.neowit.apex.completion

import com.neowit.apex.parser.Member
import scala.collection.JavaConversions._
import spray.json._
//import DefaultJsonProtocol._

object ApexModelJsonProtocol extends DefaultJsonProtocol {
    //implicit val namespaceFormat = jsonFormat1(ApexNamespace2)
    implicit val apexTypeFormat: JsonFormat[ApexType2] = lazyFormat(jsonFormat(ApexType2, "name", "superType", "methods", "tag", "ctors", "fqn"))
    implicit val apexMethodFormat: JsonFormat[ApexMethod2] = lazyFormat(jsonFormat(ApexMethod2, "s", "n", "v", "p", "h", "r", "d"))
    //implicit val apexParamFormat: JsonFormat[ApexParam2] = jsonFormat(ApexParam2)
}

object ApexModel2 {

    val NAMESPACES = List("ApexPages", "Approval", "Auth", "Database", "Dom", "Flow", "KbManagement", "Messaging", "QuickAction", "Reports", "Schema", "System" )

    private val memberByNamespace: Map[String, ApexModel2Member] = load()

    def load(): Map[String, ApexModel2Member] = {
        val memberByNamespace = Map.newBuilder[String, ApexModel2Member]
        for (namespace <- NAMESPACES) {
            /*
            val is = getClass.getClassLoader.getResource("apex-doc/" + namespace + ".json")
            val doc = scala.io.Source.fromInputStream(is.openStream()).getLines().mkString
            val jsonAst = JsonParser(doc)
            //println(jsonAst.prettyPrint)
            */

            memberByNamespace += (namespace.toLowerCase -> new ApexNamespace2(namespace))

        }
        memberByNamespace.result()
    }
    def getNamespace(namespace: String): Option[Member] = {
        memberByNamespace.get(namespace.toLowerCase)
    }

    /**
     *
     * @param fqn, possible values look like
     *           String or System.String
     * @return
     */
    def getTypeMember(fqn: String): Option[Member] = {
        if (fqn.indexOf(".") > 0) {
            //this is probably a fully qualified name
            val types = getMembers(fqn.split("\\.")(0))
            return types.find(_.getSignature.toLowerCase == fqn.toLowerCase) match {
                case Some(member) => Some(member)
                case None => None
            }
        } else {
            //check if this is a system type
            getSystemTypeMember(fqn)
        }

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

    def getInstanceMembers(typeName: String): List[Member] = {
        //TODO
        /*
        ApexModel.getMembers("(instance methods)").find(_.getIdentity.toLowerCase == typeName.toLowerCase) match {
            case Some(typeMember) =>
                Some(typeMember)
            case None => None
        }
        */
        List()
    }
    def getSystemTypeMember(systemType: String): Option[Member] = {
        memberByNamespace.get("system") match {
            case Some(system) =>
                system.getChildren.find(_.getIdentity.toLowerCase == systemType.toLowerCase) match {
                    case Some(typeMember) =>
                        return Some(typeMember)
                    case None =>
                }
            case None =>
        }
        None

    }

    def getSystemTypeMembers(systemType: String, isStatic: Boolean): List[Member] = {
        getSystemTypeMember(systemType)  match {
            case Some(typeMember) =>
                if (isStatic) typeMember.asInstanceOf[ApexModel2Member].getStaticChildren else typeMember.getChildren
            case None => List()
        }
    }
}

trait ApexModel2Member extends Member {
    //override def getIdentity: String = ???

    //override def getSignature: String = ???

    //override def isStatic: Boolean = ???
    override def getVisibility: String = "public"

    override def getPath: String = getParent match {
        case Some(member) => member.getIdentity
        case None => "System"
    }

    protected def isLoaded: Boolean = true

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
}
case class ApexNamespace2(name: String) extends ApexModel2Member {

    import ApexModelJsonProtocol._

    override def getIdentity: String = name

    override def getSignature: String = getIdentity
    override def isStatic: Boolean = true

    private var isDoneLoading = false
    override def isLoaded:Boolean = isDoneLoading

    override def loadMembers(): Unit = {
        loadFile(name)
        if ("System" == name) {
            //add Exception to System namespace
            loadFile("Exception")
            //add methods from System.System
            getChild("System") match {
              case Some(systemMember) =>
                  systemMember.getChildren.foreach(this.addChild)
              case None =>
            }
        }
        isDoneLoading = true
    }

    private def loadFile(namespace: String): Unit = {
        val is = getClass.getClassLoader.getResource("apex-doc/" + namespace + ".json")
        val doc = scala.io.Source.fromInputStream(is.openStream()).getLines().mkString
        val jsonAst = JsonParser(doc)
        val types = jsonAst.asJsObject.fields //Map[typeName -> type description JSON]
        val typesWithSuperTypes = List.newBuilder[ApexType2]
        for (typeName <- types.keys) {
            val typeJson = types(typeName)
            println("typeName=" + typeName)
            val apexTypeMember = typeJson.convertTo[ApexType2]

            apexTypeMember.parent = Some(this)
            addChild(apexTypeMember)

            //if current Apex Type has a super type then extend it accordingly
            if (apexTypeMember.superType.isDefined) {
                typesWithSuperTypes += apexTypeMember
        }
        }
        //now when we loaded everything - process types that have super types
        for (apexTypeMember <- typesWithSuperTypes.result()) {
            apexTypeMember.superType match {
                case Some(_typeName) =>
                    //top-up with children from super type
                    getChild(_typeName) match {
                        case Some(superTypeMember) =>
                            superTypeMember.getChildren.foreach(apexTypeMember.addChild)
                        case None =>
                    }
                case None =>
    }
}
    }
}
case class ApexType2(name: String, superType: Option[String], methods: List[ApexMethod2], tag: String, ctors: List[ApexMethod2], fqn: String) extends ApexModel2Member {

    override def getIdentity: String = name
    override def getSignature: String = fqn
    override def isStatic: Boolean = false

    private var isDoneLoading = false
    override def isLoaded:Boolean = isDoneLoading
    override def loadMembers(): Unit = {
        for (method <- methods) {
            method.parent = Some(this)
            addChild(method)
        }
        isDoneLoading = true
    }
    override def getSuperType: Option[String] = superType
}
case class ApexMethod2(s: String, n: String, v: String, p: List[String], h: String, r: String, d: String) extends ApexModel2Member {
    override def getIdentity: String = n
    override def getSignature: String = d
    override def isStatic: Boolean = "1" == s

}

/*
case class ApexParam2(paramType: String) extends ApexModel2Member {
    override def getIdentity: String = paramType
    override def getSignature: String = paramType
    override def isStatic: Boolean = false

}
*/
