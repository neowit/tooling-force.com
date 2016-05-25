package com.neowit.apex.completion.models

import com.neowit.apex.parser.{BuiltInMethodMember, Member}
//import scala.collection.JavaConversions._
import spray.json._

trait ApexModelJsonProtocol extends DefaultJsonProtocol {
    implicit val apexTypeFormat: JsonFormat[ApexType] = lazyFormat(jsonFormat(ApexType, "name", "superType", "enums", "methods", "tag", "ctors", "fqn"))
    implicit val apexMethodFormat: JsonFormat[ApexMethod] = lazyFormat(jsonFormat(ApexMethod, "s", "n", "v", "p", "h", "r", "d"))
    implicit val apexEnumFormat: JsonFormat[ApexEnumMember] = lazyFormat(jsonFormat(ApexEnumMember, "name", "enumConstants", "tag", "fqn"))
    implicit val apexEnumConstantFormat: JsonFormat[ApexEnumConstantMember] = lazyFormat(jsonFormat(ApexEnumConstantMember, "s", "n", "v", "h", "r"))
    implicit val apexAnnotationFormat: JsonFormat[ApexAnnotationMember] = lazyFormat(jsonFormat(ApexAnnotationMember, "name", "annotations", "tag", "fqn"))
    implicit val apexAnnotationConstantFormat: JsonFormat[ApexAnnotationConstantMember] = lazyFormat(jsonFormat(ApexAnnotationConstantMember, "n", "h", "p", "d"))
}

object ApexModel {

    private val NAMESPACES = List("Apex", "ApexPages", "applauncher", "Approval", "Auth", "cache", "Canvas", "ChatterAnswers", "ConnectApi", "Database", "Datacloud", "dom", "Flow", "KbManagement", "Messaging", "Process", "QuickAction", "reports", "Schema", "Search", "Site", "Support", "System", "TerritoryMgmt", "TxnSecurity", "UserProvisioning" )

    private val memberByNamespace: Map[String, ApexModelMember] = load()

    private def load(): Map[String, ApexModelMember] = {
        val memberByNamespace = Map.newBuilder[String, ApexModelMember]
        for (namespace <- NAMESPACES) {
            memberByNamespace += (namespace.toLowerCase -> new ApexNamespace(namespace))
        }

        memberByNamespace.result()
    }
    def getNamespace(namespace: String): Option[Member] = {
        memberByNamespace.get(namespace.toLowerCase)
    }

    /**
     *
     * @param fqn, short of fully qualified name,
     *           possible values look like: String or System.String
     * @return
     */
    def getTypeMember(fqn: String): Option[Member] = {
        if (fqn.indexOf(".") > 0) {
            //this is probably a fully qualified name
            val types = getMembers(fqn.split("\\.")(0))

            types.find(_.getSignature.toLowerCase == fqn.toLowerCase) match {
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

    def getSystemTypeMember(systemType: String): Option[Member] = {
        getTypeMember("System." + systemType)
    }

    def getSystemTypeMembers(systemType: String, isStatic: Boolean): List[Member] = {
        getSystemTypeMember(systemType)  match {
            case Some(typeMember) =>
                if (isStatic) typeMember.asInstanceOf[ApexModelMember].getStaticChildren else typeMember.getChildren
            case None => List()
        }
    }
}


class ApexNamespace(name: String) extends GenericNamespace(name) with ApexModelJsonProtocol {

    override def getIdentity: String = name

    override def getSignature: String = getIdentity
    override def isStatic: Boolean = true

    private var isDoneLoading = false
    override def isLoaded:Boolean = isDoneLoading

    //some methods are spread between namespaces,
    //e.g. Database classes are in Database namespace, while Database methods are in System.Database class
    override def getChildren: List[Member] = {
        val myChildren = super.getChildren
        //check if System namespace has class with the name of current namespace
        val extraMembers = if ("System" != name) {
            ApexModel.getSystemTypeMembers(name, isStatic = true)
        } else List()

        myChildren ++ extraMembers
    }

    override def loadMembers(): Unit = {
        isDoneLoading = true //must do it here because loadFile calls getChild and isDoneLoading = false causes infinite loop
        loadFile(name)
        if ("System" == name) {
            //add Exception to System namespace
            loadFile("hand-made/System_Exception")
            //add enum constant methods to System namespace
            loadFile("hand-made/System_Enum")
            //add StatusCode enum to System namespace
            loadFile("hand-made/System_StatusCode")
            //add Trigger to System namespace
            loadFile("hand-made/System_Trigger")
            //add Annotations to System namespace
            loadFile("hand-made/System_Annotations")
            //add methods from System.System
            getChild("System") match {
                case Some(systemMember) =>
                    systemMember.getChildren.foreach(this.addChild)
                case None =>
            }
            //override erroneous System namespace definitions
            loadFile("hand-made/System_ApexPages", overwriteChildren = true)
        } else if ("ApexPages" == name) {
            //add ApexPages.Severity
            loadFile("hand-made/ApexPages_Severity", overwriteChildren = true)
        }
    }

    protected def loadTypes(types: Map[String, JsValue], overwriteChildren: Boolean) {
        val typesWithSuperTypes = List.newBuilder[ApexType]
        for (typeName <- types.keys) {
            val typeJson = types(typeName)
            //println("typeName=" + typeName)
            val apexModelMember =
                if (typeJson.asJsObject.getFields("tag").head.toString().contains("ENUMDEF"))
                    typeJson.convertTo[ApexEnumMember]
                else if (typeJson.asJsObject.getFields("tag").head.toString().contains("ANNOTATION"))
                    typeJson.convertTo[ApexAnnotationMember]
                else //assume CLASSDEF
                    typeJson.convertTo[ApexType]

            apexModelMember.setParent(this)
            addChild(apexModelMember, overwriteChildren)

            //if current Apex Type has a super type then extend it accordingly

            apexModelMember match {
                case apexTypeMember: ApexType =>
                    if (apexTypeMember.getSuperType.isDefined) {
                        typesWithSuperTypes += apexTypeMember
                    }
                case _ =>
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

case class ApexType(name: String, superType: Option[String], enums: Option[List[ApexEnumMember]],
                    methods: Option[List[ApexMethod]], tag: String, ctors: Option[List[ApexMethod]], fqn: String) extends ApexModelMember {

    override def getIdentity: String = name
    override def getSignature: String = fqn
    override def isStatic: Boolean = false

    private var isDoneLoading = false
    override def isLoaded:Boolean = isDoneLoading
    override def loadMembers(): Unit = {
        isDoneLoading = true //must do it here because loadFile calls getChild and isDoneLoading = false causes infinite loop
        methods match {
            case Some(_methods) =>
                for (method <- _methods) {
                    //method.setParent(this)
                    addChild(method)
                }
            case None =>
        }
        enums match {
            case Some(_enums) =>
                for (enumName <- _enums) {
                    //method.setParent(this)
                    addChild(enumName)
                }
            case None =>
        }
    }

    override def getSuperType: Option[String] = superType
}

/**
 *
 * @param s - if static then  = 1
 * @param n - method name
 * @param v - method visibility
 * @param p - list of parameter types
 * @param h - help doc
 * @param r - return value type, e.g. "String"
 * @param d - signature, e.g. "public void addError(Exception exceptionError, Boolean escape)"
 */
case class ApexMethod(s: String, n: String, v: String, p: List[String], h: String, r: String, d: String) extends ApexModelMember {
    override def getIdentity: String = n + p.mkString(", ")

    /**
     * for most member types Identity is unique (for Methods and Inner Classes it is not)
     */
    override def getIdentityToDisplay: String = n

    override def getSignature: String = d
    override def isStatic: Boolean = "1" == s
    override def getDoc: String = h

    override def getType: String = {
        val initialType = r
        //remove all garbage - from "Set (of same type)" keep only "Set"
        initialType.replaceAllLiterally(" (of same type)", "")
    }
}

/**
 * description of parameter list @see ApexMethod
 */
case class ApexEnumConstantMember(s: Option[String], n: String, v: Option[String], h: String, r: String) extends ApexModelMember {
    override def getVisibility: String = v.getOrElse("public").toLowerCase

    override def getIdentity: String = n

    override def getSignature: String = getIdentity
    override def isStatic: Boolean = "1" == s.getOrElse("1")
    override def getDoc: String = h

    override def getType: String = r

    addChild(new BuiltInMethodMember(this, "name", "name", "String")) //add default name() method of enum constant
    addChild(new BuiltInMethodMember(this, "ordinal", "ordinal", "Integer")) //add default ordinal() method of enum constant
}

case class ApexEnumMember(name: String, enumConstants: List[ApexEnumConstantMember],
                          tag: String, fqn: String) extends ApexModelMember {
    override def getVisibility: String = "public"

    override def getIdentity: String = name

    override def getSignature: String = fqn
    override def isStatic: Boolean = true

    private var isDoneLoading = false
    override def isLoaded:Boolean = isDoneLoading
    override def loadMembers(): Unit = {
        isDoneLoading = true //must do it here because loadFile calls getChild and isDoneLoading = false causes infinite loop
        for (enumConstant <- enumConstants) {
            //method.setParent(this)
            addChild(enumConstant)
        }
        addChild(new BuiltInMethodMember(this, "values", "values", "List<" + getType + ">")) //add default values() method of enum
        addChild(new BuiltInMethodMember(this, "equals", "equals", "Boolean", List("Object obj"))) //add default equals() method of enum
        addChild(new BuiltInMethodMember(this, "hashCode", "hashCode", "Integer")) //add default equals() method of enum
    }
}

case class ApexAnnotationConstantMember(n: String, h: String, p: Option[List[String]], d: String) extends ApexModelMember {
    override def getVisibility: String = "public"

    override def getIdentity: String = n + (p match {
                                              case Some(params) if params.nonEmpty => "(" + params.mkString(", ") + ")"
                                              case None => ""
                                            })

    override def getSignature: String = d
    override def isStatic: Boolean = true
    override def getDoc: String = h

    override def getType: String = "Annotation"
}

case class ApexAnnotationMember(name: String, annotations: List[ApexAnnotationConstantMember],
                          tag: String, fqn: String) extends ApexModelMember {
    override def getVisibility: String = "public"

    override def getIdentity: String = name

    override def getSignature: String = fqn
    override def isStatic: Boolean = true

    private var isDoneLoading = false
    override def isLoaded:Boolean = isDoneLoading
    override def loadMembers(): Unit = {
        isDoneLoading = true //must do it here because loadFile calls getChild and isDoneLoading = false causes infinite loop
        for (annotationConstant <- annotations) {
            //method.setParent(this)
            addChild(annotationConstant)
        }
    }
}
