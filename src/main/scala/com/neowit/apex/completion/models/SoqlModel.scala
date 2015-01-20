package com.neowit.apex.completion.models

import com.neowit.apex.parser.Member
import spray.json.{JsValue, JsonFormat, DefaultJsonProtocol}

object SoqlModelJsonProtocol extends DefaultJsonProtocol {
    //implicit val namespaceFormat = jsonFormat1(ApexNamespace)
    implicit val soqlTypeFormat: JsonFormat[SoqlType] = lazyFormat(jsonFormat(SoqlType, "name", "methods", "tag", "fqn"))
    implicit val soqlAggregateFunction: JsonFormat[SoqlFunction] = lazyFormat(jsonFormat(SoqlFunction, "n", "v", "p", "h", "r", "d"))
    //implicit val apexParamFormat: JsonFormat[ApexParam2] = jsonFormat(ApexParam2)
}
object SoqlModel extends ModelBase {

    private val NAMESPACES = List("SOQL" )

    private val memberByNamespace: Map[String, ApexModelMember] = load()

    override def getNameSpaces: List[String] = NAMESPACES


    override def getNamespaceInstance(namespace: String): Namespace = new SoqlNamespace(namespace)

    override def getMemberByNamespace: Map[String, ApexModelMember] = memberByNamespace

    def getMember(fqn: String): Option[Member] = {
        if (fqn.indexOf(".") > 0) {
            //this is probably a fully qualified name
            val types = getMembers(fqn.split("\\.")(0))
            return types.find(_.getSignature.toLowerCase == fqn.toLowerCase) match {
                case Some(member) => Some(member)
                case None => None
            }
        } else {
            //check if this is a system type
            getMember("SOQL." + fqn)
        }
    }
}

case class SoqlNamespace(name: String) extends Namespace {
    import com.neowit.apex.completion.models.SoqlModelJsonProtocol._

    override def getName: String = name

    private var isDoneLoading = false
    override def isLoaded:Boolean = isDoneLoading
    override def loadMembers(): Unit = {
        isDoneLoading = true //must do it here because loadFile calls getChild and isDoneLoading = false causes infinite loop
        super.loadMembers()
        loadFile("hand-made/SOQL")
    }

    override protected def loadTypes(types: Map[String, JsValue], overwriteChildren: Boolean): Unit = {

        for (typeName <- types.keys) {
            val typeJson = types(typeName)
            //println("typeName=" + typeName)
            val apexModelMember = typeJson.convertTo[SoqlType]

            apexModelMember.setParent(this)
            addChild(apexModelMember, overwriteChildren)

        }
    }
}

case class SoqlType(name: String,  methods: Option[List[SoqlFunction]], tag: String, fqn: String) extends ApexModelMember {

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
    }
}

case class SoqlFunction(n: String, v: String, p: List[String], h: String, r: String, d: String) extends ApexModelMember {

    override def getIdentity: String = n + p.mkString(", ")

    /**
     * for most member types Identity is unique (for Methods, Aggregate functions, and Inner Classes it is not)
     */
    override def getIdentityToDisplay: String = n

    override def getSignature: String = d
    override def isStatic: Boolean = true
    override def getDoc: String = h

    override def getType: String = r

    private var isDoneLoading = false
    override def isLoaded:Boolean = isDoneLoading
    override def loadMembers(): Unit = {
        isDoneLoading = true //must do it here because loadFile calls getChild and isDoneLoading = false causes infinite loop
        super.loadMembers()
    }
}
