package com.neowit.apex.parser

import java.nio.file.Path

import com.neowit.apex.parser.antlr.ApexcodeParser._
import org.antlr.v4.runtime.{CommonTokenStream, ParserRuleContext}
import org.antlr.v4.runtime.tree.{ParseTree, TerminalNode, TerminalNodeImpl}
import com.neowit.apex.parser.antlr.{ApexcodeLexer, ApexcodeParser}

import scala.collection.mutable
import scala.collection.JavaConversions._
import spray.json._
import DefaultJsonProtocol._

case class Location ( file: Path, line: Option[Int], column: Option[Int], identity: String )

object Location extends DefaultJsonProtocol {
    def apply(filePath: Path, ctx: ParserRuleContext, identity: String): Option[Location] = {
        Option(Location(filePath, Option(ctx.getStart.getLine), Option(ctx.getStart.getCharPositionInLine), identity))
    }

    implicit object LocationJsonFormat extends RootJsonFormat[Location] {
        def write(c: Location) =
            Map(
                "filePath" -> c.file.toString.toJson,
                "line" -> c.line.getOrElse(-1).toJson,
                "column" -> c.column.getOrElse(-1).toJson,
                "identity" -> c.identity.toJson
            ).toJson

        def read(value: JsValue) = value match {
            case _ => deserializationError("Location deserialisation is NOT supported")
        }
    }
}

trait AnonymousMember {
    private var parent: Option[AnonymousMember] = None
    private var apexTree: Option[ApexTree] = None

    def isParentChangeAllowed: Boolean = false

    def setParent(member: AnonymousMember) = {
        if (parent.isDefined && !isParentChangeAllowed) {
            throw new IllegalAccessError("can not set parent of Member twice")
        } else {
            parent = Some(member)
        }
    }
    def getParent: Option[AnonymousMember] = parent

    def setApexTree(tree: ApexTree) = {
        if (apexTree.isDefined) {
            throw new IllegalAccessError("can not set apexTree of Member twice")
        } else {
            apexTree = Some(tree)
        }
    }

    def getApexTree: ApexTree = {
        if (apexTree.isDefined) {
            apexTree.get
        } else {
            //println("Apex Tree is not defined for member: " + this.getIdentityToDisplay)
            new ApexTree()
        }
    }

    /**
     * find parent which matches specified filter
     * @param filter - condition
     * @return
     */
    def findParent(filter: (AnonymousMember) => Boolean): Option[AnonymousMember] = {
        getParent match {
            case Some(parentMember) =>
                if (filter(parentMember)) Some(parentMember) else parentMember.findParent(filter)
            case None => None
        }
    }

    /**
     * @return class in which current member is defined
     */
    def getClassMember: Option[ClassLikeMember] = {
        def getMyClass[T <: AnonymousMember](m: Option[AnonymousMember]): Option[AnonymousMember] = {
            m match {
                case Some(member) =>
                    if (member.isInstanceOf[ClassLikeMember]) {
                        Some(member)
                    } else {
                        getMyClass(member.getParent)
                    }
                case None => None
            }
        }
        getMyClass(Some(this)) match {
            case Some(member) => Some(member.asInstanceOf[ClassLikeMember])
            case None => None
        }
    }

    def getTopMostClassMember: Option[ClassLikeMember] = {
        if (this.isInstanceOf[ClassLikeMember] && !this.isInstanceOf[InnerClassLikeMember]) {
            return Some(this.asInstanceOf[ClassLikeMember])
        }
        def getParentByType[T <: AnonymousMember](m: AnonymousMember, filter: AnonymousMember => Boolean): Option[AnonymousMember] = {
            m.getParent match {
                case Some(member) =>
                    if (filter(member)) {
                        Some(member)
                    } else {
                        getParentByType(member, filter)
                    }
                case None => None
            }
        }
        getParentByType(this, (m) => m.getClass == classOf[ClassMember] || m.getClass == classOf[InterfaceMember]) match {
            case Some(member) => Some(member.asInstanceOf[ClassLikeMember])
            case None => None
        }
    }
    /**
     * using provided typeName check if this type is defined in the current Outer Class
     * @param typeName, e.g. MyClass or String
     * @return
     */
    def findMemberByTypeLocally(typeName: String): Option[AnonymousMember] = {
        getTopMostClassMember match {
            case Some(outerClassMember) =>
                outerClassMember.getInnerClassByType(typeName)
            case None => None
        }
    }

    def getSuperType: Option[String] = None

    def getFullSuperType: Option[String] = None

    private val children = mutable.HashMap[String, Member]()
    protected def clearChildren() = children.clear()

    def addChild(member: AnonymousMember): Unit = {
        addChild(member, overwrite = false)
    }
    def addChild(member: AnonymousMember, overwrite: Boolean = false) {
        member.getParent match {
            case Some(_parent) if _parent.equals(this) => //do nothing
            case _ => member.setParent(this)
        }
        //do not add AnonymousMember to the default list of children
        member match {
            case m: Member =>
                try {
                    val canAdd = overwrite || (getChild(m.getIdentity.toLowerCase, withHierarchy = false) match {
                        case Some(_existingMember) =>
                            //this child already exists, do not overwrite
                            false
                        case None => true
                    })

                    if (canAdd) {
                        children.+=(m.getIdentity.toLowerCase -> m)
                    }
                } catch {
                    case ex:Throwable =>
                        this match {
                            case m: Member =>
                                println("failed to add member of parent: " + m.getIdentity)
                                println(ex.getStackTrace)
                            case _ =>
                                println("failed to add member of parent")
                        }
                }
            case _ => //do nothing
        }
    }

    def getChildren: List[Member] = {
        children.values.toList
    }

    /**
     * this method is only relevant when current member is a Class or Inner Class
     * use this method to get children of this member and super type members
     * @param apexTree previously generated parse tree
     * @return
     */
    def getChildrenWithInheritance(apexTree: ApexTree): List[Member] = {
        getSuperType match {
            case Some(superType) =>
                apexTree.getClassMemberByType(superType).orElse(findMemberByTypeLocally(superType)) match {
                    case Some(superTypeMember) =>
                        val superTypeChildren = superTypeMember.getChildrenWithInheritance(apexTree)
                        val myChildrenMap = getChildren.map(m => m.getIdentity.toLowerCase -> m).toMap
                        val superTypeChildrenMinusMyChildren = superTypeChildren.filterNot(
                            superChild => myChildrenMap.containsKey(superChild.getIdentity.toLowerCase)
                        )
                        superTypeChildrenMinusMyChildren ++ getChildren
                    case None => getChildren
                }
            case None => getChildren
        }
    }

    def getChild(identity : String, withHierarchy: Boolean = true): Option[Member] = {

        def findChildHierarchically(identity: String, apexTree: ApexTree = new ApexTree): Option[Member] = {
            getSuperType match {
                case Some(superType) =>
                    //first check if superType is an inner class in the current main/outer class
                    val innerSuperClassMember = getParent match {
                        case Some(parentMember:ClassLikeMember) =>
                            parentMember.getInnerClassByType(superType)
                        case _ => None
                    }
                    innerSuperClassMember match {
                        case Some(member) => member.getChild(identity)
                        case None =>
                            //fall back to the main tree
                            apexTree.getClassMemberByType(superType) match {
                                case Some(superTypeMember) =>
                                    superTypeMember.getChild(identity)
                                case None => None
                            }
                    }

                case None => None
            }
        }

        getChildInternal(identity) match {
            case Some(childMember) => Some(childMember)
            case None =>
                if (withHierarchy) findChildHierarchically(identity.toLowerCase, getApexTree) else None
        }
    }

    protected def getChildInternal(identity: String): Option[Member] = {
        children.get(identity.toLowerCase)
    }
}

class StatementMember(ctx: StatementContext) extends AnonymousMember


trait Member extends AnonymousMember {

    /**
     * @return
     * for class it is class name
     * for method it is method name + string of parameter types
     * for variable it is variable name
     * etc
     */
    def getIdentity:String

    /**
      * @return position of this member in the file system and inside file
      */
    def getLocation: Option[Location]

    /**
      * for cases where member with same name resides in different locations (e.g. methods of one class with same name but different parameters)
      */
    def getLocations: List[Location] = {
        getLocation match {
            case Some(location) => List(location)
            case None => Nil
        }
    }

    def serialise: JsValue = {
        getLocations.toJson
    }

    /**
     * for most member types Identity is unique (for Methods and Inner Classes it is not)
     */
    def getIdentityToDisplay:String = getIdentity

    def getSignature:String
    def getType: String

    //e.g. MyClass.InnerClass
    def getFullType: String = getType

    def getDoc: String = " " //TODO - implement documentation retrieval

    def getVisibility: String = "private" //TODO

    def isStatic: Boolean

    /*
    def getPath: String = {

        def resolveFullPath(currentPath: String): String = getParent match {
            case Some(member) => member.getPath + "." + currentPath
            case None => currentPath
        }
        val res = resolveFullPath(getIdentity)
        res
    }
    */

    override def toString: String = {
        //val txt = getIdentity
        val txt = getSignature + " => type=" + getType + "; id=" + getIdentity
        val chlds = new StringBuilder()
        for (x <- getChildren) {
            chlds.++= (x.toString())
        }
        if (chlds.nonEmpty )
            txt + ": \n - " + chlds.toString()
        else
            txt + "\n - "
    }

    def toJson: JsValue = {
        val data = Map("identity" -> getIdentityToDisplay, "realIdentity" -> getIdentity,
            "signature" -> getSignature, "type" -> getType,
            "visibility" -> getVisibility, "doc" -> getDoc)
        //JSONObject(data)
        data.toJson
    }

    override def equals(o: Any): Boolean = {
        o.isInstanceOf[Member] && this.getClass == o.getClass &&
            this.getIdentity.toLowerCase == o.asInstanceOf[Member].getIdentity.toLowerCase
    }
    override def hashCode = getIdentity.toLowerCase.hashCode

}

case class CompoundMember(members: List[Member]) extends Member {
    /**
      * @return
      * for class it is class name
      * for method it is method name + string of parameter types
      * for variable it is variable name
      * etc
      */
    override def getIdentity: String = members.head.getIdentity

    /**
      * @return position of this member in the file system and inside file
      */
    override def getLocation: Option[Location] = None

    /**
      * for cases where member with same name resides in different locations (e.g. methods of one class with same name but different parameters)
      */
    override def getLocations: List[Location] = members.filter(_.getLocation.isDefined).map(_.getLocation.get)

    override def getSignature: String = members.head.getSignature

    override def getType: String = members.head.getType

    override def isStatic: Boolean = members.head.isStatic
}
/**
 * some types (e.g. enum) have default members, defined on the system level, in addition to user defined members
 * @param parent - member to which this built-in member belongs
 * @param identity - e.g. "getChild"
 * @param displayIdentity e.g. "getChild()"
 * @param retType - e.g. "SomeClass"
 * @param signature - e.g. "public SomeClass getChild(Integer)
 * @param isStaticMember - true if this member is static
 */
class BuiltInMember(parent: AnonymousMember, identity: String, displayIdentity: String, retType: String,
                    signature: Option[String] = None, isStaticMember: Boolean = false) extends Member {
    setParent(parent)

    override def getIdentity: String = identity
    override def getIdentityToDisplay: String = displayIdentity

    override def getType: String = retType

    override def getSignature: String = signature match {
        case Some(s) => s
        case None => "public " + getType + " " + getIdentityToDisplay
    }

    override def isStatic: Boolean = isStaticMember

    /**
      * @return position of this member in the file system and inside file
      */
    override def getLocation: Option[Location] = None
}

class BuiltInMethodMember(parent: Member, identity: String, displayIdentity: String, retType: String,
                          params: List[String] = List(),
                          signature: Option[String] = None, isStaticMember: Boolean = false)
    extends BuiltInMember(parent, identity, displayIdentity, retType, signature, isStaticMember) {

    override def getSignature: String = signature match {
        case Some(s) => s
        case None => "public " + getType + " " + getIdentityToDisplay + "(" + params.mkString(", ") + ")"
    }

    /**
      * @return position of this member in the file system and inside file
      */
    override def getLocation: Option[Location] = None
}

/**
 * parent for Class & Interface members
 * @param ctx - ParserRuleContext must be either ClassDeclarationContext or InterfaceDeclarationContext
 */
abstract class ClassLikeMember(ctx: ParserRuleContext, filePath: Path) extends Member {
    private val innerClassByClassName = new mutable.HashMap[String, InnerClassLikeMember]() //inner-class-name.toLowerCase => InnerClassMember
    private val enumByName = new mutable.HashMap[String, EnumMember]() //enum-name.toLowerCase => EnumMember

    // methodsByName used to record multiple methods have same name but different params
    private val methodsByName = mutable.HashMap[String, List[MethodMember]]() // method-name.toLowerCase => List(MethodMember)

    override def getType: String = getIdentity
    override def isStatic: Boolean = false

    def getSignature: String = {
        ApexParserUtils.getChildren[TerminalNode](ctx, n => n.isInstanceOf[TerminalNode]).map(_.getText).mkString(" ")
    }

    //TODO add proper support for "implements"
    override def getSuperType: Option[String] = {
        //if one of ctx children is "extends" then the next one will be TypeContext with the name of super class
        val extendIndex = ApexParserUtils.findChildIndex(ctx, _ctx => "extends" == _ctx.getText.toLowerCase )
        val index = if (extendIndex > 0 && ctx.getChildCount > extendIndex) extendIndex else {
            ApexParserUtils.findChildIndex(ctx, _ctx => "implements" == _ctx.getText.toLowerCase )
        }

        if (index > 0 && ctx.getChildCount > index) {
            val superTypeContext = ctx.getChild(index + 1) //TypeContext goes after TerminalNode "extend"
            if (null != superTypeContext) {
                return Some(superTypeContext.getText)
            }
        }
        None
    }
    override def getFullSuperType: Option[String] = this.getSuperType

    override def addChild(member: AnonymousMember): Unit = {
        super.addChild(member)
        member match {
            case m: InnerClassLikeMember =>
                innerClassByClassName += (m.getType.toLowerCase -> m)
            case m: EnumMember =>
                enumByName += (m.getType.toLowerCase -> m)
            case m: MethodMember =>
                val methodName = m.getMethodName.toLowerCase
                methodsByName.get(methodName) match {
                    case Some(methodMembers) =>
                        methodsByName += (methodName -> (m :: methodMembers))
                    case None =>
                        methodsByName += (methodName -> List(m))
                }
            case _ =>
        }
    }
    def getInnerClassByType(innerClassTypeName: String): Option[Member] = {
        innerClassByClassName.get(innerClassTypeName.toLowerCase) match {
            case Some(m: InnerClassLikeMember) => Some(m)
            case _ => None
        }
    }

    /**
     * enum Seasons (WINTER, SPRING, SUMMER, FALL)
     * map enumByName contains keys that look like this: Seasons -> EnumMember
     *
     * @param enumTypeName - can be one of two forms:
     *                     Seasons - resolved directly from enumByName map
     *                     Seasons.Winter - requires two step resolution:
     *                      1. get EnumMember by name "Seasons"
     *                      2. get child of EnumMember by name "Winter"
     * @return
     */
    def getEnumByName(enumTypeName: String): Option[Member] = {
        if (enumTypeName.indexOf(".") > 0) {
            val path = enumTypeName.split("\\.")
            getEnumByName(path(0)) match {
                case Some(enumMember) => enumMember.getChild(path(1))
                case None => None
            }
        } else {
            enumByName.get(enumTypeName.toLowerCase)
        }
    }

    def getMethodsByName(methodName: String): List[Member] = {
        methodsByName.getOrElse(methodName.toLowerCase, Nil)
    }

    override protected def getChildInternal(identity: String): Option[Member] = {
        super.getChildInternal(identity).orElse{
            // check if identity represents method name
            getMethodsByName(identity) match {
                case Nil =>
                    None
                case methods =>
                    // return compound member
                    //super.getChildInternal(identity) //TODO
                    Option(CompoundMember(methods))
            }
        }
    }

    /**
     * if this class extends another class then find Member of that other class
     * @return
     */
    def getSuperClassMember: Option[ClassLikeMember] = {
        this.getFullSuperType match {
            case Some(fullSuperType) => getApexTree.getClassMemberByType(fullSuperType)
            case None => None
        }
    }

    /**
     * check if given otherClassMember is a super class of current member
     * @param otherClassMember - class member to test against
     */
    def isInheritFrom (otherClassMember: ClassLikeMember): Boolean = {
        if (otherClassMember == this) {
            true
        } else {
            this.getSuperClassMember match {
                case Some(superClassMember) => superClassMember.isInheritFrom(otherClassMember)
                case None => false
            }
        }

    }

    /**
      * @return position of this member in the file system and inside file
      */
    override def getLocation: Option[Location] = {
        Location(filePath, ctx, getIdentity)
    }
}

case class ClassMember(ctx: ClassDeclarationContext, filePath: Path) extends ClassLikeMember(ctx, filePath) {

    def getIdentity:String = {
        //ctx.getToken(ApexcodeParser.Identifier, 0).getText
        ctx.Identifier().getText
    }

    override def getVisibility: String = {
        //go up the tree (if needed) to get to ClassBodyDeclarationContext and from there find visibility
        ApexParserUtils.getParent(ctx, classOf[ClassBodyDeclarationContext]) match {
            case Some(member) => ClassBodyMember.getVisibility(member)
            case None => "private"
        }
    }

    override protected def getChildInternal(identity: String): Option[Member] = {
        // check if identity represents inner class
        super.getChildInternal( InnerClassMember.getIdentityPrefix + identity )
            .orElse(super.getChildInternal(identity))
    }
}



case class InterfaceMember(ctx: InterfaceDeclarationContext, filePath: Path) extends ClassLikeMember(ctx, filePath) {
    def getIdentity:String = {
        //ctx.getToken(ApexcodeParser.Identifier, 0).getText
        ctx.Identifier().getText
    }
    override def getType: String = getIdentity

    override def getSignature: String = {
        ApexParserUtils.getChildren[TerminalNode](ctx, n => n.isInstanceOf[TerminalNode]).map(_.getText).mkString(" ")
    }

    override def isStatic: Boolean = false

    override protected def getChildInternal(identity: String): Option[Member] = {
        // check if identity represents inner class
        super.getChildInternal( InnerInterfaceMember.getIdentityPrefix + identity )
            .orElse(super.getChildInternal(identity))
    }
}

abstract class InnerClassLikeMember(ctx: ParserRuleContext, filePath: Path) extends ClassLikeMember(ctx, filePath) {
    protected def getIdentityPrefix: String

    override def getSignature: String = {
        val clsBodyDeclaration = ctx.getParent.getParent
        ApexParserUtils.findChildren(clsBodyDeclaration, classOf[TerminalNodeImpl]).map(_.getText).mkString(" ")
    }
    override def getFullType: String = {
        getParent match {
            case Some(parentMember: Member) if !this.getType.startsWith(parentMember.getType + ".") =>
                parentMember.getType + "." + this.getType
            case _ => super.getFullType
        }
    }

    override def getFullSuperType: Option[String] = {
        //if this class extends another inner class then need to resolve it as Outer.Inner
        super.getSuperType match {
            case Some(superTypeName) =>
                if (superTypeName.indexOf(".") > 0) {
                    //already fully qualified type name
                    Some(superTypeName)
                } else {
                    //check if this is another class of the current ClassMember class
                    getParent match {
                        case Some(classMember: Member) =>
                            classMember.asInstanceOf[ClassLikeMember].getInnerClassByType(superTypeName) match {
                                case Some(anotherInnerClassMember) =>
                                    //current superTypeName belongs to inner class of the same Outer class as current InnerClass
                                    Some(classMember.getType + "." + superTypeName)
                                case None =>
                                    //superTypeName is not a member of current outer class, so have to assume that superTypeName
                                    // is fully qualified, otherwise it will not compile
                                    Some(superTypeName)
                            }
                        case _ => None
                    }
                }
            case None => None
        }
    }

    /**
      * @return position of this member in the file system and inside file
      */
    override def getLocation: Option[Location] = {
        Location(filePath, ctx, getIdentityToDisplay)
    }
}

object InnerClassMember {
    def getIdentityPrefix: String = "InnerClass:"
}
class InnerClassMember(ctx: ClassDeclarationContext, filePath: Path) extends InnerClassLikeMember(ctx, filePath) {

    protected def getIdentityPrefix: String = InnerClassMember.getIdentityPrefix

    override def getType: String = {
        val strType = ctx.Identifier().getText
        if (strType.indexOf("\\.") > 0) {
            //this is full type Outer.Inner - return only last bit - Inner
            strType.split("\\.").tail.head
        } else {
            strType
        }
    }
    override def getIdentity: String = this.getParent match {
        case Some(parentMember) => getIdentityPrefix +  ctx.Identifier().getText
        case None => "" //this case is only relevant when we are adding child to parent first time, as Inner Class requires parent to resolve identity
    }

    override def getIdentityToDisplay: String = ctx.Identifier().getText
}

object InnerInterfaceMember {
    def getIdentityPrefix: String = "InnerInterface:"
}
class InnerInterfaceMember(ctx: InterfaceDeclarationContext, filePath: Path) extends InnerClassLikeMember(ctx, filePath) {

    protected def getIdentityPrefix: String = InnerInterfaceMember.getIdentityPrefix

    override def getType: String = {
        val strType = ctx.Identifier().getText
        if (strType.indexOf("\\.") > 0) {
            //this is full type Outer.Inner - return only last bit - Inner
            strType.split("\\.").tail.head
        } else {
            strType
        }
    }
    override def getIdentity: String = this.getParent match {
        case Some(parentMember) => getIdentityPrefix +  ctx.Identifier().getText
        case None => "" //this case is only relevant when we are adding child to parent first time, as Inner Class requires parent to resolve identity
    }

    override def getIdentityToDisplay: String = ctx.Identifier().getText
}

object ClassBodyMember {

    def isInnerClass(ctx: ParseTree): Boolean = {
        ctx.getParent match {
            case pp:ApexcodeParser.MemberDeclarationContext => true
            case _ => false
        }
    }

    def isInnerInterface(ctx: ParseTree): Boolean = {
        ctx.getParent match {
            case pp:ApexcodeParser.MemberDeclarationContext => true
            case _ => false
        }
    }
    //def isMethodOfClass(ctx: ParseTree): Boolean = findChildren(ctx, classOf[ApexcodeParser.MethodDeclarationContext]).nonEmpty
    def isMethodOfClass(ctx: ParseTree): Boolean = {
        val memberDeclarations = ApexParserUtils.getChildren(ctx, _.isInstanceOf[MemberDeclarationContext])
        memberDeclarations.nonEmpty && ApexParserUtils.getChildren(memberDeclarations.head, _.isInstanceOf[MethodDeclarationContext]).nonEmpty
    }

    def isMethodOfInterface(ctx: ParseTree): Boolean = {
        val memberDeclarations = ApexParserUtils.getChildren(ctx, _.isInstanceOf[InterfaceMemberDeclarationContext])
        memberDeclarations.nonEmpty && ApexParserUtils.getChildren(memberDeclarations.head, _.isInstanceOf[InterfaceMethodDeclarationContext]).nonEmpty
    }

    def isEnum(ctx: ParseTree): Boolean = ApexParserUtils.findChildren(ctx, classOf[ApexcodeParser.EnumDeclarationContext]).nonEmpty
    def isField(ctx: ParseTree): Boolean = {
        ApexParserUtils.getChild[ApexcodeParser.MemberDeclarationContext](ctx, classOf[ApexcodeParser.MemberDeclarationContext]) match {
            case Some(memberDeclarationContext) =>
                ApexParserUtils.getChild[ApexcodeParser.FieldDeclarationContext](memberDeclarationContext, classOf[ApexcodeParser.FieldDeclarationContext])  match {
                    case Some(x) =>
                        true
                    case None => false
                }
            case None => false
        }
    }
    def isProperty(ctx: ParseTree): Boolean = ApexParserUtils.findChildren(ctx, classOf[ApexcodeParser.PropertyDeclarationContext]).nonEmpty

    def isStatic(ctx: ParseTree): Boolean = {
        //classOrInterfaceModifier-s
        val modifierContexts = ApexParserUtils.findChildren(ctx, classOf[ApexcodeParser.ClassOrInterfaceModifierContext])
        val x = modifierContexts.find(
            context => {
                val children = ApexParserUtils.getChildren[ParseTree](context)
                children.nonEmpty && "static" == children(0).getText
            })
        x.isDefined
    }

    val VISIBILITIES = Set("private", "public", "protected", "global", "webservice")

    def getVisibility(ctx: ParseTree): String = {
        ctx match {
            case _cxt: ClassBodyDeclarationContext =>
                val modifier = _cxt.modifier().find(
                    m => VISIBILITIES.contains(
                        m.classOrInterfaceModifier().getChild(0).getText.toLowerCase))
                modifier match {
                    case Some(modifierContext) => modifierContext.classOrInterfaceModifier().getChild(classOf[TerminalNodeImpl], 0).getText.toLowerCase
                    case None => "private"
                }
            case _ => "TODO"
        }
    }

    /**
     * find top most class declaration context compared to current context
     * @param ctx - context used as a start point of climbing parse tree
     * @return higher level class declaration if there is one
     */
    def getTopMostClassContext(ctx: ParseTree): Option[ParseTree] = {
        ApexParserUtils.getParent(ctx, classOf[ClassDeclarationContext]) match {
            case Some(classCtx) => getTopMostClassContext(classCtx)
            case None =>
                if (ctx.isInstanceOf[ClassDeclarationContext]) {
                    Some(ctx)
                } else {
                    None
                }
        }

    }

    /**
     * when type of current Member is Inner Class - resolve full inner class type
     * e.g.
     *  public InnerClass var;
     * will be resolved as
     *  public OuterClass.InnerClass var;
     *
     * @return
     */
    def getFullTypeIfTypeIsInnerClass(member: Member, parentMember: Option[AnonymousMember]): Option[String] = {
        parentMember match {
            case Some(_parentMember) =>
                _parentMember.getChild(member.getType) match {
                    case Some(_member) if _member.isInstanceOf[InnerClassLikeMember] =>
                        println("_member.getFullType=" + _member.getFullType)
                        Some(_member.getFullType)
                    case _ => None
                }
            case _ => None
        }
    }
}

abstract class ClassBodyMember(ctx: ClassBodyDeclarationContext) extends Member {


}

object EnumMember {
    def unapply(ctx: ParseTree): Option[ParseTree] = {
        if (ClassBodyMember.isEnum(ctx)) Some(ctx) else None
    }
}

class EnumMember(ctx: EnumDeclarationContext, filePath: Path) extends Member {
    addChild(new BuiltInMethodMember(this, "values", "values", "List<" + getType + ">")) //add default values() method of enum

    override def getIdentity:String = {
        ctx.Identifier().getText
    }

    override def getSignature: String = {
        ApexParserUtils.findChildren(ctx, classOf[TerminalNodeImpl]).filter(node => "{" != node.getText && "}" != node.getText).mkString(" ")
    }

    override def getType: String = getIdentity

    override def isStatic: Boolean = false

    /**
      * @return position of this member in the file system and inside file
      */
    override def getLocation: Option[Location] = {
        Location(filePath, ctx, getIdentity)
    }
}

/**
 * enum constant has only two methods: name(), ordinal()
 */
class BuiltInEnumConstantMethodMember(parent: EnumConstantMember, identity: String, displayIdentity: String, retType: String)
                                                                extends BuiltInMember(parent, identity, displayIdentity, retType)

class EnumConstantMember(ctx: EnumConstantContext, filePath: Path) extends Member {
    addChild(new BuiltInMethodMember(this, "name", "name", "String")) //add default name() method of enum constant
    addChild(new BuiltInMethodMember(this, "ordinal", "ordinal", "Integer")) //add default ordinal() method of enum constant
    /**
     * @return
     * for class it is class name
     * for method it is method name + string of parameter types
     * for variable it is variable name
     * etc
     */
    override def getIdentity: String = {
        ctx.Identifier().getText
    }

    /**
     * for most member types Identity is unique (for Methods and Inner Classes it is not)
     */
    override def getIdentityToDisplay: String = ctx.Identifier().getText

    override def getType: String = {
        getParent match {
            case Some(m) if m.isInstanceOf[EnumMember] => m.asInstanceOf[EnumMember].getType + "." + getIdentity
            case _ => ""
        }
    }

    override def getSignature: String = getType

    override def isStatic: Boolean = true

    /**
      * @return position of this member in the file system and inside file
      */
    override def getLocation: Option[Location] = Location(filePath, ctx, getIdentity)
}

object PropertyMember {
    def unapply(ctx: ParseTree): Option[ParseTree] = {
        if (ClassBodyMember.isProperty(ctx)) Some(ctx) else None
    }
    def getModifiers(ctx: ParseTree): List[String] = {
        ApexParserUtils.getParent(ctx, classOf[ClassBodyDeclarationContext]) match {
            case Some(classBodyDeclarationCtx) =>
                classBodyDeclarationCtx.modifier().filter(null != _.classOrInterfaceModifier()).map(m => m.classOrInterfaceModifier().getChild(0).getText).toList
            case None => List()
        }
    }
}

class PropertyMember(ctx: PropertyDeclarationContext, filePath: Path) extends Member {

    override def getIdentity: String = {
        ctx.variableDeclarators().variableDeclarator().find(null != _.variableDeclaratorId()) match {
            case Some(variableDeclaratorIdContext) => variableDeclaratorIdContext.getText
            case None => ""
        }
    }

    override def getType: String = {
        if (null != ctx && null != ctx.`type`()) {
            ctx.`type`().getText
        } else {
            "void"
        }
    }

    override def getSignature: String = {
        val modifiers = getModifiers.mkString(" ")
        modifiers + " " + getType + " " + getIdentity
    }

    private def getModifiers: List[String] = {
        PropertyMember.getModifiers(ctx)
    }

    private lazy val _isStatic = getModifiers.map(_.toLowerCase).indexOf("static") >=0

    override def isStatic: Boolean = _isStatic

    override def getVisibility: String = getModifiers.find(v => ClassBodyMember.VISIBILITIES.contains(v.toLowerCase)) match {
        case Some(visibility) => visibility.toLowerCase
        case None => "private"
    }

    /**
      * @return position of this member in the file system and inside file
      */
    override def getLocation: Option[Location] = Location(filePath, ctx, getIdentity)
}

object FieldMember {
    def unapply(ctx: ParseTree): Option[ParseTree] = {
        if (ClassBodyMember.isField(ctx)) Some(ctx) else None
    }
}
class FieldMember(ctx: FieldDeclarationContext, filePath: Path) extends Member {
    val fieldDeclarationContext = ctx

    override def getIdentity:String = {
        val fieldDeclarationContext = ApexParserUtils.findChildren(ctx, classOf[VariableDeclaratorIdContext])
        if (fieldDeclarationContext.nonEmpty) {
            fieldDeclarationContext.head.Identifier().getText
        } else ""
    }

    override def getType: String = {
        if (null != fieldDeclarationContext && null != fieldDeclarationContext.`type`()) {
            fieldDeclarationContext.`type`().getText
        } else {
            "void"
        }
    }

    override def getSignature: String = {
        val modifiers = getModifiers.mkString(" ")
        if (modifiers.nonEmpty) {
            modifiers + " " + getType + " " + getIdentity
        } else {
            getType + " " + getIdentity
        }
    }

    private def getModifiers: List[String] = {
        PropertyMember.getModifiers(ctx)
    }

    val _isStatic = getModifiers.map(_.toLowerCase).indexOf("static") >=0

    override def isStatic: Boolean = _isStatic

    override def getVisibility: String = getModifiers.find(v => ClassBodyMember.VISIBILITIES.contains(v.toLowerCase)) match {
      case Some(visibility) => visibility.toLowerCase
      case None => "private"
    }

    /**
      * @return position of this member in the file system and inside file
      */
    override def getLocation: Option[Location] = Location(filePath, ctx, getIdentity)
}


abstract class MethodMember(ctx: ParserRuleContext, parser: ApexcodeParser, filePath: Path) extends Member {

    //same as Member.toJson but with "isStatic"
    override def toJson: JsValue = {
        val isStaticNum = if (ClassBodyMember.isStatic(ctx)) 1 else 0
        val data = super.toJson.asJsObject.fields + ("isStatic" -> isStaticNum.toJson)
        data.toJson
    }

    override def getVisibility: String = {
        ClassBodyMember.getVisibility(ctx)
    }
    override def isStatic: Boolean = ClassBodyMember.isStatic(ctx)

    override def getFullType: String = {
        ClassBodyMember.getFullTypeIfTypeIsInnerClass(this, getParent) match {
            case Some(fullType) => fullType
            case None => ClassBodyMember.getFullTypeIfTypeIsInnerClass(this, getParent.flatMap(_.getParent)) match {
                case Some(fullType) => fullType
                case None => this.getType
            }
        }
    }
    def getMethodName:String

    override def equals(o: Any): Boolean = {
        if (super.equals(o)) {
            //check that params also match
            val otherMethod = o.asInstanceOf[MethodMember]
            otherMethod.getArgs.map(_.getType.toLowerCase).mkString == this.getArgs.map(_.getType.toLowerCase).mkString
        } else {
            false
        }
    }

    override def hashCode = {
        (getIdentity.toLowerCase + getArgs.map(_.getType).mkString).hashCode
    }

    /**
     * several methods may have the same name but different parameters and static context
     * @return
     */
    override def getIdentity:String = {
        //... <Type> methodName (formalParameters)
        getMethodName + getArgs.map(_.getType).mkString
    }

    /**
     * several methods may have the same name but different parameters and static context
     * however when returning completion result we need to list method name only
     * @return
     */
    override def getIdentityToDisplay:String = {
        getMethodName
    }

    def getMethodMemberClass:  Class[_ <: ParserRuleContext]

    override def getSignature:String = {
        ApexParserUtils.findChild(ctx, getMethodMemberClass) match {
            case Some(methodDeclaration) =>
                val start = ApexParserUtils.findChildren(ctx, classOf[ClassOrInterfaceModifierContext])
                    .filter(null!= _.getChild(classOf[TerminalNodeImpl], 0))
                    .map(_.getChild(classOf[TerminalNodeImpl], 0)).mkString(" ")
                val params = getArgs.mkString(",")

                if (start.nonEmpty)
                    start + " " + getType + " " + getMethodName + s"($params)"
                else
                    getType + " " + getMethodName + s"($params)"

            case None => //fall back
                getIdentity + ": Signature is NOT COVERED by current implementation" //TODO - perhaps throwing an exception may be better?
        }
    }

    def getArgs:List[MethodParameter]


    override def getDoc: String = {
        val startToken = ctx.getStart
        parser.getTokenStream match {
            case tokens: CommonTokenStream =>
                ApexParserUtils.getNearestHiddenTokenToLeft(startToken, ApexcodeLexer.APEXDOC_COMMENT, tokens) match {
                  case Some(_commentToken) =>
                      return unwrapJavadoc(_commentToken.getText)
                  case None =>
                }
            case _ =>
        }
        super.getDoc
    }

    /**
     * if doc is inside javadoc style comments then we need to remove leading spaces and "*" in each line
     * @param text - text to clean up
     * @return
     */
    private def unwrapJavadoc(text: String): String = {
        if (null != text) {
            text.split("\\r?\\n").map(line => line.replaceFirst("\\s*((\\/\\*+)|(\\**\\/)|(\\**)*)", "")).filterNot(_.trim.isEmpty).mkString(System.getProperty("line.separator"))
        } else {
            ""
        }
    }

    protected def getFormalParams(formalParams: ApexcodeParser.FormalParametersContext, filePath: Path):List[MethodParameter] = {
        formalParams.formalParameterList() match {
            case paramsListCtx: ApexcodeParser.FormalParameterListContext =>
                val params = mutable.ListBuffer[MethodParameter]()

                var i = 0

                while (i < paramsListCtx.getChildCount) {
                    //paramsListCtx.getChildCount = number of parameters
                    paramsListCtx.getChild(i) match {
                        case formalParamCtx: ApexcodeParser.FormalParameterContext =>
                            val param: MethodParameter = new MethodParameter(paramsListCtx.getChild(i).asInstanceOf[ApexcodeParser.FormalParameterContext], filePath)
                            //println("param=" + param.getSignature)
                            params += param
                        case _ =>
                    }
                    i = i + 1

                }
                params.toList
            case _ => List()
        }
    }
    /**
      * @return position of this member in the file system and inside file
      */
    override def getLocation: Option[Location] = Location(filePath, ctx, getSignature)
}

object ClassMethodMember {
    def unapply(ctx: ParseTree): Option[ParseTree] = {
        if (ClassBodyMember.isMethodOfClass(ctx)) Some(ctx) else None
    }
}
class ClassMethodMember(ctx: ParserRuleContext, parser: ApexcodeParser, filePath: Path)
        extends MethodMember(ctx.asInstanceOf[ClassBodyDeclarationContext], parser, filePath) {

    def getMethodMemberClass: Class[_ <: ParserRuleContext] = classOf[MethodDeclarationContext]

    override def getMethodName:String = {
        //... <Type> methodName (formalParameters)
        val methodDeclarationContext = ApexParserUtils.findChildren(ctx, classOf[MethodDeclarationContext])
        if (methodDeclarationContext.nonEmpty) {
            methodDeclarationContext.head.Identifier().getText
        } else {
            ""
        }
    }

    override def getType: String = {
        val declarationContexts = ApexParserUtils.findChildren(ctx, classOf[MethodDeclarationContext])
        if (declarationContexts.nonEmpty) {
            val declarationContext = declarationContexts.head
            if (null != declarationContext.`type`())
                return parser.getTokenStream.getText(declarationContext.`type`())
        }
        "void"
    }

    override def getArgs:List[MethodParameter] = {
        val paramsContext = ApexParserUtils.findChildren(ctx, classOf[MethodDeclarationContext])
        if (paramsContext.nonEmpty) {
            getFormalParams(paramsContext.head.formalParameters(), filePath)
        } else {
            List()
        }
    }
}

object InterfaceMethodMember {
    def unapply(ctx: ParseTree): Option[ParseTree] = {
        if (ClassBodyMember.isMethodOfInterface(ctx)) Some(ctx) else None
    }
}

class InterfaceMethodMember(ctx: ParserRuleContext, parser: ApexcodeParser, filePath: Path) extends MethodMember(ctx.asInstanceOf[InterfaceBodyDeclarationContext], parser, filePath) {

    def getMethodMemberClass: Class[_ <: ParserRuleContext] = classOf[InterfaceMethodDeclarationContext]

    override def getMethodName:String = {
        //... <Type> methodName (formalParameters)
        val methodDeclarationContext = ApexParserUtils.findChildren(ctx, classOf[InterfaceMethodDeclarationContext])
        if (methodDeclarationContext.nonEmpty) {
            methodDeclarationContext.head.Identifier().getText
        } else {
            ""
        }
    }

    override def getType: String = {
        val declarationContexts = ApexParserUtils.findChildren(ctx, classOf[InterfaceMethodDeclarationContext])
        if (declarationContexts.nonEmpty) {
            val declarationContext = declarationContexts.head
            if (null != declarationContext.`type`())
                return parser.getTokenStream.getText(declarationContext.`type`())
        }
        "void"
    }
    override def getArgs:List[MethodParameter] = {
        val paramsContext = ApexParserUtils.findChildren(ctx, classOf[InterfaceMethodDeclarationContext])
        if (paramsContext.nonEmpty) {
            getFormalParams(paramsContext.head.formalParameters(), filePath)
        } else {
            List()
        }
    }
}

class MethodParameter(ctx: ApexcodeParser.FormalParameterContext, filePath: Path) extends Member {
    //class/method name
    override def getType: String = ctx.getRuleContext(classOf[ApexcodeParser.TypeContext], 0).getText

    override def getSignature: String = getModifier + " " + getType + " " + getIdentity

    override def getIdentity: String = {
        val identityContext = ctx.getRuleContext(classOf[ApexcodeParser.VariableDeclaratorIdContext], 0)
        identityContext.Identifier().getText
    }
    // e.g. 'final'
    def getModifier: String = {
        val modifierContext = ctx.getRuleContext(classOf[ApexcodeParser.VariableModifierContext], 0)
        if (null == modifierContext) "" else  modifierContext.getText
    }

    override def isStatic: Boolean = false

    override def toString: String = getSignature

    /**
      * @return position of this member in the file system and inside file
      */
    override def getLocation: Option[Location] = Location(filePath, ctx, getIdentity)
}

/**
 * one LocalVariableDeclarationStatementContext can define multiple variables
 */
case class LocalVariableMember(localVariableDeclarationCtx: ApexcodeParser.LocalVariableDeclarationContext, ctx: ApexcodeParser.VariableDeclaratorContext, filePath: Path) extends Member {
    /**
     * @return
     * for class it is class name
     * for method it is method name + string of parameter types
     * for variable it is variable name
     * etc
     */
    override def getIdentity: String = ctx.variableDeclaratorId().Identifier().getText

    override def getType: String = localVariableDeclarationCtx.`type`().getText

    override def getSignature: String = getType + " " + getIdentity

    override def isStatic: Boolean = false //local variables are never static

    /**
      * @return position of this member in the file system and inside file
      */
    override def getLocation: Option[Location] = Location(filePath, ctx, getIdentity)
}

class EnhancedForLocalVariableMember(ctx: ApexcodeParser.EnhancedForControlContext, filePath: Path) extends Member {
    /**
     * @return
     * for class it is class name
     * for method it is method name + string of parameter types
     * for variable it is variable name
     * etc
     */
    override def getIdentity: String = ctx.variableDeclaratorId().Identifier().getText

    override def getType: String = ctx.`type`().getText

    override def getSignature: String = getType + " " + getIdentity

    override def isStatic: Boolean = false //local variables are never static

    /**
      * @return position of this member in the file system and inside file
      */
    override def getLocation: Option[Location] = Location(filePath, ctx, getIdentity)
}

class CatchClauseMember(ctx: ApexcodeParser.CatchClauseContext, filePath: Path) extends Member {
    /**
     * @return
     * for class it is class name
     * for method it is method name + string of parameter types
     * for variable it is variable name
     * etc
     */
    override def getIdentity: String = ctx.Identifier().getText

    /**
     * each Identifier() returns only part of exception type
     * e.g. System.DmlException will be returned as
     * ctx.catchType().qualifiedName(0).Identifier(0) = System
     * ctx.catchType().qualifiedName(0).Identifier(1) = DmlException
     *
     * @return
     */
    override def getType: String = ctx.catchType().qualifiedName(0).Identifier().mkString(".")

    override def getSignature: String = getType + " " + getIdentity

    override def isStatic: Boolean = false

    /**
     * catch clause needs special treatment, its identity should be returned as a child in getChild
     * because it does not have real children other than exception typ and name/variable
     * @param identity - something which may match name of exception variable
     * @param withHierarchy - ignored, because CatchClauseMember does not have real children
     * @return
     */
    override def getChild(identity: String, withHierarchy: Boolean): Option[Member] = {
        if (identity.toLowerCase == getIdentity.toLowerCase) {
            Some(this)
        } else {
            None
        }
    }

    /**
      * @return position of this member in the file system and inside file
      */
    override def getLocation: Option[Location] = Location(filePath, ctx, getIdentity)
}

class CreatorMember(ctx: ApexcodeParser.CreatorContext) extends AnonymousMember {
    private var _createdName: Option[String] = None
    def createdName: String = {
        _createdName match {
          case Some(name) => name
          case None => //calculate
              val typeArgs = ApexParserUtils.findChildren(ctx, classOf[TypeArgumentContext])
              if (typeArgs.isEmpty) {
                  //simple case: Account acc = new Account( <caret>)
                  _createdName = Some(ctx.createdName().getText)
              }  else {
                  //inside collection creator like: new Map<String, Account> { 'acc' => new Account( <caret>)}
                  _createdName = Some(typeArgs.last.`type`().getText)
              }
              _createdName.get
        }
    }
}
