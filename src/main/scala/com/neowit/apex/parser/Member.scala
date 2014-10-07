package com.neowit.apex.parser

import com.neowit.apex.parser.antlr.ApexcodeParser._
import org.antlr.v4.runtime.tree.{TerminalNodeImpl, TerminalNode, ParseTree}

import com.neowit.apex.parser.antlr.ApexcodeParser

import scala.collection.mutable
import scala.collection.JavaConversions._

import spray.json._
import DefaultJsonProtocol._

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
    def getClassMember: Option[ClassMember] = {
        def getMyClass[T <: AnonymousMember](m: Option[AnonymousMember]): Option[AnonymousMember] = {
            m match {
                case Some(member) =>
                    if (member.isInstanceOf[ClassMember]) {
                        Some(member)
                    } else {
                        getMyClass(member.getParent)
                    }
                case None => None
            }
        }
        getMyClass(Some(this)) match {
            case Some(member) => Some(member.asInstanceOf[ClassMember])
            case None => None
        }
    }

    def getTopMostClassMember: Option[ClassMember] = {
        if (this.isInstanceOf[ClassMember] && !this.isInstanceOf[InnerClassMember]) {
            return Some(this.asInstanceOf[ClassMember])
        }
        def getParentByType[T <: AnonymousMember](m: AnonymousMember, classType: Class[T]): Option[AnonymousMember] = {
            m.getParent match {
                case Some(member) =>
                    if (member.getClass == classType) {
                        Some(member)
                    } else {
                        getParentByType(member, classType)
                    }
                case None => None
            }
        }
        getParentByType(this, classOf[ClassMember]) match {
            case Some(member) => Some(member.asInstanceOf[ClassMember])
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
                                println(ex.getStackTraceString)
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
                        case Some(parentMember:ClassMember) =>
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

        children.get(identity.toLowerCase) match {
            case Some(childMember) => Some(childMember)
            case None => if (withHierarchy) findChildHierarchically(identity.toLowerCase, getApexTree) else None
        }
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

class ClassMember(ctx: ClassDeclarationContext) extends Member {
    private val innerClassByClassName = new mutable.LinkedHashMap[String, InnerClassMember]() //inner-class-name.toLowerCase => InnerClassMember
    private val enumByName = new mutable.LinkedHashMap[String, EnumMember]() //inner-class-name.toLowerCase => InnerClassMember

    override def getType: String = getIdentity

    def getIdentity:String = {
        //ctx.getToken(ApexcodeParser.Identifier, 0).getText
        ctx.Identifier().getText
    }


    override def isStatic: Boolean = false

    def getSignature: String = {
        ClassBodyMember.getChildren[TerminalNode](ctx, n => n.isInstanceOf[TerminalNode]).map(_.getText).mkString(" ")
    }

    override def getVisibility: String = {
        //go up the tree (if needed) to get to ClassBodyDeclarationContext and from there find visibility
        ClassBodyMember.getParent(ctx, classOf[ClassBodyDeclarationContext]) match {
            case Some(member) => ClassBodyMember.getVisibility(member)
            case None => "private"
        }
    }

    override def getSuperType: Option[String] = {
        //if one of ctx children is "extends" then the next one will be TypeContext with the name of super class
        val extendIndex = ClassBodyMember.findChildIndex(ctx, _ctx => "extends" == _ctx.getText.toLowerCase)
        if (extendIndex > 0 && ctx.getChildCount > extendIndex) {
            val superTypeContext = ctx.getChild(extendIndex + 1) //TypeContext goes after TerminalNode "extend"
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
            case m: InnerClassMember =>
                innerClassByClassName += (m.getType.toLowerCase -> m)
            case m: EnumMember =>
                enumByName += (m.getType.toLowerCase -> m)
            case _ =>
        }
    }
    def getInnerClassByType(innerClassTypeName: String): Option[InnerClassMember] = {
        innerClassByClassName.get(innerClassTypeName.toLowerCase)
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

    /**
     * if this class extends another class then find Member of that other class
     * @return
     */
    def getSuperClassMember: Option[ClassMember] = {
        this.getFullSuperType match {
            case Some(fullSuperType) => getApexTree.getClassMemberByType(fullSuperType)
            case None => None
        }
    }

    /**
     * check if given otherClassMember is a super class of current member
     * @param otherClassMember - class member to test against
     */
    def isInheritFrom (otherClassMember: ClassMember): Boolean = {
        if (otherClassMember == this) {
            true
        } else {
            this.getSuperClassMember match {
              case Some(superClassMember) => superClassMember.isInheritFrom(otherClassMember)
              case None => false
            }
        }

    }
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
}

class BuiltInMethodMember(parent: Member, identity: String, displayIdentity: String, retType: String,
                              params: List[String] = List(),
                              signature: Option[String] = None, isStaticMember: Boolean = false)
                                extends BuiltInMember(parent, identity, displayIdentity, retType, signature, isStaticMember) {

    override def getSignature: String = signature match {
        case Some(s) => s
        case None => "public " + getType + " " + getIdentityToDisplay + "(" + params.mkString(", ") + ")"
    }
}


object InnerClassMember {
    private val IDENTITY_PREFIX = "InnerClass:"
}
class InnerClassMember(ctx: ClassDeclarationContext) extends ClassMember(ctx) {

    override def getType: String = {
        val strType = ctx.Identifier().getText
        if (strType.indexOf("\\.") > 0) {
            //this is full type Outer.Inner - return only last bit - Inner
            strType.split("\\.").tail.head
        } else {
            strType
        }
    }

    override def getSignature: String = {
        val clsBodyDeclaration = ctx.getParent.getParent
        ClassBodyMember.findChildren(clsBodyDeclaration, classOf[TerminalNodeImpl]).map(_.getText).mkString(" ")
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
                            classMember.asInstanceOf[ClassMember].getInnerClassByType(superTypeName) match {
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



    override def getIdentity: String = this.getParent match {
        case Some(parentMember) => InnerClassMember.IDENTITY_PREFIX +  super.getIdentity
        case None => "" //this case is only relevant when we are adding child to parent first time, as Inner Class requires parent to resolve adentity
    }

    override def getIdentityToDisplay: String = super.getIdentity
}

object ClassBodyMember {

    def isInnerClass(ctx: ParseTree): Boolean = {
        ctx.getParent match {
            case pp:ApexcodeParser.MemberDeclarationContext => true
            case _ => false
        }
    }
    //def isMethod(ctx: ParseTree): Boolean = findChildren(ctx, classOf[ApexcodeParser.MethodDeclarationContext]).nonEmpty
    def isMethod(ctx: ParseTree): Boolean = {
        val memberDeclarations = getChildren(ctx, _.isInstanceOf[MemberDeclarationContext])
        memberDeclarations.nonEmpty && getChildren(memberDeclarations.head, _.isInstanceOf[MethodDeclarationContext]).nonEmpty
    }
    def isEnum(ctx: ParseTree): Boolean = findChildren(ctx, classOf[ApexcodeParser.EnumDeclarationContext]).nonEmpty
    def isField(ctx: ParseTree): Boolean = {
        getChild[ApexcodeParser.MemberDeclarationContext](ctx, classOf[ApexcodeParser.MemberDeclarationContext]) match {
            case Some(memberDeclarationContext) =>
                getChild[ApexcodeParser.FieldDeclarationContext](memberDeclarationContext, classOf[ApexcodeParser.FieldDeclarationContext])  match {
                    case Some(x) =>
                        true
                    case None => false
                }
            case None => false
        }
    }
    def isProperty(ctx: ParseTree): Boolean = findChildren(ctx, classOf[ApexcodeParser.PropertyDeclarationContext]).nonEmpty

    def isStatic(ctx: ParseTree): Boolean = {
        //classOrInterfaceModifier-s
        val modifierContexts = findChildren(ctx, classOf[ApexcodeParser.ClassOrInterfaceModifierContext])
        val x = modifierContexts.find(
            context => {
                val children = getChildren[ParseTree](context)
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
     * find children (recursively) satisfying given context type
     * @param ctxType
     * @tparam T
     * @return
     */
    def findChildren[T <: ParseTree](ctx: ParseTree, ctxType: Class[T],
                                     filter: ParseTree => Boolean = { _ => true}): List[T] = {
        val foundChildren = getChildren[ParseTree](ctx, filter).filter(_.getClass == ctxType).map(_.asInstanceOf[T])
        if (foundChildren.isEmpty) {
            //descend 1 level
            val res= getChildren[ParseTree](ctx, filter).map(findChildren(_, ctxType, filter)).flatMap(_.toList)
            res
        } else {
            foundChildren.toList
        }
    }
    def findChild[T <: ParseTree](ctx: ParseTree, ctxType: Class[T]): Option[T] = {
        val foundChildren = findChildren(ctx, ctxType)
        if (foundChildren.nonEmpty)
            Some(foundChildren.head)
        else
            None
    }

    def getChildren[T](ctx:ParseTree, filter: ParseTree => Boolean = { _ => true}): List[T] = {
        var children = mutable.ListBuffer[T]()
        var i = 0
        while (i < ctx.getChildCount) {
            val child = ctx.getChild(i)
            if (filter(child)) {
                children += child.asInstanceOf[T]
            }

            i += 1
        }
        children.toList
    }

    def getChild[T](ctx:ParseTree, filter: ParseTree => Boolean = { _ => true}): Option[T] = {
        var i = 0
        while (i < ctx.getChildCount) {
            val child = ctx.getChild(i)
            if (filter(child)) {
                return Some(child.asInstanceOf[T])
            }

            i += 1
        }
        None
    }

    /**
     *
     * @param ctx parent context
     * @param filter - predicate to apply
     * @return index of a direct child which matches specified predicate
     */
    def findChildIndex(ctx: ParseTree, filter: (ParseTree) => Boolean): Int = {
        var i = 0
        while (i < ctx.getChildCount) {
            val child = ctx.getChild(i)
            if (filter(child)) {
                return i
            }

            i += 1
        }
        -1
    }

    def getChild[T](ctx:ParseTree, cls: Class[T]): Option[T] = {
        getChild(ctx, n => n.getClass == cls)
    }

    def getParent[T](ctx:ParseTree, ctxType: Class[T]): Option[T] = {
        def getParentImpl[T](ctx: ParseTree, ctxType: Class[T]): Option[T] = {
            if (null == ctx) {
                None
            } else {
                if (ctx.getClass == ctxType) {
                    Some(ctx.asInstanceOf[T])
                } else {
                    if (null != ctx.getParent) {
                        getParentImpl(ctx.getParent, ctxType)
                    } else {
                        None
                    }
                }
            }
        }
        getParentImpl(ctx.getParent, ctxType)
    }

    /**
     * find top most class declaration context compared to current context
     * @param ctx - context used as a start point of climbing parse tree
     * @return higher level class declaration if there is one
     */
    def getTopMostClassContext(ctx: ParseTree): Option[ParseTree] = {
        getParent(ctx, classOf[ClassDeclarationContext]) match {
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
     * @param member
     * @return
     */
    def getFullTypeIfTypeIsInnerClass(member: Member, parentMember: Option[AnonymousMember]): Option[String] = {
        parentMember match {
            case Some(_parentMember) =>
                _parentMember.getChild(member.getType) match {
                    case Some(_member) if _member.isInstanceOf[InnerClassMember] =>
                        println("_member.getFullType=" + _member.getFullType)
                        Some(_member.getFullType)
                    case _ => None
                }
            case _ => None
        }
    }
}

abstract class ClassBodyMember(ctx: ClassBodyDeclarationContext) extends Member {

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

}

object EnumMember {
    def unapply(ctx: ParseTree): Option[ParseTree] = {
        if (ClassBodyMember.isEnum(ctx)) Some(ctx) else None
    }
}

class EnumMember(ctx: EnumDeclarationContext) extends Member {
    addChild(new BuiltInMethodMember(this, "values", "values", "List<" + getType + ">")) //add default values() method of enum

    override def getIdentity:String = {
        ctx.Identifier().getText
    }

    override def getSignature: String = {
        ClassBodyMember.findChildren(ctx, classOf[TerminalNodeImpl]).filter(node => "{" != node.getText && "}" != node.getText).mkString(" ")
    }

    override def getType: String = getIdentity

    override def isStatic: Boolean = false
}

/**
 * enum constant has only two methods: name(), ordinal()
 */
class BuiltInEnumConstantMethodMember(parent: EnumConstantMember, identity: String, displayIdentity: String, retType: String)
                                                                extends BuiltInMember(parent, identity, displayIdentity, retType)

class EnumConstantMember(ctx: EnumConstantContext) extends Member {
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
}

object PropertyMember {
    def unapply(ctx: ParseTree): Option[ParseTree] = {
        if (ClassBodyMember.isProperty(ctx)) Some(ctx) else None
    }
    def getModifiers(ctx: ParseTree): List[String] = {
        ClassBodyMember.getParent(ctx, classOf[ClassBodyDeclarationContext]) match {
            case Some(classBodyDeclarationCtx) =>
                classBodyDeclarationCtx.modifier().map(m => m.classOrInterfaceModifier().getChild(0).getText).toList
            case None => List()
        }
    }
}

class PropertyMember(ctx: PropertyDeclarationContext) extends Member {

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

    val _isStatic = getModifiers.map(_.toLowerCase).indexOf("static") >=0

    override def isStatic: Boolean = _isStatic

    override def getVisibility: String = getModifiers.find(v => ClassBodyMember.VISIBILITIES.contains(v.toLowerCase)) match {
        case Some(visibility) => visibility.toLowerCase
        case None => "private"
    }
}

object FieldMember {
    def unapply(ctx: ParseTree): Option[ParseTree] = {
        if (ClassBodyMember.isField(ctx)) Some(ctx) else None
    }
}
class FieldMember(ctx: FieldDeclarationContext) extends Member {
    val fieldDeclarationContext = ctx

    override def getIdentity:String = {
        val fieldDeclarationContext = ClassBodyMember.findChildren(ctx, classOf[VariableDeclaratorIdContext])
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
}

object MethodMember {
    def unapply(ctx: ParseTree): Option[ParseTree] = {
        if (ClassBodyMember.isMethod(ctx)) Some(ctx) else None
    }
}
class MethodMember(ctx: ClassBodyDeclarationContext, parser: ApexcodeParser) extends ClassBodyMember(ctx) {

    def getMethodName:String = {
        //... <Type> methodName (formalParameters)
        val methodDeclarationContext = ClassBodyMember.findChildren(ctx, classOf[MethodDeclarationContext])
        if (methodDeclarationContext.nonEmpty) {
            methodDeclarationContext.head.Identifier().getText
        } else {
            ""
        }
    }

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

    override def getSignature:String = {
        ClassBodyMember.findChild(ctx, classOf[MethodDeclarationContext]) match {
            case Some(methodDeclaration) =>
                val start = ClassBodyMember.findChildren(ctx, classOf[ClassOrInterfaceModifierContext])
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

    def getArgs:List[MethodParameter] = {
        val paramsContext = ClassBodyMember.findChildren(ctx, classOf[MethodDeclarationContext])
        if (paramsContext.nonEmpty) {
            paramsContext.head.formalParameters().formalParameterList() match {
                case paramsListCtx: ApexcodeParser.FormalParameterListContext =>
                    val params = mutable.ListBuffer[MethodParameter]()

                    var i = 0

                    while (i < paramsListCtx.getChildCount) {
                        //paramsListCtx.getChildCount = number of parameters
                        paramsListCtx.getChild(i) match {
                            case formalParamCtx: ApexcodeParser.FormalParameterContext =>
                                val param: MethodParameter = new MethodParameter(paramsListCtx.getChild(i).asInstanceOf[ApexcodeParser.FormalParameterContext])
                                //println("param=" + param.getSignature)
                                params += param
                            case _ =>
                        }
                        i = i + 1

                    }
                    params.toList
                case _ => List()
            }
        } else {
            List()
        }
    }
    override def getType: String = {
        val declarationContexts = ClassBodyMember.findChildren(ctx, classOf[MethodDeclarationContext])
        if (declarationContexts.nonEmpty) {
            val declarationContext = declarationContexts.head
            if (null != declarationContext.`type`())
                return parser.getTokenStream.getText(declarationContext.`type`())
        }
        "void"
    }
}

class MethodParameter(ctx: ApexcodeParser.FormalParameterContext) extends Member {
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
}

/**
 * one LocalVariableDeclarationStatementContext can define multiple variables
 */
class LocalVariableMember(localVariableDeclarationCtx: ApexcodeParser.LocalVariableDeclarationContext, ctx: ApexcodeParser.VariableDeclaratorContext) extends Member {
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
}

class EnhancedForLocalVariableMember(ctx: ApexcodeParser.EnhancedForControlContext) extends Member {
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
}

class CatchClauseMember(ctx: ApexcodeParser.CatchClauseContext) extends Member {
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
}

class CreatorMember(ctx: ApexcodeParser.CreatorContext) extends AnonymousMember {
    private var _createdName: Option[String] = None
    def createdName: String = {
        _createdName match {
          case Some(name) => name
          case None => //calculate
              val typeArgs = ClassBodyMember.findChildren(ctx, classOf[TypeArgumentContext])
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
