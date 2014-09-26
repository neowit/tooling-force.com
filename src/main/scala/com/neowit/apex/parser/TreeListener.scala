package com.neowit.apex.parser

import com.neowit.apex.parser.TreeListener.ApexTree
import com.neowit.apex.parser.antlr.ApexcodeParser._
import org.antlr.v4.runtime.Token
import org.antlr.v4.runtime.tree.{TerminalNodeImpl, TerminalNode, ParseTree}

import com.neowit.apex.parser.antlr.{ApexcodeParser, ApexcodeBaseListener}

import scala.collection.mutable
import scala.collection.JavaConversions._
import scala.util.parsing.json.JSONObject

object TreeListener {
    type ApexTree = Map[String, Member]
}
class TreeListener (val parser: ApexcodeParser) extends ApexcodeBaseListener {
    val tree = new mutable.LinkedHashMap[String, Member]()//path -> member, e.g. ParentClass.InnerClass -> ClassMember

    //contains stack of current class/method hierarchy, currently processed class is at the top
    val stack= mutable.Stack[Member]()


    def dump() {
        for(key <- tree.keySet) {
            println(key + ": " + tree.get(key).get.toString)
        }
    }
    def getTree: ApexTree = {
        tree.toMap
    }

    def registerMember(member: Member) {
        if (stack.nonEmpty) {
            val parentMember = stack.top
            parentMember.addChild(member)
            member.parent = Some(parentMember)
        }
        //stack.push(member)
    }

    override def enterClassDeclaration(ctx: ApexcodeParser.ClassDeclarationContext): Unit ={
        val member = if (ClassBodyMember.isInnerClass(ctx)) new InnerClassMember(ctx) else new ClassMember(ctx)
        registerMember(member)
        tree += ((member.getPath, member))
        stack.push(member)

    }
    override def exitClassDeclaration(ctx: ApexcodeParser.ClassDeclarationContext) {
        stack.pop()

    }
    override def enterClassBodyDeclaration(ctx: ClassBodyDeclarationContext): Unit = {
        val member = ctx match {
            case EnumMember(context) => new EnumMember(ctx)
            case MethodMember(context) => new MethodMember(ctx, parser)
            case FieldMember(context) => new FieldMember(ctx, parser)
            case PropertyMember(context) => new PropertyMember(ctx, parser)
            case _ => null;//new ClassBodyMember(ctx)
        }
        if (null != member) {
            registerMember(member)
        }
    }

    override def exitClassBodyDeclaration(ctx: ClassBodyDeclarationContext): Unit = {
        //super.exitClassBodyDeclaration(ctx)
    }


    private val identifierMap = mutable.Map[String, Set[TerminalNode]]()
    /**
     * record all identifiers for future use
     */
    override def visitTerminal(node: TerminalNode): Unit = {
        val symbol: Token = node.getSymbol
        if (symbol.getType == ApexcodeParser.Identifier
                && !node.getParent.isInstanceOf[ClassOrInterfaceTypeContext]
                && !node.getParent.isInstanceOf[CreatedNameContext]
                && !node.getParent.isInstanceOf[PrimaryContext]
        ) {
            val name = symbol.getText
            identifierMap.get(name) match {
              case Some(nodes) =>
                  identifierMap(name) = nodes + node
              case None =>
                  identifierMap(symbol.getText) = Set(node)
            }
        }
    }

    /**
     * @param name - name of identifier (e.g. variable name or method name)
     * @return list of identifiers matching this name
     */
    def getIdentifiers(name: String): Option[Set[TerminalNode]] = {
        identifierMap.get(name)
    }
}

trait Member {
    var parent: Option[Member] = None
    private val children = mutable.HashMap[String, Member]()

    def addChild(member: Member) {
        try {
            getChild(member.getIdentity.toLowerCase) match {
                case Some(_existingMember) => //this child already exists, do not overwrite
                case None =>
                    children.+=(member.getIdentity.toLowerCase -> member)
            }
        } catch {
            case ex:Throwable => println("failed to add member of parent: " + this.getIdentity)
        }
    }
    def getParent: Option[Member] = {
        parent
    }

    /**
     * find parent which matches specified filter
     * @param filter - condition
     * @return
     */
    def findParent(filter: (Member) => Boolean): Option[Member] = {
        getParent match {
          case Some(parentMember) =>
              if (filter(parentMember)) Some(parentMember) else parentMember.findParent(filter)
          case None => None
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
        getSuperTypeIdentity match {
          case Some(superTypeName) => apexTree.get(superTypeName) match {
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

    def getChild(identity : String, apexTree: Option[ApexTree] = None): Option[Member] = {

        def findChildHierarchically(identity: String, apexTree: Option[ApexTree] = None): Option[Member] = {
            apexTree match {
                case Some(tree) =>
                    getSuperTypeIdentity match {
                        case Some(superTypeName) =>
                            tree.get(superTypeName) match {
                                case Some(superTypeMember) =>
                                    superTypeMember.getChild(identity, Some(tree))
                                case None => None
                            }
                        case None => None
                    }
                case None => None
            }
        }

        children.get(identity.toLowerCase) match {
            case Some(childMember) => Some(childMember)
            case None => findChildHierarchically(identity.toLowerCase, apexTree)
        }
    }

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
    def getType: String = getIdentity

    //e.g. MyClass.InnerClass
    def getFullType: String = getType

    def getSuperType: Option[String] = None

    def getFullSuperType: Option[String] = None

    def getDoc: String = " " //TODO - implement documentation retrieval

    def getVisibility: String = "private" //TODO

    def isStatic: Boolean

    def getPath: String = {

        def resolveFullPath(currentPath: String): String = parent match {
            case Some(member) => member.getPath + "." + currentPath
            case None => currentPath
        }
        val res = resolveFullPath(getIdentity)
        res
    }

    override def toString: String = {
        //val txt = getIdentity
        val txt = getSignature + " => type=" + getType + "; id=" + getIdentity
        val chlds = new StringBuilder()
        for (x <- children) {
            chlds.++= (x.toString)
        }
        if (chlds.nonEmpty )
            txt + ": \n - " + chlds.toString()
        else
            txt + "\n - "
    }

    def toJson: JSONObject = {
        val data = Map("identity" -> getIdentityToDisplay, "realIdentity" -> getIdentity,
                        "signature" -> getSignature, "type" -> getType,
                        "visibility" -> getVisibility, "doc" -> getDoc)
        JSONObject(data)
    }

    override def equals(o: Any): Boolean = {
        o.isInstanceOf[Member] && this.getClass == o.getClass &&
            this.getIdentity.toLowerCase == o.asInstanceOf[Member].getIdentity.toLowerCase
    }
    override def hashCode = getIdentity.toLowerCase.hashCode
}

class ClassMember(ctx: ClassDeclarationContext) extends Member {
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
}

object InnerClassMember {
    private val IDENTITY_PREFIX = "InnerClass:"
    def getFullIdentity(outerClassIdentity: String, innerClassIdentity: String): String = {
        outerClassIdentity + "." + IDENTITY_PREFIX + innerClassIdentity
    }
}
class InnerClassMember(ctx: ClassDeclarationContext) extends ClassMember(ctx) {

    override def getSignature: String = {
        val clsBodyDeclaration = ctx.getParent.getParent
        ClassBodyMember.findChildren(clsBodyDeclaration, classOf[TerminalNodeImpl]).map(_.getText).mkString(" ")
    }
    override def getFullType: String = {
        getParent match {
            case Some(parentMember) if !this.getType.startsWith(parentMember.getType + ".") =>
                parentMember.getType + "." + this.getType
            case _ => super.getFullType
        }
    }

    override def getFullSuperType: Option[String] = {
    override def getIdentity: String = this.getParent match {
        case Some(parentMember) => InnerClassMember.getFullIdentity(parentMember.getIdentity, super.getIdentity)
        case None => throw new IllegalAccessError("never expected to get to this place")
    }

    override def getIdentityToDisplay: String = super.getIdentity
        this.getSuperType match {
          case Some(superTypeName) =>
              //check if superTypeName is full or partial type name
              findParent(_.isInstanceOf[ClassMember]) match {
                  case Some(parentClassMember) if superTypeName.indexOf(".") < 1 =>
                      //inner class can inherit from
                      //1. stand alone class (not inner) e.g. MyClass - no further processing needed to get Full Super Type
                      //2. inner class from another class, according to Apex rules must be defined as OtherClass.InnerClass
                      //   - no further processing needed to get Full Super Type
                      //3. inner class in the current class:
                      //   - can be defined as CurrentClass.InnerClass or InnerClass
                      //     here we are making sure that if InnerClass is specified then it is resolved as CurrentClass.InnerClass

                      //is this a partially defined inner class? like Inner instead of Outer.Inner
                      parentClassMember.getChild(superTypeName) match {
                          case Some(member) => //yes it is, let's add outer class name to full type
                              Some(parentClassMember.getType + "." + superTypeName)
                          case None => //assume that full super type name is already provided
                              Some(superTypeName)
                      }

                  case _ => Some(superTypeName)
              }
          case None => None
        }
    }
}

object ClassBodyMember {

    def isInnerClass(ctx: ParseTree): Boolean = {
        ctx.getParent match {
            case pp:ApexcodeParser.MemberDeclarationContext => true
            case _ => false
        }
    }
    def isMethod(ctx: ParseTree): Boolean = findChildren(ctx, classOf[ApexcodeParser.MethodDeclarationContext]).nonEmpty
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
                  case Some(modifierContext) => modifierContext.classOrInterfaceModifier().getChild(classOf[TerminalNodeImpl], 0).getText
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
    def getFullTypeIfTypeIsInnerClass(member: Member, parentMember: Option[Member]): Option[String] = {
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
    override def toJson: JSONObject = {
        val isStaticNum = if (ClassBodyMember.isStatic(ctx)) 1 else 0
        val data = super.toJson.obj + ("isStatic" -> isStaticNum)
        JSONObject(data)
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
class EnumMember(ctx: ClassBodyDeclarationContext) extends ClassBodyMember(ctx) {

    override def getIdentity:String = {
        ClassBodyMember.findChildren(ctx, classOf[EnumDeclarationContext]).head.Identifier().getText
    }

    override def getSignature: String = {
        ClassBodyMember.findChildren(ctx, classOf[TerminalNodeImpl]).filter(node => "{" != node.getText && "}" != node.getText).mkString(" ")
    }

    override def getType: String = getIdentity

    override def isStatic: Boolean = false
}

object PropertyMember {
    def unapply(ctx: ParseTree): Option[ParseTree] = {
        if (ClassBodyMember.isProperty(ctx)) Some(ctx) else None
    }
}
class PropertyMember(ctx: ClassBodyDeclarationContext, parser: ApexcodeParser) extends ClassBodyMember(ctx) {
    val propertyDeclarationContext = ctx.memberDeclaration().propertyDeclaration()

    override def getIdentity: String = {
        propertyDeclarationContext.variableDeclarators().variableDeclarator().find(null != _.variableDeclaratorId()) match {
          case Some(variableDeclaratorIdContext) => variableDeclaratorIdContext.getText
          case None => ""
        }
    }

    override def getType: String = {
        if (null != propertyDeclarationContext && null != propertyDeclarationContext.`type`()) {
            propertyDeclarationContext.`type`().getText
        } else {
            "void"
        }
    }

    override def getSignature: String = {
        val modifiers = ctx.modifier().map(m => m.classOrInterfaceModifier().getChild(0).getText).mkString(" ")
        modifiers + " " + getType + " " + getIdentity
    }
}

object FieldMember {
    def unapply(ctx: ParseTree): Option[ParseTree] = {
        if (ClassBodyMember.isField(ctx)) Some(ctx) else None
    }
}
class FieldMember(ctx: ClassBodyDeclarationContext, parser: ApexcodeParser) extends ClassBodyMember(ctx) {
    val fieldDeclarationContext = ctx.memberDeclaration().fieldDeclaration()

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
        /* //this also works, but looks too cumbersome
        val modifiers = ClassBodyMember.findChildren(ctx, classOf[ClassOrInterfaceModifierContext])
                             .map(ClassBodyMember.findChildren(_, classOf[TerminalNodeImpl])).map(_.head).mkString(" ")

        */
        val modifiers = ctx.modifier().map(m => m.classOrInterfaceModifier().getChild(0).getText).mkString(" ")
        modifiers + " " + getType + " " + getIdentity
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

