package com.neowit.apex.parser

import com.neowit.apex.parser.antlr.ApexcodeParser._
import org.antlr.v4.runtime.Token
import org.antlr.v4.runtime.tree.{TerminalNodeImpl, TerminalNode, ParseTree}

import com.neowit.apex.parser.antlr.{ApexcodeParser, ApexcodeBaseListener}

import scala.collection.mutable
import scala.collection.JavaConversions._

class TreeListener (val parser: ApexcodeParser) extends ApexcodeBaseListener {
    val tree = new mutable.LinkedHashMap[String, Member]()//path -> member, e.g. ParentClass.InnerClass -> ClassMember

    //contains stack of current class/method hierarchy, currently processed class is at the top
    val stack= mutable.Stack[Member]()


    def dump() {
        for(key <- tree.keySet) {
            println(key + ": " + tree.get(key).get.toString)
        }
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
        val member = new ClassMember(ctx)
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
            case _ => new ClassBodyMember(ctx)
        }
        registerMember(member)
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
    val children = mutable.ListBuffer[Member]()

    def addChild(member: Member) {
        children.+=(member)
    }

    def getIdentity:String
    def getSignature:String
    def getType: String = getIdentity

    def getVisibility: String = "private" //TODO

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

}

class ClassMember(ctx: ClassDeclarationContext) extends Member {
    def getIdentity:String = {
        ctx.getToken(ApexcodeParser.Identifier, 0).getText
    }


    def getSignature: String = {
        ClassBodyMember.findChildren(ctx, classOf[TerminalNodeImpl]).mkString(" ")
    }

}

object ClassBodyMember {

    def isInnerClass(ctx: ParseTree): Boolean = findChildren(ctx, classOf[ApexcodeParser.ClassDeclarationContext]).nonEmpty
    def isMethod(ctx: ParseTree): Boolean = findChildren(ctx, classOf[ApexcodeParser.MethodDeclarationContext]).nonEmpty
    def isEnum(ctx: ParseTree): Boolean = findChildren(ctx, classOf[ApexcodeParser.EnumDeclarationContext]).nonEmpty
    def isField(ctx: ParseTree): Boolean = findChildren(ctx, classOf[ApexcodeParser.FieldDeclarationContext]).nonEmpty
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

    /**
     * find children satisfying given context type
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

}

class ClassBodyMember(ctx: ClassBodyDeclarationContext) extends Member {

    def getIdentity:String = {
        ctx.getToken(ApexcodeParser.Identifier, 0) match {
            case node: TerminalNode => node.getText
            case _ =>
                //fall back to first level of terminal nodes
                ClassBodyMember.findChildren(ctx, classOf[TerminalNodeImpl]).mkString(" ").replaceAll("\\{|\\}|;", "")
        }
    }

    lazy val isInnerClass: Boolean = ClassBodyMember.isInnerClass(ctx)


    //try to identify and include only items belonging to method/class/field signature, as opposed to body
    def getSignature: String = {
        ctx.getText
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
}

object FieldMember {
    def unapply(ctx: ParseTree): Option[ParseTree] = {
        if (ClassBodyMember.isField(ctx)) Some(ctx) else None
    }
}
class FieldMember(ctx: ClassBodyDeclarationContext, parser: ApexcodeParser) extends ClassBodyMember(ctx) {

    override def getIdentity:String = {
        val fieldDeclarationContext = ClassBodyMember.findChildren(ctx, classOf[VariableDeclaratorIdContext])
        if (fieldDeclarationContext.nonEmpty) {
            fieldDeclarationContext.head.Identifier().getText
        } else ""
    }

    override def getType: String = {
        val fieldDeclarationContexts = ClassBodyMember.findChildren(ctx, classOf[FieldDeclarationContext])
        if (fieldDeclarationContexts.nonEmpty) {
            val fieldDeclarationContext = fieldDeclarationContexts.head
            if (null != fieldDeclarationContext.`type`())
                return parser.getTokenStream.getText(fieldDeclarationContext.`type`())
        }
        "void"
    }

    override def getSignature: String = {
        val modifiers = ClassBodyMember.findChildren(ctx, classOf[ClassOrInterfaceModifierContext])
                             .map(ClassBodyMember.findChildren(_, classOf[TerminalNodeImpl])).map(_.head).mkString(" ")

        modifiers + " " + getType + " " + getIdentity
    }
}

object MethodMember {
    def unapply(ctx: ParseTree): Option[ParseTree] = {
        if (ClassBodyMember.isMethod(ctx)) Some(ctx) else None
    }
}
class MethodMember(ctx: ClassBodyDeclarationContext, parser: ApexcodeParser) extends ClassBodyMember(ctx) {

    override def getIdentity:String = {
        //... <Type> methodName (formalParameters)
        val methodDeclarationContext = ClassBodyMember.findChildren(ctx, classOf[MethodDeclarationContext])
        if (methodDeclarationContext.nonEmpty) {
            methodDeclarationContext.head.Identifier().getText
        } else {
            ""
        }
    }
    override def getSignature:String = {
        ClassBodyMember.findChild(ctx, classOf[MethodDeclarationContext]) match {
            case Some(methodDeclaration) =>
                val start = ClassBodyMember.findChildren(ctx, classOf[ClassOrInterfaceModifierContext])
                            .filter(null!= _.getChild(classOf[TerminalNodeImpl], 0))
                            .map(_.getChild(classOf[TerminalNodeImpl], 0)).mkString(" ")
                val params = getArgs.mkString(", ")

                if (start.nonEmpty)
                    start + " " + getType + " " + getIdentity + s"($params)"
                else
                    getType + " " + getIdentity + s"($params)"

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

    override def toString: String = getSignature
}

