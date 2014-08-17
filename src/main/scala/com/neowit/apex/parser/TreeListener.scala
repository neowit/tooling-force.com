package com.neowit.apex.parser

import com.neowit.apex.parser.antlr.ApexcodeParser._
import org.antlr.v4.runtime.tree.{Tree, ParseTree}
import org.antlr.v4.runtime.{ParserRuleContext, TokenStream}

import com.neowit.apex.parser.antlr.{ApexcodeParser, ApexcodeBaseListener}

import scala.collection.mutable
import scala.collection.JavaConversions._

class TreeListener (val parser: ApexcodeParser) extends ApexcodeBaseListener {
    val tree = new mutable.LinkedHashMap[String, Member]()
    //contains stack of current class/method hierarchy, currently processed class is at the top
    val stack= mutable.Stack[Member]()

    val classBodyMembers = mutable.ListBuffer[ClassBodyMember]()

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
        stack.push(member)
    }
    override def enterClassDeclaration(ctx: ApexcodeParser.ClassDeclarationContext){
        val member = new ClassMember(parser, ctx)
        registerMember(member)

        //System.out.println("interface I"+ctx.Identifier()+" {")
        val context = ctx.getParent.getRuleContext(classOf[ApexcodeParser.ClassOrInterfaceModifierContext], 0)
        //visibility public/private/global, etc
        val visibility = if (context.getChildCount > 0) context.getChild(0).getText else "private"
        System.out.println(visibility + " " + ctx.Identifier() + "{")
        //tree += ((ctx.Identifier().getText, ctx))
        tree += ((member.path, member))
    }

    override def exitClassDeclaration(ctx: ApexcodeParser.ClassDeclarationContext) {
        stack.pop()
        System.out.println("}")
        System.out.println("\ntree=" + tree.keySet)

    }

    /** Listen to matches of methodDeclaration */
    override def enterMethodDeclaration(ctx: ApexcodeParser.MethodDeclarationContext) {
        val member = new MethodMember(parser, ctx)
        registerMember(member)

        // need parser to get tokens
        val tokens:TokenStream = parser.getTokenStream
        //System.out.println("\t"+member.getType+" "+ member.getIdentity + "(" + member.args +");")
    }

    override def exitMethodDeclaration(ctx: ApexcodeParser.MethodDeclarationContext) {
        stack.pop()
    }

    override def enterEnumDeclaration(ctx: EnumDeclarationContext): Unit = {
        val member = new EnumMember(parser, ctx)
        registerMember(member)
        System.out.println("\t"+member.getType+" "+ member.getIdentity+";")

    }

    override def exitEnumDeclaration(ctx: EnumDeclarationContext): Unit = {
        stack.pop()
    }



    override def enterFieldDeclaration(ctx: FieldDeclarationContext): Unit = {
        //val sType = ctx.children.find(_.isInstanceOf[])
        val member = new FieldMember(parser, ctx)
        registerMember(member)
        System.out.println("\tfield: "+member.getType+" "+ member.getIdentity + ";")
    }

    override def exitFieldDeclaration(ctx: FieldDeclarationContext): Unit = {
        stack.pop()
    }

    override def enterEveryRule(ctx: ParserRuleContext): Unit = {
        super.enterEveryRule(ctx)
        if (null != ctx.getToken(ApexcodeParser.Identifier, 0)) {
            //println("rule=" + ctx.getToken(ApexcodeParser.Identifier, 0).getText)
        }
    }

    override def enterBlock(ctx: BlockContext): Unit = {
        super.enterBlock(ctx)
    }

    override def enterClassBodyDeclaration(ctx: ClassBodyDeclarationContext): Unit = {
        //super.enterClassBodyDeclaration(ctx)
        //stack
        val bodyMember = new ClassBodyMember(ctx)
        classBodyMembers.+= (bodyMember)
        println("isMethod=" + bodyMember.isMethod)

    }

    override def exitClassBodyDeclaration(ctx: ClassBodyDeclarationContext): Unit = {
        //super.exitClassBodyDeclaration(ctx)
        //stack.pop()
    }
}

trait ChildrenList[T] {
    def getChild(i: Int): Tree

    /** How many children are there? If there is none, then this
      *  node represents a leaf node.
      */
    def getChildCount(): Int
}
class ClassBodyMember(ctx: ClassBodyDeclarationContext) {
    //def isMethod: Boolean = !ctx.getRuleContexts(classOf[ApexcodeParser.MethodDeclarationContext]).isEmpty
    def isMethod: Boolean = !findRuleContexts(ctx, classOf[ApexcodeParser.MethodDeclarationContext]).isEmpty

    /**
     * find children satisfying given context type
     * @param ctxType
     * @tparam T
     * @return
     */
    def findRuleContexts[T <: ParserRuleContext](ctx: ParseTree, ctxType: Class[T]): List[T] = {
        val foundChildren = mutable.ListBuffer[T]()
        if (ctx.getChildCount > 0) {
            for(child <- getChildren[ParseTree](ctx)) {
                if (child.getClass == ctxType) {
                    foundChildren += child.asInstanceOf[T]
                }
            }
            if (foundChildren.isEmpty) {
                //descend 1 level
                for(child <- getChildren[ParseTree](ctx)) {
                    foundChildren ++= findRuleContexts(child, ctxType)
                }

            }
        }
        foundChildren.toList
    }

    def getChildren[T](ctx:ParseTree): List[T] = {
        var children = mutable.ListBuffer[T]()
        var i = 0
        while (i < ctx.getChildCount) {
            val child = ctx.getChild(i)
            children += child.asInstanceOf[T]

            i += 1
        }
        children.toList
    }
}

object Visibility {
    def isVisibilityContext(ctx: ParserRuleContext): Boolean = {
        null != ctx  &&
            ctx.getChildCount > 0 &&
            !ctx.getChild(0).getText.matches("@.*|test.*")
    }
}
case class Visibility(visibilityContext: ParserRuleContext) {
    //visibility: public/private/global, etc
    val text = if (null != visibilityContext && visibilityContext.getChildCount > 0) visibilityContext.getChild(0).getText else "private"
}

sealed trait Member {
    var parent: Option[Member] = None
    val children = mutable.ListBuffer[Member]()

    def getIdentity: String //class/method name
    def getType: String //type of class/method/field, e.g. "Map<String, Object>"
    def getVisibilityContext:ParserRuleContext
    def getSignature: String

    val visibility: Visibility = new Visibility(getVisibilityContext)

    def addChild(member: Member) {
        children.+=(member)
    }
    //MyClass.InnerClass.method1
    //MyClass2.methodX
    val path: String = {
        def resolveFullPath(currentPath: String): String = parent match {
          case Some(member) => resolveFullPath(member.getIdentity + "." + currentPath)
          case None => currentPath
        }
        val res = resolveFullPath(getIdentity)
        res
    }

    override def toString: String = {
        val txt = getSignature
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


abstract class BaseMember(parser: ApexcodeParser, ctx: ParserRuleContext) extends Member {
    def getIdentity: String =
        ctx.getToken(ApexcodeParser.Identifier, 0).getText

}
case class ClassMember(parser: ApexcodeParser, ctx: ApexcodeParser.ClassDeclarationContext) extends BaseMember(parser, ctx) {
    def getVisibilityContext:ParserRuleContext = ctx.getParent.getRuleContext(classOf[ApexcodeParser.ClassOrInterfaceModifierContext], 0)
    def getType = getIdentity
    def getSignature: String = visibility.text + " " + getIdentity
}

case class MethodMember(parser: ApexcodeParser, ctx: ApexcodeParser.MethodDeclarationContext) extends BaseMember(parser, ctx) {
    def getVisibilityContext = {
        if (null != ctx.getParent && null != ctx.getParent.getParent) {
            val modifierCtxs = ctx.getParent.getParent.getRuleContexts(classOf[ApexcodeParser.ModifierContext])
            modifierCtxs.find(Visibility.isVisibilityContext(_)) match {
                case Some(v) => v
                case _ => null
            }
        } else {
            null
        }
    }
    def getType = if (null == ctx.`type`()) "void" else parser.getTokenStream.getText(ctx.`type`())

    def getSignature: String =
        visibility.text + " " + getType + " " + getIdentity + "(" + getArgs.mkString(", ") + ")"

    def getArgs:List[MethodParameter] = {
        ctx.formalParameters().formalParameterList() match {
            case paramsListCtx: ApexcodeParser.FormalParameterListContext =>
                val params = mutable.ListBuffer[MethodParameter]()

                var i = 0

                while (i < paramsListCtx.getChildCount) { //paramsListCtx.getChildCount = number of parameters
                    paramsListCtx.getChild(i) match {
                        case formalParamCtx:ApexcodeParser.FormalParameterContext =>
                            val param: MethodParameter = new MethodParameter(parser, paramsListCtx.getChild(i).asInstanceOf[ApexcodeParser.FormalParameterContext])
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
}

case class MethodParameter(parser: ApexcodeParser, ctx: ApexcodeParser.FormalParameterContext) extends BaseMember(parser, ctx) {
    //class/method name
    override def getType: String = ctx.getRuleContext(classOf[ApexcodeParser.TypeContext], 0).getText

    override def getSignature: String = getModifier + " " + getType + " " + getIdentity

    //type of class/method/field, e.g. "Map<String, Object>"
    override def getVisibilityContext: ParserRuleContext = null

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

case class EnumMember(parser: ApexcodeParser, ctx: ApexcodeParser.EnumDeclarationContext) extends BaseMember(parser, ctx) {
    def getVisibilityContext = ctx.getParent.getRuleContext(classOf[ApexcodeParser.EnumDeclarationContext], 0)
    def getType = "enum"
    def getSignature: String = visibility.text + " " + getType +  getIdentity

}

case class FieldMember(parser: ApexcodeParser, ctx: ApexcodeParser.FieldDeclarationContext) extends BaseMember(parser, ctx) {
    def getVisibilityContext = ctx.getParent.getParent.getRuleContext(classOf[ApexcodeParser.ModifierContext], 0)
    def getType = if (null == ctx.`type`()) "void" else parser.getTokenStream.getText(ctx.`type`())

    override def getIdentity: String =
        ctx.variableDeclarators().variableDeclarator().get(0).variableDeclaratorId().Identifier().getText

    def getSignature: String = visibility.text + " " + getType + " " + getIdentity
}
