/*
 * Copyright (c) 2017 Andrey Gavrikov.
 * this file is part of tooling-force.com application
 * https://github.com/neowit/tooling-force.com
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package com.neowit.apex.parser

import java.nio.file.Path

import com.neowit.apex.completion.Caret
import com.neowit.apex.parser.antlr.ApexcodeParser._
import org.antlr.v4.runtime.{ParserRuleContext, Token}
import org.antlr.v4.runtime.tree.{ErrorNode, TerminalNode}
import com.neowit.apex.parser.antlr.{ApexcodeBaseListener, ApexcodeParser}

import scala.collection.mutable
import scala.collection.JavaConverters._

class ApexTreeListener (val parser: ApexcodeParser, filePath: Path, line: Int = -1, column: Int = -1, caret: Option[Caret] = None) extends ApexcodeBaseListener {
    val tree = new ApexTree()//path -> member, e.g. ParentClass.InnerClass -> ClassMember

    //contains stack of current class/method hierarchy, currently processed class is at the top
    private val memberScopeStack = mutable.Stack[AnonymousMember]()
    private var caretScopeMember: Option[AnonymousMember] = None

    def getCaretScopeMember: Option[AnonymousMember] = caretScopeMember

    def dump(): Unit = {
        tree.dump()
    }

    def getTree: ApexTree = {
        tree
    }

    def registerMember(member: AnonymousMember): Unit = {
        member.setApexTree(tree)
        if (memberScopeStack.nonEmpty) {
            val parentMember = memberScopeStack.top
            //member.setParent(parentMember)
            parentMember.addChild(member)
        }
        //stack.push(member)
    }

    override def enterClassDeclaration(ctx: ApexcodeParser.ClassDeclarationContext): Unit ={
        val member = if (ClassBodyMember.isInnerClass(ctx)) new InnerClassMember(ctx, filePath) else new ClassMember(ctx, filePath)
        registerMember(member)
        if (!member.isInstanceOf[InnerClassMember]) {
            //only top level classes shall be registered in the root of main tree
            tree.addMember(member)
        }
        memberScopeStack.push(member)

    }
    override def exitClassDeclaration(ctx: ApexcodeParser.ClassDeclarationContext): Unit = {
        memberScopeStack.pop()
    }

    override def enterTriggerDeclaration(ctx: TriggerDeclarationContext): Unit = {
        val member = new TriggerMember(ctx, filePath)
        registerMember(member)
        //only top level classes/triggers shall be registered in the root of main tree
        tree.addMember(member)

        memberScopeStack.push(member)
    }

    override def exitTriggerDeclaration(ctx: TriggerDeclarationContext): Unit = {
        memberScopeStack.pop()
    }

    override def enterInterfaceDeclaration(ctx: ApexcodeParser.InterfaceDeclarationContext): Unit ={
        val member = if (ClassBodyMember.isInnerInterface(ctx)) new InnerInterfaceMember(ctx, filePath) else InterfaceMember(ctx, filePath)
        registerMember(member)
        if (!ClassBodyMember.isInnerInterface(ctx)) {
            //only top level interfaces shall be registered in the root of main tree
            tree.addMember(member)
        }
        memberScopeStack.push(member)

    }
    override def exitInterfaceDeclaration(ctx: ApexcodeParser.InterfaceDeclarationContext): Unit = {
        memberScopeStack.pop()

    }
    override def enterCatchClause(ctx: CatchClauseContext): Unit = {
        val member = new CatchClauseMember(ctx, filePath)
        registerMember(member)
        memberScopeStack.push(member)
    }

    override def exitCatchClause(ctx: CatchClauseContext): Unit = {
        memberScopeStack.pop()
    }

    override def enterEnumDeclaration(ctx: EnumDeclarationContext): Unit = {
        val member = new EnumMember(ctx, filePath)
        registerMember(member)
        memberScopeStack.push(member)
    }

    override def exitEnumDeclaration(ctx: EnumDeclarationContext): Unit = {
        memberScopeStack.pop()
    }

    override def enterEnumConstant(ctx: EnumConstantContext): Unit = {
        val member = new EnumConstantMember(ctx, filePath)
        registerMember(member)

    }

    override def enterPropertyDeclaration(ctx: PropertyDeclarationContext): Unit = {
        val member = new PropertyMember(ctx, filePath)
        registerMember(member)

    }

    override def enterClassBodyDeclaration(ctx: ClassBodyDeclarationContext): Unit = {
        val member = ctx match {
            case ClassMethodMember(context) => new ClassMethodMember(ctx, parser, filePath)
            case _ => null;//new ClassBodyMember(ctx)
        }
        if (null != member) {
            registerMember(member)
        }
        member match {
            case m: ClassMethodMember =>
                memberScopeStack.push(m)
            case _ =>
        }
    }

    override def exitClassBodyDeclaration(ctx: ClassBodyDeclarationContext): Unit = {
        ctx match {
            case ClassMethodMember(context) =>
                memberScopeStack.pop()
            case _ =>
        }
    }

    override def enterInterfaceBodyDeclaration(ctx: InterfaceBodyDeclarationContext): Unit = {
        val member = ctx match {
            case InterfaceMethodMember(context) => new InterfaceMethodMember(ctx, parser, filePath)
            case _ => null;//new ClassBodyMember(ctx)
        }
        if (null != member) {
            registerMember(member)
        }
        member match {
            case m: InterfaceMethodMember =>
                memberScopeStack.push(m)
            case _ =>
        }
    }

    override def exitInterfaceBodyDeclaration(ctx: InterfaceBodyDeclarationContext): Unit = {
        ctx match {
            case InterfaceMethodMember(context) =>
                memberScopeStack.pop()
            case _ =>
        }
    }



    override def enterTriggerBodyDeclaration(ctx: TriggerBodyDeclarationContext): Unit = {
        val member = ctx match {
            case TriggerMethodMember(context) => new TriggerMethodMember(ctx, parser, filePath)
            case _ => null;//new ClassBodyMember(ctx)
        }
        if (null != member) {
            registerMember(member)
        }
        member match {
            case m: TriggerMethodMember =>
                memberScopeStack.push(m)
            case _ =>
        }
    }

    override def exitTriggerBodyDeclaration(ctx: TriggerBodyDeclarationContext): Unit = {
        ctx match {
            case TriggerMethodMember(context) =>
                memberScopeStack.pop()
            case _ =>
        }
    }

    override def enterStatement(ctx: StatementContext): Unit = {
        val member = new StatementMember(ctx)
        registerMember(member)
        memberScopeStack.push(member)
    }

    override def exitStatement(ctx: StatementContext): Unit = {
        memberScopeStack.pop()
    }

    private val identifierMap = mutable.Map[String, Set[Identifier]]()
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
            val identifier = if (memberScopeStack.isEmpty)
                                new Identifier(node, None)
                            else
                                new Identifier(node, Some(memberScopeStack.top))
            identifierMap.get(name) match {
              case Some(nodes) =>
                  identifierMap(name) = nodes + identifier
              case None =>
                  identifierMap(symbol.getText) = Set(identifier)
            }
        }
    }


    override def enterLocalVariableDeclaration(ctx: LocalVariableDeclarationContext): Unit = {
        //cycle through all variables declared in the current expression
        //String a, b, c;
        for (varDeclaratorCtx <- ctx.variableDeclarators().variableDeclarator().asScala) {
            val member = new LocalVariableMember(ctx, varDeclaratorCtx, filePath)
            registerMember(member)
        }
    }

    /**
     * this covers FOR cycle defined as
     * for (MyClass cls : collection) {...}
     **/
    override def enterEnhancedForControl(ctx: EnhancedForControlContext): Unit = {
        val member = new EnhancedForLocalVariableMember(ctx, filePath)
        registerMember(member)
    }

    override def enterFieldDeclaration(ctx: FieldDeclarationContext): Unit = {
        val member = new FieldMember(ctx, filePath)
        registerMember(member)
    }


    override def enterFormalParameter(ctx: FormalParameterContext): Unit = {
        val member = new MethodParameter(ctx, filePath)
        registerMember(member)

    }


    override def enterCreator(ctx: CreatorContext): Unit = {
        val member = new CreatorMember(ctx)
        registerMember(member)
        memberScopeStack.push(member)
    }

    override def exitCreator(ctx: CreatorContext): Unit = {
        memberScopeStack.pop()
    }

    /**
     * @param name - name of identifier (e.g. variable name or method name)
     * @return list of identifiers matching this name
     */
    def getIdentifiers(name: String): Option[Set[Identifier]] = {
        identifierMap.get(name)
    }

    override def visitErrorNode(node: ErrorNode): Unit = {
        checkTargetMember(node.getSymbol)
    }

    override def enterEveryRule(ctx: ParserRuleContext): Unit = {
        checkTargetMember(ctx.getStart)
    }


    /**
     * check if current node is part of the token we are trying to auto-complete
     */
    private def checkTargetMember(token: Token): Unit = {
        if (line > 0 && memberScopeStack.nonEmpty) {
            //we are in progress of parsing file where completion needs to be done
            //println("target node=" + token.getText)
            //println("target node.Line=" + token.getLine)
            if (token.getLine == line && token.getCharPositionInLine < column) {
                caretScopeMember = Some(memberScopeStack.top)
            } else {
                //alternative way of detecting scope - when we have caret offset information
                caret match {
                    case Some(_caret) if token.getStartIndex < _caret.getOffset && token.getStopIndex >= _caret.getOffset =>
                        //looks like current token surrounds caret
                        caretScopeMember = Some(memberScopeStack.top)
                    case _ =>
                }
            }

        }
    }
}

class Identifier(node: TerminalNode, parentMember: Option[AnonymousMember])

