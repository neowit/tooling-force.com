package com.neowit.apex.completion

import com.neowit.apex.Session
import com.neowit.apex.parser.antlr.SoqlParser._
import com.neowit.apex.parser.antlr.{SoqlParser, SoqlLexer}
import com.neowit.apex.parser.{ApexParserUtils, ApexTree, Member}
import com.sforce.soap.partner.ChildRelationship
import org.antlr.v4.runtime.tree.ParseTree
import org.antlr.v4.runtime._
import scala.collection.JavaConversions._

class SoqlCompletionResult(val options: List[Member], val isSoqlStatement: Boolean)

class SoqlAutoComplete (token: Token, line: Int, column: Int, cachedTree: ApexTree, session: Session) {

    def listOptions:SoqlCompletionResult = {

        //val soqlString = stripBrackets(token.getText)
        val soqlString = token.getText
        val expressionTokens = getCaretStatement(soqlString)
        if (expressionTokens.isEmpty) {
            //looks like the file is too broken to get to the point where caret resides
            return new SoqlCompletionResult(List(), isSoqlStatement = true)
        }


        val input = new ANTLRInputStream(soqlString)
        val lexer = new SoqlLexer(input)
        val tokens = new CommonTokenStream(lexer)
        val parser = new SoqlParser(tokens)
        ApexParserUtils.removeConsoleErrorListener(parser)
        parser.setErrorHandler(new SoqlCompletionErrorStrategy())
        val tree = parser.soqlCodeUnit()
        /*
        val walker = new ParseTreeWalker()
        val extractor = new SoqlTreeListener(parser, line, column)
        walker.walk(extractor, tree)
        */

        val finalContext = expressionTokens.head.finalContext
        val definition = findSymbolType(expressionTokens, tree)

        definition match {
          case Some(soqlTypeMember) =>
              new SoqlCompletionResult(removeDuplicates(resolveExpression(soqlTypeMember, expressionTokens), finalContext), isSoqlStatement = true)
          case None =>
              println("Failed to find definition")
              new SoqlCompletionResult(Nil, isSoqlStatement = false)
        }
    }

    private def resolveExpression(parentType: Member, expressionTokens: List[AToken]): List[Member] = {
        if (Nil == expressionTokens) {
            return parentType.getChildren
        }
        val token: AToken = expressionTokens.head
        if (token.symbol.isEmpty) {
            return parentType.getChildren
        }
        val tokensToGo = expressionTokens.tail
        parentType.getChild(token.symbol) match {
            case Some(_childMember) =>
                return resolveExpression(_childMember, tokensToGo)
            case None if tokensToGo.isEmpty => //parent does not have a child with this identity, return partial match
                val partialMatchChildren = filterByPrefix(parentType.getChildren, token.symbol)
                if (partialMatchChildren.isEmpty) {
                    //TODO
                    //return getApexTypeMembers(token.symbol)
                } else {
                    return partialMatchChildren
                }
            case _ => //check if parentType has child which has displayable identity == token.symbol
        }

        Nil
    }

    private def filterByPrefix(members: List[Member], prefix: String): List[Member] = {
        members.filter(_.getIdentity.toLowerCase.startsWith(prefix.toLowerCase))
    }

    /**
     * using Apex token and line/column of caret in currently edited file convert these to coordinates inside SOQL string
     * @param token - which contains SOQL expression [select ...]
     * @param line - original caret line num in the source file
     * @param column - original caret column in the source file
     * @return - index of caret in SOQL string
     */
    private def getCaretPositionInSoql(token: Token, line: Int, column: Int): (Int, Int) = {
        val caretLine = line - token.getLine
        val caretColumn = if (line == token.getLine ) {
            //SOQL starts in the middle of Apex line
            column - token.getCharPositionInLine
        } else {
            column
        }
        (caretLine, caretColumn)
    }


    private def getCaretStatement(soqlString: String): List[AToken] = {
        val (caretLine, caretColumn) = getCaretPositionInSoql(token, line, column)

        val caret = new CaretInString(caretLine + 1, caretColumn, soqlString)
        val input = new ANTLRInputStream(soqlString)
        val tokenSource = new CodeCompletionTokenSource(new SoqlLexer(input), caret)
        val tokens: CommonTokenStream = new CommonTokenStream(tokenSource)  //Actual
        //val tokens: CommonTokenStream = new CommonTokenStream(getLexer(file))
        val parser = new SoqlParser(tokens)
        ApexParserUtils.removeConsoleErrorListener(parser)
        parser.setBuildParseTree(true)
        parser.setErrorHandler(new CompletionErrorStrategy())

        //parse tree until we reach caret caretAToken
        try {
            parser.soqlCodeUnit()
        } catch {
            case ex: CaretReachedException =>
                return CompletionUtils.breakExpressionToATokens(ex)
            case e:Throwable =>
                println(e.getMessage)
        }

        List()

    }

    /**
     * select Id, <caret> from Account
     *
     * @return typeMember - in the above example: typeMember will be FromTypeMember - "Account"
     */
    private def findSymbolType(expressionTokens: List[AToken], tree: SoqlCodeUnitContext ): Option[Member] = {

        def getFromMember(ctx: ParserRuleContext): Option[SoqlMember] = {
            getFrom(ctx) match {
                case x :: xs => Some(x)
                case _ => None

            }
        }

        expressionTokens.head.finalContext match {
            case ctx: SelectItemContext =>
                //started new field in Select part
                getFromMember(tree)
            case ctx: FieldItemContext =>
                //part of field (most likely trying to complete relationship)
                getFromMember(tree)
            case ctx: AggregateFunctionContext =>
                //started new field inside aggregate function which has a variant without argument
                getFromMember(tree)
            case ctx: FieldNameContext if ctx.getParent.isInstanceOf[AggregateFunctionContext] =>
                //started new field inside aggregate function which requires an argument
                getFromMember(tree)
            case ctx: FieldNameContext if ctx.getParent.isInstanceOf[FieldItemContext] =>
                //looks like this is part of relationship e.g. Owner.<caret>
                getFromMember(tree)
            case ctx: ObjectTypeContext if ctx.getParent.isInstanceOf[FromStatementContext] =>
                //looks like caret is just after 'FROM' keyword
                Some(new DBModelMember(session))
            case ctx: WhereConditionExpressionContext =>
                //started new field inside where
                getFromMember(tree)
            case ctx: RelationshipPathContext =>
                //started new relationship name inside nested select
                getFromMember(tree) match {
                  case Some(fromMember) if fromMember.isInstanceOf[FromTypeMember] =>
                      Some(new ChildRelationshipsContainerMember(fromMember.asInstanceOf[FromTypeMember]))
                  case None => None
                }

            case _ => None //TODO
        }
    }

    private def getFrom(ctx: ParserRuleContext): List[FromTypeMember] = {

        val soqlCodeUnitContextOption = if (ctx.isInstanceOf[SoqlCodeUnitContext]) Some(ctx) else ApexParserUtils.getParent(ctx, classOf[SoqlCodeUnitContext])
        soqlCodeUnitContextOption match {
            case Some(soqlStatement) =>
                val objTypeMembers = ApexParserUtils.findChild(soqlStatement, classOf[FromStatementContext]) match {
                  case Some(fromStatement) =>
                      fromStatement.objectType().map(new FromTypeMember(_, session))
                  case None => Nil
                }
                objTypeMembers.toList
            case None => Nil
        }
    }

    /**
     * if caret represents field name, then remove all already entered (simple) field names from list of options
     * @param members - member list to reduce
     * @param finalContext - caret context
     * @return - reduced list of members
     */
    private def removeDuplicates(members: List[Member], finalContext: ParseTree): List[Member] = {
        //need to keep relationship fields because these can be useful more than once in the same SELECT statement
        def isReferenceMember(m: Member): Boolean = {
            m.isInstanceOf[SObjectFieldMember] && m.asInstanceOf[SObjectFieldMember].isReference
        }
        val membersWithoutDuplicates = finalContext match {
            case ctx: SelectItemContext if ctx.getParent.isInstanceOf[SelectStatementContext] =>
                //started new field in Select part
                val existingFieldNames = ApexParserUtils.findChildren(ctx.getParent, classOf[FieldNameContext]).map(fNameNode => fNameNode.Identifier(0).toString).toSet
                members.filter(m => !existingFieldNames.contains(m.getIdentity) || isReferenceMember(m))

            case _ => members
        }
        //remove all SObject methods, leave Fields and Relationships only
        membersWithoutDuplicates.filterNot(m => m.isInstanceOf[ApexMethod])

    }
}

class CaretInString(line:  Int, column: Int, str: String) extends Caret (line, column){
    def getOffset: Int = {
        ApexParserUtils.getOffset(str, line, column)
    }
}

trait SoqlMember extends Member {
    override def isStatic: Boolean = false
    override def toString: String = getIdentity
}

class FromTypeMember(ctx: ObjectTypeContext, session: Session) extends SoqlMember {
    override def getIdentity: String = ctx.Identifier().getSymbol.getText

    override def getType: String = getIdentity

    override def getSignature: String = getIdentity

    private def getSObjectMember: Option[DatabaseModelMember] = DatabaseModel.getModelBySession(session) match {
        case Some(dbModel) => dbModel.getSObjectMember(getIdentity)
        case _ => None
    }

    override def getChildren: List[Member] = {
        getSObjectMember match {
            case Some(sobjectMember) =>
                sobjectMember.getChildren
            case None => Nil
        }
    }

    override def getChild(identity: String, withHierarchy: Boolean): Option[Member] = {
        getSObjectMember match {
            case Some(sobjectMember) =>
                sobjectMember.getChild(identity)
            case None => None
        }
    }

    def getChildRelationships: Array[com.sforce.soap.partner.ChildRelationship]  = {
        getSObjectMember match {
            case Some(member) if member.isInstanceOf[SObjectMember] =>
                val sobjectMember = member.asInstanceOf[SObjectMember]
                //force sobject member to load its children (including relationships)
                sobjectMember.getChildren
                //return actual relationships
                sobjectMember.asInstanceOf[SObjectMember].getChildRelationships
            case None => Array()
        }
    }
}

class DBModelMember(session: Session) extends Member {
    /**
     * @return
     * for class it is class name
     * for method it is method name + string of parameter types
     * for variable it is variable name
     * etc
     */
    override def getIdentity: String = "APEX_DB_MODEL"

    override def getType: String = getIdentity

    override def getSignature: String = getIdentity

    override def isStatic: Boolean = true

    override def getChildren: List[Member] = DatabaseModel.getModelBySession(session) match {
        case Some(dbModel) => dbModel.getSObjectMembers
        case None => Nil
    }

    override def getChild(identity: String, withHierarchy: Boolean): Option[Member] = {
        DatabaseModel.getModelBySession(session) match {
            case Some(dbModel) => dbModel.getSObjectMember(getIdentity)
            case None => None
        }
    }
}

/**
 * artificial proxy which contains list of relationships for specific object type
 */
class ChildRelationshipsContainerMember(fromTypeMember: FromTypeMember) extends SoqlMember {

    override def getIdentity: String = fromTypeMember.getIdentity + "CHILD_RELATIONSHIPS"

    override def getType: String = getIdentity

    override def getSignature: String = getIdentity

    override def getChildren: List[Member] = fromTypeMember.getChildRelationships.toList.filter(null != _.getRelationshipName).map(new ChildRelationshipMember(_))

    override def getChild(identity: String, withHierarchy: Boolean): Option[Member] = super.getChild(identity, withHierarchy)
}

class ChildRelationshipMember(relationship: ChildRelationship) extends SoqlMember {

    override def getIdentity: String = relationship.getRelationshipName

    override def getType: String = getIdentity

    override def getSignature: String = getIdentity + " -> " + relationship.getChildSObject
}

class SoqlCompletionErrorStrategy extends DefaultErrorStrategy {

    override def singleTokenDeletion(recognizer: Parser): Token = {
        if (SoqlLexer.FROM == recognizer.getInputStream.LA(1))
            null //force singleTokenInsertion to make FROM parseable
        else
            super.singleTokenDeletion(recognizer)
    }
}