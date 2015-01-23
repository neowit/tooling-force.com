package com.neowit.apex.completion

import com.neowit.apex.Session
import com.neowit.apex.completion.models.ApexMethod
import com.neowit.apex.parser.antlr.SoqlParser._
import com.neowit.apex.parser.antlr.{SoqlParser, SoqlLexer}
import com.neowit.apex.parser._
import org.antlr.v4.runtime.tree.ParseTree
import org.antlr.v4.runtime._
import scala.collection.JavaConversions._

class SoqlCompletionResult(val options: List[Member], val isSoqlStatement: Boolean, val expressionTokens: List[AToken] = Nil )

class SoqlAutoComplete (token: Token, line: Int, column: Int, cachedTree: ApexTree, session: Session) {

    def listOptions:SoqlCompletionResult = {

        //val soqlString = stripBrackets(token.getText)
        val soqlString = token.getText
        val (expressionTokens, caretReachedException) = getCaretStatement(soqlString)
        if (expressionTokens.isEmpty) {
            //looks like the file is too broken to get to the point where caret resides
            return new SoqlCompletionResult(List(), isSoqlStatement = true)
        }
        //check if this is apex bound expression inside SOQL statement
        //i.e. [select ... from ... where Something > :expr<caret>]
        if (expressionTokens.nonEmpty) {
            expressionTokens.head.token match {
                case Some(_token) if caretReachedException.isDefined && _token.getTokenIndex > 0=>
                    val tokens = caretReachedException.get.getInputStream
                    val tokenBeforeExpression = tokens.get(_token.getTokenIndex -1)
                    if (":" == tokenBeforeExpression.getText) {
                        //this looks like a bound variable in SOQL
                        return new SoqlCompletionResult(List(), isSoqlStatement = false, expressionTokens)
                    }

                case _ =>
            }
        }


        val input = new ANTLRInputStream(soqlString)
        val lexer = new SoqlLexer(input)
        val tokens = new CommonTokenStream(lexer)
        val parser = new SoqlParser(tokens)
        ApexParserUtils.removeConsoleErrorListener(parser)
        //parser.setErrorHandler(new SoqlCompletionErrorStrategy())
        val tree = parser.soqlCodeUnit()
        /*
        val walker = new ParseTreeWalker()
        val extractor = new SoqlTreeListener(parser, line, column)
        walker.walk(extractor, tree)
        */

        val finalContext = expressionTokens.head.finalContext
        val definition = findSymbolType(expressionTokens, tree, tokens, caretReachedException, parser)

        definition match {
          case Some(soqlTypeMember) =>
              new SoqlCompletionResult(removeDuplicates(resolveExpression(soqlTypeMember, expressionTokens), finalContext, tree), isSoqlStatement = true)
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


    private def getCaretStatement(soqlString: String): (List[AToken], Option[CaretReachedException]) = {
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
                return (breakExpressionToATokens(ex), Some(ex))
            case e:Throwable =>
                println(e.getMessage)
        }

        (List(), None)

    }
    def breakExpressionToATokens(ex: CaretReachedException): List[AToken] = {
        val expressionTokens = CompletionUtils.breakExpressionToATokens(ex)
        if (expressionTokens.nonEmpty && Set("select", "from").contains(expressionTokens(expressionTokens.size-1).symbol.toLowerCase)) {
            //most likely attempting to complete first field name in SELECT ... part
            val lastToken = expressionTokens(expressionTokens.size-1)
            return expressionTokens.init ++ List(new AToken(lastToken.index + 1, "", "", None, ex.finalContext))
        }
        expressionTokens
    }

    /**
     * select Id, _caret_ from Account
     *
     * @return typeMember - in the above example: typeMember will be FromTypeMember - "Account"
     */
    private def findSymbolType(expressionTokens: List[AToken], tree: SoqlCodeUnitContext, tokens: TokenStream,
                               caretReachedException: Option[CaretReachedException], parser: SoqlParser ): Option[Member] = {


        val finalContext = expressionTokens.head.finalContext
        val isSubquery = ApexParserUtils.getParent(finalContext, classOf[SubqueryContext]).isDefined

        finalContext match {
            case ctx: ObjectTypeContext if ctx.getParent.isInstanceOf[FromStatementContext] =>
                //looks like caret is just after 'FROM' keyword
                Some(new DBModelMember(session))
            case ctx: SoqlCodeUnitContext =>
                //looks like caret is just after 'FROM' keyword
                Some(new DBModelMember(session))
            case ctx: RelationshipPathContext =>
                //started new relationship name inside nested select
                getTopLevelFrom(tokens, session) match {
                  case Some(fromMember) if fromMember.isInstanceOf[FromTypeMember] =>
                      Some(new ChildRelationshipsContainerMember(fromMember.asInstanceOf[FromTypeMember]))
                  case None => None
                }
            case ctx: FieldItemContext if isSubquery =>
                //this is a sub query
                if (caretReachedException.isDefined && null != caretReachedException.get.caretToken.prevToken
                    && "from" == caretReachedException.get.caretToken.prevToken.getText) {
                    //this is not field item, it looks more like a relationship name in FROM part of inside nested select
                    getTopLevelFrom(tokens, session) match {
                        case Some(fromMember) if fromMember.isInstanceOf[FromTypeMember] =>
                            Some(new ChildRelationshipsContainerMember(fromMember.asInstanceOf[FromTypeMember]))
                        case None => None
                    }
                } else {
                    getSubqueryFrom(tree, tokens, caretReachedException)
                }
            case ctx: FieldNameContext if isSubquery =>
                //this is a sub query
                getSubqueryFrom(tree, tokens, caretReachedException)
            case ctx: WhereConditionExpressionContext if isSubquery =>
                //this is a sub query
                getSubqueryFrom(tree, tokens, caretReachedException) match {
                    case Some(_fromTypeMember) =>
                        Some(new WhereLeftItemMember(_fromTypeMember, parser))
                    case None => None
                }
            case ctx: WhereConditionExpressionContext if !isSubquery =>
                //this is a sub query
                getFromMember(tree, tokens, caretReachedException) match {
                    case Some(_fromTypeMember) =>
                        Some(new WhereLeftItemMember(_fromTypeMember, parser))
                    case None => None
                }

            case ctx: ConditionExpressionContext=>
                Some(new WhereRightItemMember())

            case ctx: SelectStatementContext =>
                //first field in empty SELECT
                getFromMember(tree, tokens, caretReachedException) match {
                  case Some(fromMember) if fromMember.isInstanceOf[FromTypeMember] =>
                      Some(new SelectItemMember(fromMember.asInstanceOf[FromTypeMember], parser))
                  case None => None
                }
            case ctx: SelectItemContext =>
                //second or more field in non empty SELECT
                getFromMember(tree, tokens, caretReachedException) match {
                    case Some(fromMember) =>
                        Some(new SelectItemMember(fromMember, parser))
                    case None => None
                }
            case _ => getFromMember(tree, tokens, caretReachedException)
        }
    }

    private def getTopLevelFrom(tokens: TokenStream, session: Session): Option[FromTypeMember] = {
        SoqlParserUtils.findFromToken(tokens, tokens.get(1)) match {
            case Some(fromToken) if tokens.size() > fromToken.getTokenIndex + 1=> //top level FROM object type
                Some(new FromTypeMember(tokens.get(fromToken.getTokenIndex + 1), session))
            case None => None
        }
    }

    def getFromMember(ctx: ParserRuleContext, tokens: TokenStream, caretReachedException: Option[CaretReachedException]): Option[SoqlMember] = {
        caretReachedException match {
            case Some(_caretReachedException) => getFrom(tokens, _caretReachedException) match {
                case x :: xs => Some(x)
                case _ => None
            }
            case None => //fall back to what parser managed to deduce from current SQOL expression
                getFrom(ctx) match {
                    case x :: xs => Some(x)
                    case _ => None

                }
        }
    }
    private def getSubqueryFrom(ctx: ParserRuleContext, tokens: TokenStream, caretReachedException: Option[CaretReachedException]): Option[SubqueryFromTypeMember] = {
        getFromMember(ctx, tokens, caretReachedException) match {
            case Some(subqueryFromMember) =>
                getTopLevelFrom(tokens, session) match {
                    case Some(fromTypeMember) => //top level FROM object type
                        Some(new SubqueryFromTypeMember(subqueryFromMember.getIdentity, fromTypeMember, session))
                    case None => None

                }
            case None => None
        }
    }

    /**
     * find FROM _Object_Type_ in provided token stream
     * this method first tries to detect scope and find FROM in that scope
     * if this method fails then fall back to FromStatementContext detected by main antlr parser
     * @param tokens - tokes representing current SOQL expression
     * @param caretReachedException - used mainly for information about caret location
     * @return
     *         current version returns only 1 object type (ignoring the fact that SOQL supports multiple
     *         objects in main FROM part)
     */
    private def getFrom(tokens: TokenStream, caretReachedException: CaretReachedException ): List[FromTypeMember] = {
        SoqlParserUtils.findFromToken(tokens, caretReachedException.caretToken) match {
            case Some(fromToken) =>
                val objectTypeTokenIndex = fromToken.getTokenIndex + 1
                if (objectTypeTokenIndex < tokens.size()) {
                    return List(new FromTypeMember(tokens.get(objectTypeTokenIndex), session))
                }
            case None =>
        }
        getFrom(caretReachedException.finalContext)
    }

    /**
     * less reliable alternative to getFrom(tokens: TokenStream, caretReachedException: CaretReachedException ):
     * if SOQL expression is sufficiently broken then this method may return wrong FROM clause (e.g. from nested SELECT)
     * @return
     */
    private def getFrom(ctx: RuleContext): List[FromTypeMember] = {

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
    private def removeDuplicates(members: List[Member], finalContext: ParseTree, tree: SoqlCodeUnitContext): List[Member] = {
        //need to keep relationship fields because these can be useful more than once in the same SELECT statement
        def isReferenceMember(m: Member): Boolean = {
            m.isInstanceOf[SObjectRelationshipFieldMember]
        }
        val membersWithoutDuplicates = finalContext match {
            case ctx: SelectItemContext if ctx.getParent.isInstanceOf[SelectStatementContext] =>
                //started new field in Select part
                val existingFieldNames =
                    ApexParserUtils.findChild(tree, classOf[SelectStatementContext]) match {
                  case Some(selectStatementContext) =>
                      ApexParserUtils.findChildren(selectStatementContext, classOf[FieldNameContext]).map(fNameNode => fNameNode.Identifier(0).toString).toSet
                  case None => Set()
                }
                members.filter(m => !existingFieldNames.contains(m.getIdentity) || isReferenceMember(m))
            case ctx: FromStatementContext =>
                // check if we are dealing with last field in main SELECT mistakenly associated with FROM context
                val existingFieldNames =
                    ApexParserUtils.getParent(ctx, classOf[SoqlStatementContext]) match {
                        case Some(soqlStatementContext) =>
                            ApexParserUtils.findChildren(soqlStatementContext, classOf[FieldNameContext]).map(fNameNode => fNameNode.Identifier(0).toString).toSet
                        case None => Set()
                    }
                members.filter(m => !existingFieldNames.contains(m.getIdentity) || isReferenceMember(m))

            case ctx: FieldItemContext if ApexParserUtils.getParent(ctx, classOf[SubqueryContext]).isDefined   =>
                //sub-query
                val existingFieldNames =
                    ApexParserUtils.getParent(ctx, classOf[SubqueryContext]) match {
                        case Some(subqueryContext) =>
                            ApexParserUtils.findChildren(subqueryContext, classOf[FieldNameContext]).map(fNameNode => fNameNode.Identifier(0).toString).toSet
                        case None => Set()
                    }
                members.filter(m => !existingFieldNames.contains(m.getIdentity) || isReferenceMember(m))

            case _ => members
        }
        //remove all SObject methods, leave Fields and Relationships only
        membersWithoutDuplicates.filterNot(m => m.isInstanceOf[ApexMethod])

    }
}

class CaretInString(line:  Int, column: Int, str: String) extends Caret (line, column){
    lazy val getOffset: Int =  getOffset(str, line, column - 1)

    /**
     * this method is different from ApexParserUtils.getOffset
     * in this method pos is incremented every time when "\n" character is encountered
     * @param line -  line - starts from 1
     * @param startIndex - column in line, starts from zero
     *                   e.g.: in vim columns start from 1,
     *                   so before passing value to this method make sure to do: column - 1
     * @return
     */
    def getOffset(text: String, line: Int, startIndex: Int): Int = {
        //val bytes = text.take
        var lineNum: Int = 1
        var pos = 0

        while ( lineNum < line && pos < text.length ) {
            val ch = text.charAt(pos)
            if ('\n' == ch) {
                if (lineNum < line) {
                    pos = pos + 1
                }
                lineNum += 1
            }
            if (lineNum < line) {
                pos = pos + 1
            }
        }
        val offset = pos + startIndex
        offset
    }
}


class SoqlCompletionErrorStrategy extends DefaultErrorStrategy {

    override def singleTokenDeletion(recognizer: Parser): Token = {
        if (SoqlLexer.FROM == recognizer.getInputStream.LA(1))
            null //force singleTokenInsertion to make FROM parseable
        else
            super.singleTokenDeletion(recognizer)
    }
}