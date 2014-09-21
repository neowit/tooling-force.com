package com.neowit.apex.completion

import java.io.{FileInputStream, File}

import com.neowit.apex.parser.{TreeListener, Member}
import com.neowit.apex.parser.TreeListener.ApexTree
import com.neowit.apex.parser.antlr.{ApexcodeLexer, ApexcodeParser}
import com.neowit.apex.parser.antlr.ApexcodeParser._
import org.antlr.v4.runtime.tree.{TerminalNode, ParseTree, ParseTreeWalker}
import org.antlr.v4.runtime._

class AutoComplete2(file: File, line: Int, column: Int, cachedTree: ApexTree = Map[String, Member]()) {
    def listOptions:List[Member] = {
        val expressionTokens = getCaretStatement
        val lexer = getLexer(file)
        val tokens = new CommonTokenStream(lexer)
        val parser = new ApexcodeParser(tokens)
        val tree = parser.compilationUnit()
        val walker = new ParseTreeWalker()
        val extractor = new TreeListener(parser)
        walker.walk(extractor, tree)

        //TODO
        //now for each caretAToken find its type and use it to resolve subsequent caretAToken
        //List( someClassInstance, method(), goes, her)
        val fullApexTree = cachedTree ++ extractor.tree
        val definition = findSymbolType(expressionTokens.head, extractor)
        definition match {
            case Some((parseTree, typeContext)) =>
                //typeContext contains node type, e.g. SomeClass of the first token in expressionTokens
                val startType = typeContext.getText
            case None =>
        }


        List()
    }
    /*
    private def resolveType(parentType: String, token: AToken, apexTree: ApexTree): Option[String] = {
        apexTree.get(typeContext.getText) match {
            case Some((parseTree, typeContext)) =>
            //typeContext contains node type, e.g. Integer

            case None =>
        }

    }
    */
    /*
    def listOptions(definitionNode: ParseTree, typeContext:ParserRuleContext, extractor: TreeListener): Option[List[Member]] = {
        println("caret type =" + typeContext.getText)
        val fullApexTree = cachedTree ++ extractor.tree
        fullApexTree.get(typeContext.getText) match {
            case Some(member) =>
                val members = member.getChildren
                println("Potential signatures:")
                println("-" + members.map(m => m.getIdentity + "=> " + m.getSignature).mkString("\n-"))
                println("\n\n" + members.map(m => m.toJson).mkString("\n "))
                return Some(members.toList)
            case None => //check if this is one of standard Apex types
                val typeName = typeContext.getText
                val staticMembers = ApexModel.getMembers(typeName)
                val instanceMethods = ApexModel.getInstanceMembers(typeName)
                if (staticMembers.nonEmpty || instanceMethods.nonEmpty) {
                    return Some(staticMembers ++ instanceMethods)
                } else {
                    return None
                }
        }

    }
    */

    /**
     * find blockStatement where caret reside
     *
     * vari| - list of all elements that can start with “vari” in current scope
     * variable.| - list of all methods of “variable” type
     * variable.andSome| - list of all methods of “variable” type that start with “andSome”
     * variable.andSome.| - list of all methods of property “andSome” of “variable”
     *
     * collection.| - list of all methods of collection (need to know collection type)
     * collection[0].| - list of all methods of element in collection (need to know element type)
     * @return
     */
    private def getCaretStatement: List[AToken] = {
        val caret = new Caret(line, column, file)
        val tokenSource = new CodeCompletionTokenSource(getLexer(file), caret)
        val tokens: CommonTokenStream = new CommonTokenStream(tokenSource)  //Actual
        //val tokens: CommonTokenStream = new CommonTokenStream(getLexer(file))
        val parser = new ApexcodeParser(tokens)

        parser.setBuildParseTree(true)
        parser.setErrorHandler(new CompletionErrorStrategy2())
        //val tree = parser.compilationUnit()
        //val walker = new ParseTreeWalker()
        //val extractor = new TreeListener(parser)
        //walker.walk(extractor, tree)

        //parse tree until we reach caret caretAToken
        try {
            val tree = parser.compilationUnit()
        } catch {
            case ex: CaretReachedException2 =>
                println("found caret?")
                //println(ex.getToken.getText)
                //listOptions(ex)
                return resolveExpression(ex)
            case e:Throwable =>
        }

        List()

    }

    private def findSymbolType(caretAToken: AToken, extractor: TreeListener): Option[(ParseTree, ParserRuleContext)] = {
        extractor.getIdentifiers(caretAToken.symbol) match {
            case Some(potentialDefinitionNodes) =>
                //val potentialDefinitionNodes = nodes - caretNode
                /* sort by proximity to caret*/
                //sortWith((x, y) => x.getSymbol.getLine  > y.getSymbol.getLine)
                //now find one which is closest to the caret and most likely definition
                val distances = collection.mutable.ArrayBuffer[(Int, ParseTree, TerminalNode)]()
                for (node <- potentialDefinitionNodes ) {
                    distanceToCommonParent(caretAToken.finalContext, node) match {
                        case Some((steps, commonParent)) =>
                            distances += ((steps, commonParent, node))
                        case None =>
                    }
                    println(node)
                }
                //find one with shortest distance
                distances.toList.sortWith((x: (Int, ParseTree, TerminalNode), y: (Int, ParseTree, TerminalNode)) => x._1 < y._1).headOption  match {
                    case Some((steps, commonParent, n)) =>
                        println(steps + "=" + n)
                        //node which we found is most likely a definition of one under caret
                        return getTypeParent(n) match {
                            case Some((pt, typeContext)) => Some((pt, typeContext))
                            case None => None
                        }

                    case None =>
                    //cursor symbol is not defined in the current file
                }
                /*
                //remove node which matches the caret
                nodes.toList.find(n => caretAToken.equals(n.getSymbol))  match {
                    case Some(caretNode) =>
                    case None =>
                }
                */
                println(potentialDefinitionNodes)
            case None =>
        }
        None
    }


    /**
     *
     * @param n - node which may contain type definition
     *       1:   e.g. String str;
     *       2:   str.
     *          in this case n will be node containing str in line 1:
     * @return
     */
    private def getTypeParent(n: ParseTree): Option[(ParseTree, ParserRuleContext)] = {
        if (null == n) {
            None
        } else {
            n match {
                case x: ClassDeclarationContext => Some((x, x.`type`()))
                //case x: TypeBoundContext => Some((x, x.`type`()))
                case x: MethodDeclarationContext => Some((x, x.`type`()))
                case x: FieldDeclarationContext => Some((x, x.`type`()))
                case x: InterfaceMethodDeclarationContext => Some((x, x.`type`()))
                case x: TypeArgumentContext => Some((x, x.`type`()))
                case x: FormalParameterContext => Some((x, x.`type`()))
                case x: LastFormalParameterContext => Some((x, x.`type`()))
                case x: AnnotationTypeElementRestContext => Some((x, x.`type`()))
                case x: LocalVariableDeclarationContext => Some((x, x.`type`()))
                case x: EnhancedForControlContext => Some((x, x.`type`()))
                case x: ExpressionContext => Some((x, x.`type`()))
                case x: PrimaryContext => Some((x, x.`type`()))
                case x: CatchClauseContext => Some((x, x.catchType().qualifiedName(0)))
                case _ => getTypeParent(n.getParent)
            }
        }

    }

    private def distanceToCommonParent(caret: ParseTree, node: TerminalNode): Option[(Int, ParseTree)] = {
        def getParents(parent: ParseTree, parents: List[ParseTree]): List[ParseTree] = {
            if (null != parent) {
                getParents(parent.getParent, parent :: parents)
            } else {
                parents
            }
        }
        val caretParents = getParents(caret.getParent, List()).reverse //from nearest parent to farthest
        val nodeParents = getParents(node.getParent, List()).reverse
        //in theory the nearest parent should be most likely definition
        //count number of steps to nearest parent
        var i = 0
        for (p <- caretParents) {
            val right = p.asInstanceOf[ParserRuleContext]
            nodeParents.find(n => {
                val left = n.asInstanceOf[ParserRuleContext]
                left.getStart.getStartIndex == right.getStart.getStartIndex
            }) match {
                case Some(commonParent) =>
                    //found common parent
                    return Some(i, commonParent)
                case None =>
            }
            i += 1
        }
        None
    }

    private case class AToken(index: Int, symbol: String, expression: String, token: Option[Token], finalContext: ParseTree) {
        def isArray: Boolean = expression.endsWith("]")
        def isMethod: Boolean = expression.endsWith(")")
        //def getToken:Token = if (caretAToken.isDefined) caretAToken.get else null
        def equals(otherToken: Token): Boolean = {
            token match {
              case Some(_token) =>
                  symbol == otherToken.getText && otherToken.getTokenIndex == _token.getTokenIndex
              case None =>
                    false
            }
        }


    }

    /**
     *
     * "method( completion.goes.her|)" => List(completion, goes, her)
     * completion.goes.her| => List(completion, goes, her)
     * "completion[some.num].goes.her|)" => List(completion[], goes, her)
     * "(completion.goes.her|" = => List(completion, goes, her)
     * "completion[some.num(other)].goes.her|)" => List(completion[], goes, her)
     * "completion[some.num(other[nn])].goes.her|)" => List(completion[], goes, her)
     * "completion(some.num[other]).goes.her|)" => List( completion(), goes, her)
     */
    private def resolveExpression(ex: CaretReachedException2): List[AToken] = {
        val expressionTokens = List.newBuilder[AToken]
        //at this point ex contains all information we need to build full statement on which ctrl+space was pressed
        //e.g.: MyClass.MyInnerClass.
        //ex: cls.
        //ex: cls[1].
        val cause = ex.cause
        val ctx = ex.finalContext
        //val parser = ex.recognizer

        val startToken = ctx.asInstanceOf[ParserRuleContext].getStart //e.g. 'str'
        val startTokenIndex = startToken.getTokenIndex //@333 = 333
        //val startTokenText = startToken.getText //e.g. 'str'
        val tokenStream = cause.getInputStream.asInstanceOf[CommonTokenStream]

        var index = startTokenIndex
        var currentToken = startToken
        var symbol = startToken.getText
        var expression = symbol
        var token:Option[Token] = Some(currentToken)

        while (currentToken.getType != CaretToken2.CARET_TOKEN_TYPE) {

            val newIndex = currentToken.getText match {
                case "(" =>
                    val i = consumeUntil(tokenStream, index + 1, ")")
                    expression = symbol + "()"
                    i
                case "[" =>
                    val i = consumeUntil(tokenStream, index + 1, "]")
                    expression = symbol + "[]"
                    i
                case "." => //end of expression
                    expressionTokens.+=(new AToken(index - 1, symbol, expression, token, ctx))
                    symbol = ""
                    expression = ""
                    token = None
                    //index + 1
                    index
                case _ =>
                    index
            }
            if (newIndex != index) {
                //expressionTokens.+=(tokenStream.get(newIndex))
                index = newIndex
            }
            index += 1
            currentToken = tokenStream.get(index)
            if (symbol.isEmpty && "\\w".r.findFirstIn(currentToken.getText).isDefined) {
                symbol = currentToken.getText
                expression = symbol
                token = Some(currentToken)
            }
        }
        expressionTokens.+=(new AToken(index - 1, symbol, expression, token, ctx))

        //get all tokens till caret
        //val allTokens = tokenStream.get(startIndex, endTokenIndex)

        println(cause.getCtx.getText)
        expressionTokens.result()
    }
    private def consumeUntil(tokenStream: CommonTokenStream, startTokenIndex: Integer, str: String): Int = {
        var index = startTokenIndex
        var currentToken = tokenStream.get(index)
        while (currentToken.getText != str && currentToken.getType != CaretToken2.CARET_TOKEN_TYPE) {
            currentToken.getText match {
                case "(" => index = consumeUntil(tokenStream, index + 1, ")") + 1
                case "[" => index = consumeUntil(tokenStream, index + 1, "]") + 1
                case _ => index += 1

            }
            currentToken = tokenStream.get(index)
        }
        index
    }

    private def getLexer(file: File): ApexcodeLexer = {
        val input = new ANTLRInputStream(new FileInputStream(file))
        val lexer = new ApexcodeLexer(input)
        lexer
    }

    class Caret(val line:  Int, val startIndex: Int, file: File) {
        private var tokenType: String = ""

        def getOffset: Int = {
            CompletionUtils.getOffset(file, line, startIndex)
        }

        def setType(path: String): Unit = {
            tokenType = path
        }

        def getType: String = {
            tokenType
        }

        def equals(node: Token): Boolean = {
            line == node.getLine && startIndex == node.getCharPositionInLine
        }
    }
    /**
     *
     * used to stop parser at current cursor position
     */
    class CodeCompletionTokenSource( source: TokenSource, caret: Caret) extends TokenSource {

        private var tokenFactory: TokenFactory[_] = CommonTokenFactory.DEFAULT
        private var caretToken: Token = null
        private val tokenFactorySourcePair = new misc.Pair(source, source.getInputStream)

        private val caretOffset = caret.getOffset
        private var prevToken: Token = null

        def getLine: Int = {
            source.getLine
        }

        def getCharPositionInLine: Int = {
            source.getCharPositionInLine
        }

        def getInputStream: CharStream = {
            source.getInputStream
        }

        def getSourceName: String = {
            source.getSourceName
        }
        def nextToken: Token = {
            if (null == caretToken) {
                var token: Token = source.nextToken
                println("caretAToken=" + token.toString)
                if (token.getStopIndex + 1 < caretOffset) {
                    //before target caretAToken
                    prevToken = token

                } else if (token.getStartIndex > caretOffset) {
                    //caretAToken = CaretToken(tokenFactorySourcePair, Token.DEFAULT_CHANNEL, caretOffset, caretOffset)
                    //found target?
                    //caretToken = prevToken
                    val t = CaretToken2(tokenFactorySourcePair, Token.DEFAULT_CHANNEL, caretOffset, caretOffset)
                    t.setOriginalToken(token)
                    t.prevToken = prevToken
                    token = t
                    caretToken = token
                } else {
                    if (token.getStopIndex + 1 == caretOffset && token.getStopIndex >= token.getStartIndex) {
                        if (!isWordToken(token)) {
                            return token
                        }
                    }
                    val t = CaretToken2(token)
                    t.prevToken = prevToken
                    token = t
                    caretToken = token
                }
                return token
            }

            throw new UnsupportedOperationException("Attempted to look past the caret.")
        }
        def getTokenFactory: TokenFactory[_] = {
            tokenFactory
        }

        def setTokenFactory(tokenFactory: TokenFactory[_]) {
            source.setTokenFactory(tokenFactory)
            this.tokenFactory = if (tokenFactory != null) tokenFactory else CommonTokenFactory.DEFAULT
        }
    }

    protected def isWordToken(token: Token): Boolean = {
        CompletionUtils.isWordToken(token)
    }
}

class CaretReachedException2(val recognizer: Parser, val finalContext: RuleContext, val cause: RecognitionException)
    extends RuntimeException(cause) {

}

class CompletionErrorStrategy2 extends DefaultErrorStrategy {

    override def reportError(recognizer: Parser, e: RecognitionException) {
        if (e != null && e.getOffendingToken != null &&
            e.getOffendingToken.getType == CaretToken2.CARET_TOKEN_TYPE) {
            return
        }
        super.reportError(recognizer, e)
    }


    override def recover(recognizer: Parser, e: RecognitionException) {
        if (e != null && e.getOffendingToken != null &&
            e.getOffendingToken.getType == CaretToken2.CARET_TOKEN_TYPE) {
            throw new CaretReachedException2(recognizer, recognizer.getContext, e)
        }
        super.recover(recognizer, e)
    }

}


object CaretToken2 {
    final val CARET_TOKEN_TYPE: Int = -2
    def apply(tokenFactorySourcePair: misc.Pair[TokenSource, CharStream], channel: Int, start: Int, stop: Int) = {
        new org.antlr.v4.runtime.CommonToken(tokenFactorySourcePair, CaretToken2.CARET_TOKEN_TYPE, channel, start, stop) with CaretTokenTrait
    }

    def apply(oldToken: Token) = {
        val token = new org.antlr.v4.runtime.CommonToken(oldToken) with CaretTokenTrait
        token.setType(CaretToken2.CARET_TOKEN_TYPE)
        token.setOriginalToken(oldToken)
        token
    }
}

