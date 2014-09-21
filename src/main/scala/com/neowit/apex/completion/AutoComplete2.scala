package com.neowit.apex.completion

import java.io.{FileInputStream, File}

import com.neowit.apex.parser.{TreeListener, Member}
import com.neowit.apex.parser.TreeListener.ApexTree
import com.neowit.apex.parser.antlr.{ApexcodeLexer, ApexcodeParser}
import com.neowit.apex.parser.antlr.ApexcodeParser.BlockStatementContext
import org.antlr.v4.runtime.tree.ParseTreeWalker
import org.antlr.v4.runtime._

class AutoComplete2(file: File, line: Int, column: Int, cachedTree: ApexTree = Map[String, Member]()) {
    def listOptions:List[Member] = {
        /*
        val tokens: CommonTokenStream = new CommonTokenStream(getLexer(file)) //DEBUG
        val parser = new ApexcodeParser(tokens)

        parser.setBuildParseTree(true)
        parser.setErrorHandler(new CompletionErrorStrategy())
        */
        getCaretStatement()
        List()

    }

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
    private def getCaretStatement(): List[BlockStatementContext] = {
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

        //parse tree until we reach caret token
        try {
            val tree = parser.compilationUnit()
        } catch {
            case ex: CaretReachedException2 =>
                println("found caret?")
                //println(ex.getToken.getText)
                breakpoint(ex)
            case e:Throwable =>
        }

        List()

    }

    def breakpoint(ex: CaretReachedException2): Unit = {
        //at this point ex contains all information we need to build full statement on which ctrl+space was pressed
        //e.g.: MyClass.MyInnerClass.
        //ex: cls.
        //ex: cls[1].
        val cause = ex.cause
        println(cause.getCtx.getText)
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
                println("token=" + token.toString)
                if (token.getStopIndex + 1 < caretOffset) {
                    //before target token
                    prevToken = token

                } else if (token.getStartIndex > caretOffset) {
                    //token = CaretToken(tokenFactorySourcePair, Token.DEFAULT_CHANNEL, caretOffset, caretOffset)
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

class CaretReachedException2(val finalContext: RuleContext, val cause: RecognitionException)
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
            throw new CaretReachedException2(recognizer.getContext, e)
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

