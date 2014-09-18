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
        parser.setErrorHandler(new CompletionErrorStrategy())
        //val tree = parser.compilationUnit()
        //val walker = new ParseTreeWalker()
        //val extractor = new TreeListener(parser)
        //walker.walk(extractor, tree)

        //parse tree until we reach caret token
        try {
            val tree = parser.compilationUnit()
        } catch {
            case ex: CaretReachedException =>
            case e:Throwable =>
        }

        List()

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

        private val caretOffset = caret.getOffset

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
            var token: Token = source.nextToken
            println("token=" + token.toString)
            if (token.getStopIndex + 1 < caretOffset) {
                //before target token

            } else if (token.getStartIndex > caretOffset) {
                //token = CaretToken(tokenFactorySourcePair, Token.DEFAULT_CHANNEL, caretOffset, caretOffset)
                //found target?
                caretToken = token
                throw new CaretReachedException(token)
            }
            else {
                throw new IllegalAccessError("Why are we here?")//TODO find out when we can get here
            }

            token
        }
        def getTokenFactory: TokenFactory[_] = {
            tokenFactory
        }

        def setTokenFactory(tokenFactory: TokenFactory[_]) {
            source.setTokenFactory(tokenFactory)
            this.tokenFactory = if (tokenFactory != null) tokenFactory else CommonTokenFactory.DEFAULT
        }
    }

    class CaretReachedException(token: Token) extends RuntimeException
}


