package com.neowit.apex.completion

import java.io.{FilenameFilter, FileInputStream, File}
import java.util.regex.Pattern

import com.neowit.apex.parser.antlr.ApexcodeParser.VariableDeclaratorContext
import com.neowit.apex.parser.{ClassBodyMember, Member, TreeListener}
import com.neowit.apex.parser.antlr.{ApexcodeParser, ApexcodeLexer}
import org.antlr.v4.runtime._
import org.antlr.v4.runtime.tree.{ParseTree, ParseTreeWalker}

import scala.io.Source


class AutoComplete(file: File, line: Int, column: Int) {

    def listOptions:List[Member] = {
        //find caret position
        val symbol = "cls"
        val caret = new Caret(line-1, column, symbol, file)

        val tokenSource = new CodeCompletionTokenSource(getLexer(file), caret)
        //val tokens: CommonTokenStream = new CommonTokenStream(tokenSource)  //Actual
        val tokens: CommonTokenStream = new CommonTokenStream(getLexer(file)) //DEBUG
        val parser = new ApexcodeParser(tokens)

        parser.setBuildParseTree(true)
        parser.setErrorHandler(new CompletionErrorStrategy())
        //parser.getInterpreter.setPredictionMode(PredictionMode.LL)

        try {
            val tree = parser.compilationUnit()
        } catch {
            case e:CaretReachedException =>
                //find symbol type
                val definition = findSymbolDefinition(caret, e, tokenSource)
                definition match {
                  case Some(tuple) =>
                      return listOptions(tuple, tokenSource.getCursorToken)
                  case None =>
                }
            case e:Throwable =>
        }

        List()
    }
    private def getLexer(file: File): ApexcodeLexer = {
        val input = new ANTLRInputStream(new FileInputStream(file))
        val lexer = new ApexcodeLexer(input)
        lexer
    }

    private def listOptions(caret: Caret): List[Member] = {
        //scan current and other class files in class folder and build tree for all of them

        /* TODO implement CompletionTokenStream to bypass erroneous place,
            i.e. modify stream and insert appropriate token instead of current incomplete one
            this can be done by looking at what tokens are acceptable at the point of error
        val tokens = cursorToken match {
          case Some(token) => new CompletionTokenStream(lexer, token)
          case None => new CommonTokenStream(lexer)
        }
        */

        //extractor.dump()
        //check if we have enough in the current file to list completion options
        println("Potential LOCAL signatures:")
        listOptions(caret, file, Some(new CompletionIgnoreErrorStrategy())) match {
          case Some(members) =>
              return members.toList
          case None =>
              //scan other available files
              println("\ntry to find definitions in other project files")
              val classFiles = file.getParentFile.listFiles(new FilenameFilter(){
                  override def accept(parentDir: File, name: String): Boolean = name.endsWith(".cls")
              })
              for (currentFile <- classFiles ) {
                  listOptions(caret, currentFile) match {
                    case Some(members) =>
                        return members.toList
                    case None =>
                  }
              }
        }

        List()
    }

    def listOptions(caret: Caret, file: File, errorStrategy: Option[ANTLRErrorStrategy] = None): Option[List[Member]] = {
        val lexer = getLexer(file)
        val tokens = new CommonTokenStream(lexer)
        val parser = new ApexcodeParser(tokens)
        errorStrategy match {
          case Some(completionErrorStrategy) =>
              //parser.setErrorHandler(new BailErrorStrategy())
              //parser.setErrorHandler(new CompletionErrorStrategy())
              parser.setErrorHandler(completionErrorStrategy)
          case None =>
        }
        val tree = parser.compilationUnit()
        val walker = new ParseTreeWalker()
        val extractor = new TreeListener(parser)
        walker.walk(extractor, tree)
        //has definition
        extractor.tree.get(caret.symbol) match {
            case Some(member) =>
                val members = member.children
                println("Potential signatures:")
                println(members.map(_.getSignature).mkString("\n"))
                Some(members.toList)
            case None =>
                None
        }

    }


    // try to find definition using parse tree
    def findSymbolDefinition(caret: Caret, caretException: CaretReachedException, tokenSource: CodeCompletionTokenSource ): Option[(Token, String)] = {
        //def filter(ctx: ParseTree): Boolean = ctx.getText == caret.symbol
        //ClassBodyMember.findChild(caretException.finalContext, classOf[VariableDeclaratorContext], filter) TODO
        None

    }
}

object CompletionUtils {

    private val WORD_PATTERN_STR = "^[\\$A-Za-z_][A-Za-z0-9_]*$"
    private val WORD_PATTERN: Pattern = Pattern.compile(WORD_PATTERN_STR)

    def isWordToken(token: Token): Boolean = {
        WORD_PATTERN.matcher(token.getText).matches
    }

    def getOffset(file: File, line: Int, startIndex: Int): Int = {
        val text = scala.io.Source.fromFile(file).mkString
        //val bytes = text.take
        var lineNum: Int = 0
        var pos = 0
        while ( lineNum < line && pos < text.length ) {
            val ch = text(pos)
            if ('\n' == ch) {
                lineNum += 1
            }
            pos = pos + 1
        }
        val offset = pos + startIndex
        offset
    }
}

object CaretToken {
    final val CARET_TOKEN_TYPE: Int = -2
    def apply(source: org.antlr.v4.runtime.misc.Pair[TokenSource, CharStream], channel: Int, start: Int, stop: Int) =
        new org.antlr.v4.runtime.CommonToken(source, CaretToken.CARET_TOKEN_TYPE, channel, start, stop) with CaretTokenTrait

    def apply(oldToken: Token) = {
        val token = new org.antlr.v4.runtime.CommonToken(oldToken) with CaretTokenTrait
        token.setType(CaretToken.CARET_TOKEN_TYPE)
        token.setOriginalToken(oldToken)
        token
    }
}

trait CaretTokenTrait extends org.antlr.v4.runtime.CommonToken {

    private var originalToken: Token = null

    def setOriginalToken(token: Token) {
        originalToken = token
    }
    def getOriginalToken: Token = {
        originalToken
    }

    def getOriginalType: Int = {
        originalToken.getType
    }

}


class Caret(val line:  Int, val startIndex: Int, val symbol:String, file: File) {
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

    private val stack= scala.collection.mutable.Stack[Token]()
    var lastPotentialDefinitionPosition = -1

    def getTokens: Array[Token] = {
        stack.toArray.reverse
    }

    /**
     * @return original token which was is replaced by CaretToken
     */
    def getCursorToken:Option[Token] = {
        if (null != caretToken && null != caretToken.asInstanceOf[CaretTokenTrait].getOriginalToken) {
            Some(caretToken.asInstanceOf[CaretTokenTrait].getOriginalToken)
        } else {
            None
        }
    }

    def nextToken: Token = {
        if (caretToken == null) {
            var token: Token = source.nextToken

            println("token=" + token.toString)
            if (token.getStopIndex + 1 < caretOffset) {
                stack.push(token)
                if (caret.symbol == token.getText) {
                    lastPotentialDefinitionPosition = stack.length - 1
                }
            }
            else if (token.getStartIndex > caretOffset) {
                token = CaretToken(tokenFactorySourcePair, Token.DEFAULT_CHANNEL, caretOffset, caretOffset)
                caretToken = token
            }
            else {
                if (token.getStopIndex + 1 == caretOffset && token.getStopIndex >= token.getStartIndex) {
                    if (!isWordToken(token)) {
                        return token
                    }
                }
                token = CaretToken(token)
                caretToken = token
            }
            return token
        }
        throw new UnsupportedOperationException("Attempted to look past the caret.")
    }

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

    def getTokenFactory: TokenFactory[_] = {
        tokenFactory
    }

    def setTokenFactory(tokenFactory: TokenFactory[_]) {
        source.setTokenFactory(tokenFactory)
        this.tokenFactory = if (tokenFactory != null) tokenFactory else CommonTokenFactory.DEFAULT
    }

    protected def isWordToken(token: Token): Boolean = {
        CompletionUtils.isWordToken(token)
    }
}

/**
 * CompletionTokenStream allows to remove incomplete token under cursor to limit the possibility of erroneous input
 * @param tokenSource
 */
class CompletionTokenStream(tokenSource: TokenSource, offendingToken: Token) extends CommonTokenStream(tokenSource) {
    override def nextTokenOnChannel(i: Int, channel: Int): Int = {
        var index = i
        val token = get(super.nextTokenOnChannel(index, channel))
        if (token.getStartIndex == offendingToken.getStartIndex) {
            index = index + 1
            if (get(super.nextTokenOnChannel(index, channel)).getText == ".") {
                index = index + 1
                if (CompletionUtils.isWordToken(get(super.nextTokenOnChannel(index, channel)))) {
                    index = index + 1
                }
            }
        }
        super.nextTokenOnChannel(index, channel)
    }
}