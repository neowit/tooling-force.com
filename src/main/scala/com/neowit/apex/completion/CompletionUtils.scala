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

package com.neowit.apex.completion

import java.io.{File, FileInputStream}

import com.neowit.apex.parser.{ApexParserUtils, CaseInsensitiveInputStream}
import com.neowit.apex.parser.antlr.{ApexcodeLexer, ApexcodeParser}
import org.antlr.v4.runtime.tree.ParseTree
import org.antlr.v4.runtime.{CommonTokenStream, ParserRuleContext, Token, TokenStream}

import scala.util.matching.Regex

object AToken {
    val SOQL_Pattern: Regex = "(?i)\\[\\s*select\\s+".r
}

/**
 *
 * @example
     * AToken represents a symbol which type we need to resolve<br/>
 * consider expression:<br/>
 *  String.isEmpty().< caret >
 *
 * @param index - position in expression, e.g. 0 for "String" above
 * @param symbol - for second token in example above it would be "isEmpty"
 * @param expression - for second token in example above it would be "isEmpty()"
 * @param token - parse tree token containing current symbol
 * @param finalContext - parse tree context calculated by Apex parser for the expression in < caret > position
 */
case class AToken(index: Int, symbol: String, expression: String, token: Option[Token], finalContext: ParseTree) {

    def isSoql: Boolean = token match {
        case Some(x) =>
            AToken.SOQL_Pattern.findFirstIn(x.getText).isDefined
        case None => false
    }
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

object CompletionUtils {

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
    def breakExpressionToATokens(ex: CaretReachedException): List[AToken] = {

        val expressionTokens = List.newBuilder[AToken]
        //at this point ex contains all information we need to build full statement on which ctrl+space was pressed
        //e.g.: MyClass.MyInnerClass.
        //ex: cls.
        //ex: cls[1].
        //val cause = ex.cause
        val ctx = ex.finalContext
        //val parser = ex.recognizer

        //val startToken = ctx.asInstanceOf[ParserRuleContext].getStart //e.g. 'str'
        val startToken = findStartToken(ex) //e.g. 'str'
        val startTokenIndex = startToken.getTokenIndex //@333 = 333
        //val startTokenText = startToken.getText //e.g. 'str'
        //val tokenStream = cause.getInputStream.asInstanceOf[CommonTokenStream]
        val tokenStream = ex.getInputStream

        var index = startTokenIndex
        var currentToken = startToken
        var symbol = if (ApexParserUtils.isWordToken(startToken) || ApexParserUtils.isAtToken(startToken) ) startToken.getText else ""
        var expression = symbol
        var token:Option[Token] = Some(currentToken)

        while (currentToken.getType != CaretToken2.CARET_TOKEN_TYPE) {

            val newIndex = currentToken.getText match {
                case "(" =>
                    val i = consumeUntil(tokenStream, index + 1, ")")
                    if (i > (index + 1)) {
                        expression = symbol + "()"
                    }
                    i
                case "[" =>
                    val i = consumeUntil(tokenStream, index + 1, "]")
                    if (i > (index + 1)) {
                        expression = symbol + "[]"
                    }
                    i
                case "." => //end of expression
                    expressionTokens.+=(new AToken(index - 1, symbol, expression, token, ctx))
                    symbol = ""
                    expression = ""
                    token = None
                    //index + 1
                    index
                case "@" => //annotation
                    expressionTokens.+=(new AToken(index, symbol, expression, token, ctx))
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
            if (tokenStream.get(index).getType != CaretToken2.CARET_TOKEN_TYPE) {
                index += 1
                currentToken = tokenStream.get(index)
                if (symbol.isEmpty && "\\w".r.findFirstIn(currentToken.getText).isDefined) {
                    symbol = currentToken.getText
                    expression = symbol
                    token = Some(currentToken)
                }
            } else {
                currentToken = tokenStream.get(index)
            }
        }
        expressionTokens.+=(new AToken(index - 1, symbol, expression, token, ctx))

        //get all tokens till caret
        //val allTokens = tokenStream.get(startIndex, endTokenIndex)

        //println(cause.getCtx.getText)
        expressionTokens.result()
    }

    /**
     * sometimes ex.finalContext is too broad, and we may need to narrow down list of tokens
     * to generate expression which is being completed
     * @param ex - CaretReachedException
     * @return
     */
    private def findStartToken(ex: CaretReachedException): Token = {
        /**
         * caret token often gets associated with the NEXT token after Caret instead of Previous one
         * this method is trying to identify if caret token has gone too far and if we need to use the
         * token which goes BEFORE caret, if that token is a word or "."
         * @return
         */
        def getTokenBeforeCaret: Token = {
            val startToken = ex.caretToken.asInstanceOf[Token] //e.g. 'str'
            var resultToken = startToken
            val caretOffset = ex.caretToken.asInstanceOf[CaretTokenTrait].getCaret match {
              case Some(caret) => caret.getOffset
              case None =>
                        return ex.finalContext.asInstanceOf[ParserRuleContext].getStart
            }
            val tokenStream = ex.getInputStream
            //identify direction
            val maxIndex = tokenStream.size()
            var index = startToken.getTokenIndex
            var tokenIndex = -1
            if (startToken.getStartIndex >= caretOffset) {
                //right to left
                while ((index -1) >0 && tokenIndex < 0) {
                    index -= 1
                    val token = tokenStream.get(index)
                    if (token.getStopIndex +1 == caretOffset) {
                        tokenIndex = token.getTokenIndex
                        resultToken = tokenStream.get(index)
                    }
                }
            } else if (startToken.getStartIndex < caretOffset) {
                //left to right
                while ( (index+1) < maxIndex && tokenIndex < 0) {
                    index += 1
                    val token = tokenStream.get(index)
                    if (token.getStopIndex +1 >= caretOffset) {
                        resultToken = tokenStream.get(index)
                        tokenIndex = token.getTokenIndex
                    }
                }
            }

            if (ApexParserUtils.isWordTokenOrDotOrAt(resultToken)) {
                resultToken
            } else {
                startToken
            }
        }

        //
        val startToken = getTokenBeforeCaret
        if (!ApexParserUtils.isWordTokenOrDotOrAt(startToken)) {
            //if token in caret position is not word token or "." then no point to continue finding "start" of expression
            return startToken
        }

        //starting caretTokenIndex move back to the most likely start of the expression
        var index = startToken.getTokenIndex
        //now move back until start of an expression is found
        var newStart = -1
        val tokenStream = ex.getInputStream
        val streamSize = tokenStream.size()
        var currentToken = tokenStream.get(index)
        var prevToken: Option[Token] = None

        while (index > 0 && newStart < 0) {
            if (ApexParserUtils.isWordTokenOrDotOrAt(currentToken)) {
                if (isValidChain(prevToken, currentToken)) {
                    prevToken = Some(currentToken)
                    index -= 1
                } else {
                    newStart = prevToken match {
                      case Some(_prevToken) => _prevToken.getTokenIndex
                      case None => startToken.getTokenIndex
                    }
                }
            } else if (ApexParserUtils.isRightParenthesis(currentToken)) {
                index = getBalancedLeftParenthesisTokenIndex(currentToken, tokenStream) - 1
            } else {
                newStart = index + 1
            }
            if (index >=0 ) {
                currentToken = tokenStream.get(index)
            }
        }
        if (newStart >=0 && newStart < streamSize) {
            currentToken = tokenStream.get(newStart)
        }
        currentToken
    }

    /**
     * make sure that tokens chain always looks like one of these
     *  'word' - where prevToken is None, "word" is nextToken, i.e. right-to-left
     *  '@' - where prevToken is "word", "@" is nextToken , i.e. right-to-left
     *  'word.' - where "." is prevToken, "word" is nextToken, i.e. right-to-left
     *  'word.word'
     *  'word.word.'
     *  etc
     * but never looks like so
     *  word _space_ word
     */
    private def isValidChain(prevToken: Option[Token], nextToken: Token): Boolean = {
        if (ApexParserUtils.isWordToken(nextToken)) {
            //previous token must be nothing or "." or "@"
            prevToken.isEmpty || ApexParserUtils.isDotToken(prevToken.get)
        } else if (ApexParserUtils.isDotToken(nextToken)) {
            prevToken.isEmpty || ApexParserUtils.isWordToken(prevToken.get)
        } else if (ApexParserUtils.isAtToken(nextToken)) {
            prevToken.isEmpty || ApexParserUtils.isWordToken(prevToken.get)
        } else {
            false
        }
    }

    /**
     * starting given token, which must be ")", move right-to-left and find next balanced "("
     * @param scopeEnd - token which contains start of the scope token, i.e. ")"
     * @param tokenStream - tokens
     * @return index of first balanced "(" token
     */
    private def getBalancedLeftParenthesisTokenIndex(scopeEnd: Token, tokenStream: TokenStream): Int = {
        if (!ApexParserUtils.isRightParenthesis(scopeEnd)) {
            scopeEnd.getTokenIndex
        } else {
            val brackets = new scala.collection.mutable.Stack[String]()
            var index = scopeEnd.getTokenIndex
            while (index > 0) {
                index -= 1
                val token = tokenStream.get(index)
                if (ApexParserUtils.isRightParenthesis(token)) {
                    brackets.push(")")
                } else if (ApexParserUtils.isLeftParenthesis(token)) {
                    if (brackets.isEmpty) {
                        return index
                    } else {
                        brackets.pop()
                    }
                }
            }
            index
        }
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
      *
      * @param lexerFun - use it provide alternative ApexcodeLexer
      * @return
      */
    def getCaretStatement(file: File, line: Int, column: Int, lexerFun: File => ApexcodeLexer = getDefaultLexer): (List[AToken], Option[CaretReachedException]) = {
        val caret = new CaretInFile(line, column, file)
        val tokenSource = new CodeCompletionTokenSource(lexerFun(file), caret)
        val tokens: CommonTokenStream = new CommonTokenStream(tokenSource)  //Actual
        //val tokens: CommonTokenStream = new CommonTokenStream(getLexer(file))
        val parser = new ApexcodeParser(tokens)
        ApexParserUtils.removeConsoleErrorListener(parser)
        parser.setBuildParseTree(true)
        parser.setErrorHandler(new CompletionErrorStrategy())

        //parse tree until we reach caret caretAToken
        try {
            parser.compilationUnit()
        } catch {
            case ex: CaretReachedException =>
                //println("found caret?")
                //println(ex.getToken.getText)
                //listOptions(ex)
                return (CompletionUtils.breakExpressionToATokens(ex), Some(ex))
            case e:Throwable =>
                println(e.getMessage)
        }

        (List(), None)
    }

    /**
      * default case insensitive ApexCode lexer
      * @param file - file to parse
      * @return case insensitive ApexcodeLexer
      */
    def getDefaultLexer(file: File): ApexcodeLexer = {
        //val input = new ANTLRInputStream(new FileInputStream(file))
        val input = new CaseInsensitiveInputStream(new FileInputStream(file))
        val lexer = new ApexcodeLexer(input)
        lexer
    }
}
