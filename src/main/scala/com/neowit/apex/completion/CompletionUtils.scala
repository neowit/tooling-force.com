package com.neowit.apex.completion

import com.neowit.apex.parser.ApexParserUtils
import org.antlr.v4.runtime.tree.ParseTree
import org.antlr.v4.runtime.{TokenStream, CommonTokenStream, Token, ParserRuleContext}

object AToken {
    val SOQL_Pattern = "(?i)\\[\\s*select\\s+".r
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
        var symbol = if (ApexParserUtils.isWordToken(startToken) ) startToken.getText else ""
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
                    if (token.getStartIndex <= caretOffset) {
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

            if (ApexParserUtils.isWordTokenOrDot(resultToken)) {
                resultToken
            } else {
                startToken
            }
        }

        //
        val startToken = getTokenBeforeCaret
        if (!ApexParserUtils.isWordTokenOrDot(startToken)) {
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

        while (index > 0 && newStart < 0) {
            if (ApexParserUtils.isWordTokenOrDot(currentToken)) {
                index -= 1
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
}
