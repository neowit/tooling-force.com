package com.neowit.apex.completion

import com.neowit.apex.parser.ApexParserUtils
import org.antlr.v4.runtime.tree.ParseTree
import org.antlr.v4.runtime.{CommonTokenStream, Token, ParserRuleContext}

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
         * token which goes BEFORE caret
         * @return
         */
        def getStartToken: Token = {
            val ctx = ex.finalContext
            val startToken = ctx.asInstanceOf[ParserRuleContext].getStart //e.g. 'str'

            if (CaretToken2.CARET_TOKEN_TYPE == startToken.getType) {
                val caretToken = startToken.asInstanceOf[CaretTokenTrait]
                val prevToken = caretToken.prevToken
                if (null != prevToken && caretToken.isBeyondCaret) {
                    //val prevTokenEndIndex = prevToken.getCharPositionInLine + (prevToken.getStopIndex - prevToken.getStartIndex) + 1
                    //if (startToken.getCharPositionInLine == prevTokenEndIndex + 1 || caretToken.getLine > caretToken.getCaretLine) {
                    return prevToken
                    //}
                }
            }
            startToken
        }
        val ctx = ex.finalContext
        //val startToken = ctx.asInstanceOf[ParserRuleContext].getStart //e.g. 'str'
        val startToken = getStartToken //e.g. 'str'
        val startTokenIndex = startToken.getTokenIndex //@333 = 333
        //get to caret token and then back to the most likely start of the expression

        val tokenStream = ex.getInputStream
        var index = startTokenIndex
        val streamSize = tokenStream.size()
        var currentToken = tokenStream.get(index)
        while (currentToken.getType != CaretToken2.CARET_TOKEN_TYPE && streamSize > index) {
            index += 1
            currentToken = tokenStream.get(index)
        }
        if (index > streamSize) {
            //failed to get to Caret_Token, use original start
            startToken
        } else {
            //now move back until start of an expression is found
            var newStart = -1
            var expectLeftParenthesis = false
            while (index > startTokenIndex && newStart < 0) {
                index -= 1
                currentToken = tokenStream.get(index)
                if (ApexParserUtils.isRightParenthesis(currentToken)){
                    expectLeftParenthesis = true
                } else if (!ApexParserUtils.isWordTokenOrDot(currentToken) && !expectLeftParenthesis ) {
                    newStart = index + 1
                    currentToken = tokenStream.get(newStart)
                } else if (ApexParserUtils.isLeftParenthesis(currentToken)) {
                    expectLeftParenthesis = false
                }
            }
            currentToken
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
