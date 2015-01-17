package com.neowit.apex.parser

import com.neowit.apex.parser.antlr.SoqlLexer
import org.antlr.v4.runtime.{Token, TokenStream}
import scala.collection.mutable

object SoqlParserUtils {

    private abstract class SearchDescriptor(val increment: Int, val scopeOpenSymbol: String, val scopeCloseSymbol: String) {
        def canContinue(tokenIndex: Int): Boolean
    }
    private class SearchLeft extends SearchDescriptor(-1, ")", "(") {
        override def canContinue(tokenIndex: Int): Boolean = tokenIndex >= 0
    }
    private class SearchRight(tokenStreamSize: Int) extends SearchDescriptor(+1, "(", ")") {
        override def canContinue(tokenIndex: Int): Boolean = tokenIndex < tokenStreamSize
    }


    private def findTokenInScope(tokens: TokenStream, startToken: Token, tokenType: Int, searchDescriptor: SearchDescriptor): Option[Token] = {
        var tokenIndex = startToken.getTokenIndex

        val brackets = mutable.Stack[String]()
        while (searchDescriptor.canContinue(tokenIndex)) {
            val token = tokens.get(tokenIndex)

            if (searchDescriptor.scopeCloseSymbol == token.getText) {
                if (brackets.nonEmpty && searchDescriptor.scopeOpenSymbol == brackets.top) {
                    brackets.pop()
                }
            } else if (searchDescriptor.scopeOpenSymbol == token.getText) {
                brackets.push(token.getText)
            } else if (tokenType == token.getType && brackets.isEmpty) {
                return Some(token)
            }
            tokenIndex += searchDescriptor.increment
        }
        None

    }

    /**
     * beginning startToken move left until find SELECT token which belongs to the same scope as  startToken
     * @example
     *          [ select Id, (select Name from Contacts), _startToken_ from Account ]
     *          return first "select"
     * @example
     *          [ select Id, (select Name, _start_token_ from Contacts) from Account ]
     *          return second "select", the one from Contacts subquery
     * @param tokens -token stream
     * @param startToken - move left starting this token in stream
     * @return
     */
    def findSelectToken(tokens: TokenStream, startToken: Token): Option[Token] = {
        findTokenInScope(tokens, startToken, SoqlLexer.SELECT, new SearchLeft)
    }

    /**
     *
     * using startToken as a starting point to detect scope - find FROM token which belongs to the same scope as startToken
     * @example
     *          [ select Id, (select Name from Contacts), _startToken_ from Account ]
     *          return Account
     * @example
     *          [ select Id, (select Name, _start_token_ from Contacts) from Account ]
     *          return Contacts
     * @param tokens -token stream
     * @param startToken - token which points to the relevant scope
     * @return
     */
    def findFromToken(tokens: TokenStream, startToken: Token): Option[Token] = {
        findSelectToken(tokens, startToken)  match {
            case Some(selectToken) =>
                val maxTokenIndex = tokens.size()
                findTokenInScope(tokens, selectToken, SoqlLexer.FROM, new SearchRight(maxTokenIndex))
            case None => None
        }
    }


}
