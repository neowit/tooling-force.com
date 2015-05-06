package com.neowit.apex.parser

import java.io.File
import java.util.regex.Pattern

import com.neowit.utils.FileUtils
import org.antlr.v4.runtime.tree.ParseTree
import org.antlr.v4.runtime._
import scala.collection.JavaConversions._
import scala.collection.mutable

object ApexParserUtils {

    private val WORD_PATTERN_STR = "^[\\$A-Za-z_][A-Za-z0-9_]*$"
    private val WORD_PATTERN: Pattern = Pattern.compile(WORD_PATTERN_STR)

    def isWordToken(token: Token): Boolean = {
        WORD_PATTERN.matcher(token.getText).matches
    }
    def isDotToken(token: Token): Boolean = {
        "." == token.getText
    }

    def isAtToken(token: Token): Boolean = {
        "@" == token.getText
    }

    def isWordTokenOrDot(token: Token): Boolean = {
        isWordToken(token) || isDotToken(token)
    }

    def isWordTokenOrDotOrAt(token: Token): Boolean = {
        isWordToken(token) || isDotToken(token) || isAtToken(token)
    }

    def isRightParenthesis(token: Token): Boolean = {
        ")" == token.getText
    }

    def isLeftParenthesis(token: Token): Boolean = {
        "(" == token.getText
    }



    def getOffset(file: File, line: Int, startIndex: Int): Int = {
        val text = FileUtils.readFile(file).mkString
        getOffset(text, line, startIndex)
    }

    def getOffset(text: String, line: Int, startIndex: Int): Int = {
        //val bytes = text.take
        var lineNum: Int = 1
        var pos = 0

        while ( lineNum < line && pos < text.length ) {
            val ch = text.charAt(pos)
            if ('\n' == ch) {
                lineNum += 1
            }
            if (lineNum < line) {
                pos = pos + 1
            }
        }
        val offset = pos + startIndex
        offset
    }
    /**
     * in most cases there is no need to dump syntax errors into console
     * @param parser - ApexcodeParser from which to remove console error listener
     */
    def removeConsoleErrorListener(parser: Parser): Unit = {
        parser.getErrorListeners.find(_.isInstanceOf[ConsoleErrorListener]) match {
          case Some(consoleErrorListener) =>
              parser.removeErrorListener(consoleErrorListener)
          case None =>
        }
    }
    /**
     * find children (recursively) satisfying given context type
     */
    def findChildren[T <: ParseTree](ctx: ParseTree, ctxType: Class[T],
                                     filter: ParseTree => Boolean = { _ => true}): List[T] = {
        val foundChildren = getChildren[ParseTree](ctx, filter).filter(_.getClass == ctxType).map(_.asInstanceOf[T])
        if (foundChildren.isEmpty) {
            //descend 1 level
            val res= getChildren[ParseTree](ctx, filter).map(findChildren(_, ctxType, filter)).flatMap(_.toList)
            res
        } else {
            foundChildren.toList
        }
    }
    def findChild[T <: ParseTree](ctx: ParseTree, ctxType: Class[T]): Option[T] = {
        val foundChildren = findChildren(ctx, ctxType)
        if (foundChildren.nonEmpty)
            Some(foundChildren.head)
        else
            None
    }

    def getChildren[T](ctx:ParseTree, filter: ParseTree => Boolean = { _ => true}): List[T] = {
        var children = mutable.ListBuffer[T]()
        var i = 0
        while (i < ctx.getChildCount) {
            val child = ctx.getChild(i)
            if (filter(child)) {
                children += child.asInstanceOf[T]
            }

            i += 1
        }
        children.toList
    }

    def getChild[T](ctx:ParseTree, filter: ParseTree => Boolean = { _ => true}): Option[T] = {
        var i = 0
        while (i < ctx.getChildCount) {
            val child = ctx.getChild(i)
            if (filter(child)) {
                return Some(child.asInstanceOf[T])
            }

            i += 1
        }
        None
    }

    /**
     *
     * @param ctx parent context
     * @param filter - predicate to apply
     * @return index of a direct child which matches specified predicate
     */
    def findChildIndex(ctx: ParseTree, filter: (ParseTree) => Boolean): Int = {
        var i = 0
        while (i < ctx.getChildCount) {
            val child = ctx.getChild(i)
            if (filter(child)) {
                return i
            }

            i += 1
        }
        -1
    }

    def getChild[T](ctx:ParseTree, cls: Class[T]): Option[T] = {
        getChild(ctx, n => n.getClass == cls)
    }

    def getParent[T](ctx:ParseTree, ctxType: Class[T]): Option[T] = {
        def getParentImpl[T](ctx: ParseTree, ctxType: Class[T]): Option[T] = {
            if (null == ctx) {
                None
            } else {
                if (ctx.getClass == ctxType) {
                    Some(ctx.asInstanceOf[T])
                } else {
                    if (null != ctx.getParent) {
                        getParentImpl(ctx.getParent, ctxType)
                    } else {
                        None
                    }
                }
            }
        }
        getParentImpl(ctx.getParent, ctxType)
    }

    /**
     * try to find hidden token of specified type (e.g. javadoc like comment) by checking tokens to the left of current context
     * and making sure that comment token does not end more than 2 lines before ctx
     * @param startToken - start token - we will be moving left from this token
     * @param tokenType - type of hidden token to look for
     * @return Option[found token] or None
     */
    def getNearestHiddenTokenToLeft(startToken: Token, tokenType: Int, tokens: CommonTokenStream): Option[Token] = {
        //
        def getNearestHiddenTokenToLeft(currentToken: Token): Option[Token] = {
            val linesBetween = Math.abs(getNumOfLinesBetween(currentToken, startToken))
            if (linesBetween > 2) {
                None
            } else {
                val index = currentToken.getTokenIndex
                val hiddenTokens = tokens.getHiddenTokensToLeft(index)
                if (null != hiddenTokens && hiddenTokens.nonEmpty && tokenType == hiddenTokens.head.getType) {
                    Some(hiddenTokens.head)
                } else {
                    if (index <= 0) {
                        return None
                    }
                    getNearestHiddenTokenToLeft(tokens.get(index - 1))
                }
            }
        }
        //
        val startIndex = startToken.getTokenIndex
        if (startIndex > 0) {
            getNearestHiddenTokenToLeft(tokens.get(startIndex))
        } else {
            None
        }
    }

    /**
     * count number of lines between the end of prev token and next token
     * @param prev - token that goes before next
     * @param next - token that goes after prev
     */
    def getNumOfLinesBetween(prev: Token, next: Token): Int = {
        if (prev.getTokenIndex != next.getTokenIndex) {
            val prevLineStart = prev.getLine
            val prevLineEnd = if (null == prev.getText) prevLineStart
            else {
                prevLineStart + prev.getText.split("\\n").size - 1
            }
            next.getLine - prevLineEnd
        } else {
            0 //same token
        }
    }

}
