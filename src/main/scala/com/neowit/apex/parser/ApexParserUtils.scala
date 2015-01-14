package com.neowit.apex.parser

import java.io.File
import java.util.regex.Pattern

import com.neowit.apex.parser.antlr.ApexcodeLexer
import org.antlr.v4.runtime.tree.ParseTree
import org.antlr.v4.runtime.{Token, Parser, ConsoleErrorListener}
import scala.collection.JavaConversions._
import scala.collection.mutable

object ApexParserUtils {

    private val WORD_PATTERN_STR = "^[\\$A-Za-z_][A-Za-z0-9_]*$"
    private val WORD_PATTERN: Pattern = Pattern.compile(WORD_PATTERN_STR)

    def isWordToken(token: Token): Boolean = {
        WORD_PATTERN.matcher(token.getText).matches
    }

    def getOffset(file: File, line: Int, startIndex: Int): Int = {
        val text = scala.io.Source.fromFile(file).mkString
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

    def isWordTokenOrDot(token: Token): Boolean = {
        isWordToken(token) || "." == token.getText
    }

    def isRightParenthesis(token: Token): Boolean = {
        ")" == token.getText
    }

    def isLeftParenthesis(token: Token): Boolean = {
        "(" == token.getText
    }


}
