package com.neowit.apex.parser

import java.io.File
import java.util.regex.Pattern

import org.antlr.v4.runtime.{Token, Parser, ConsoleErrorListener}
import scala.collection.JavaConversions._

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

}
