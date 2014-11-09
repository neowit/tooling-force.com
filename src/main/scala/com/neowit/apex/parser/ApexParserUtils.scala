package com.neowit.apex.parser

import com.neowit.apex.parser.antlr.ApexcodeParser
import org.antlr.v4.runtime.ConsoleErrorListener
import scala.collection.JavaConversions._

object ApexParserUtils {
    /**
     * in most cases there is no need to dump syntax errors into console
     * @param parser - ApexcodeParser from which to remove console error listener
     */
    def removeConsoleErrorListener(parser: ApexcodeParser): Unit = {
        parser.getErrorListeners.find(_.isInstanceOf[ConsoleErrorListener]) match {
          case Some(consoleErrorListener) =>
              parser.removeErrorListener(consoleErrorListener)
          case None =>
        }
    }

}
