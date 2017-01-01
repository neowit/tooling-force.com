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

package com.neowit.apex.parser

import java.io.{FileInputStream, File}

import com.neowit.apex.parser.antlr.{ApexcodeParser, ApexcodeLexer}
import com.neowit.utils.Logging
import org.antlr.v4.runtime.tree.ParseTreeWalker
import org.antlr.v4.runtime.{ParserRuleContext, ANTLRErrorListener, CommonTokenStream}

class SourceScanner (files: List[File]) extends Logging {
    private val completeTree = new ApexTree
    private val fileModificationTimes = Map.newBuilder[String, Long]

    private def getLexer(file: File): ApexcodeLexer = {
        //val input = new ANTLRInputStream(new FileInputStream(file))
        val input = new CaseInsensitiveInputStream(new FileInputStream(file))
        val lexer = new ApexcodeLexer(input)
        lexer
    }

    def scan(completeScan: Boolean = false, errorListener: Option[ANTLRErrorListener] = None): Unit = {
        val lastModifiedByFile = fileModificationTimes.result()
        val walker = new ParseTreeWalker()
        for (file <- files ) {
            val lastModifiedTime = lastModifiedByFile.get(file.getAbsolutePath) match {
              case Some(fTime) => fTime
              case None => -1
            }

            if (completeScan || file.lastModified() > lastModifiedTime) {
                logger.info("scanning file: " + file.getName)

                parseOne(file, errorListener) match {
                    case Some((parser, tree)) =>
                        val extractor = new ApexTreeListener(parser, file.toPath)
                        walker.walk(extractor, tree)
                        completeTree.extend(extractor.getTree)
                        fileModificationTimes.+=(file.getAbsolutePath -> file.lastModified())
                    case _ =>
                }
            }
        }
        //println(getTree)
    }

    def getTree: ApexTree = {
        completeTree
    }

    def parseOne(file: File, errorListener: Option[ANTLRErrorListener] = None): Option[(ApexcodeParser, ParserRuleContext)] =  {
        try {
            val tokens = new CommonTokenStream(getLexer(file))
            val parser = new ApexcodeParser(tokens)
            // add custom error listener if provided
            errorListener.foreach(parser.addErrorListener)
            // do not dump parse errors into console
            ApexParserUtils.removeConsoleErrorListener(parser)
            //parser.setErrorHandler(new BailErrorStrategy())
            //parser.setErrorHandler(new CompletionErrorStrategy())
            val tree = parser.compilationUnit()
            Some(parser, tree)
        } catch {
            case ex:Exception =>
                logger.error("Failed to parse file: " + file.getName, ex)
                println("Failed to parse file: " + file.getName)
                println(ex)
                println(ex.getStackTrace)
                None
        }

    }
}

