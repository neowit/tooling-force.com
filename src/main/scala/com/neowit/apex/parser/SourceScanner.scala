package com.neowit.apex.parser

import java.io.{FileInputStream, File}

import com.neowit.apex.parser.antlr.{ApexcodeParser, ApexcodeLexer}
import com.neowit.utils.Logging
import org.antlr.v4.runtime.tree.ParseTreeWalker
import org.antlr.v4.runtime.CommonTokenStream

class SourceScanner (files: List[File]) extends Logging {
    val completeTree = new ApexTree
    val fileModificationTimes = Map.newBuilder[String, Long]

    private def getLexer(file: File): ApexcodeLexer = {
        //val input = new ANTLRInputStream(new FileInputStream(file))
        val input = new CaseInsensitiveInputStream(new FileInputStream(file))
        val lexer = new ApexcodeLexer(input)
        lexer
    }

    def scan(completeScan: Boolean = false): Unit = {
        val lastModifiedByFile = fileModificationTimes.result()
        val walker = new ParseTreeWalker()
        for (file <- files ) {
            val lastModifiedTime = lastModifiedByFile.get(file.getAbsolutePath) match {
              case Some(fTime) => fTime
              case None => -1
            }

            if (completeScan || file.lastModified() > lastModifiedTime) {
                logger.debug("scanning file: " + file.getName)

                try {
                    val tokens = new CommonTokenStream(getLexer(file))
                    val parser = new ApexcodeParser(tokens)
                    ApexParserUtils.removeConsoleErrorListener(parser)
                    //parser.setErrorHandler(new BailErrorStrategy())
                    //parser.setErrorHandler(new CompletionErrorStrategy())
                    val tree = parser.compilationUnit()
                    val extractor = new ApexTreeListener(parser)
                    walker.walk(extractor, tree)
                    completeTree.extend(extractor.getTree)
                    fileModificationTimes.+=(file.getAbsolutePath -> file.lastModified())
                } catch {
                    case ex:Exception =>
                        logger.error("Failed to parse file: " + file.getName, ex)
                        logger.error(ex.getStackTrace)
                        println("Failed to parse file: " + file.getName)
                        println(ex)
                        println(ex.getStackTrace)
                }
            }
        }
        //println(getTree)
    }

    def getTree: ApexTree = {
        completeTree
    }
}

