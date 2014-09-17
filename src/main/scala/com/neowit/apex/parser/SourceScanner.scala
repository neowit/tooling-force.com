package com.neowit.apex.parser

import java.io.{FileInputStream, File}

import com.neowit.apex.parser.TreeListener.ApexTree
import com.neowit.apex.parser.antlr.{ApexcodeParser, ApexcodeLexer}
import com.neowit.utils.Logging
import org.antlr.v4.runtime.tree.ParseTreeWalker
import org.antlr.v4.runtime.{CommonTokenStream, ANTLRInputStream}

class SourceScanner (files: List[File]) extends Logging {
    val completeTree = Map.newBuilder[String, Member]
    val fileModificationTimes = Map.newBuilder[String, Long]

    private def getLexer(file: File): ApexcodeLexer = {
        val input = new ANTLRInputStream(new FileInputStream(file))
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

                val tokens = new CommonTokenStream(getLexer(file))
                val parser = new ApexcodeParser(tokens)
                //parser.setErrorHandler(new BailErrorStrategy())
                //parser.setErrorHandler(new CompletionErrorStrategy())
                val tree = parser.compilationUnit()
                val extractor = new TreeListener(parser)
                walker.walk(extractor, tree)
                completeTree.++=(extractor.getTree)
                fileModificationTimes.+=(file.getAbsolutePath -> file.lastModified())
            }
        }
        //println(getTree)
    }

    def getTree: ApexTree = {
        completeTree.result()
    }
}

