package com.neowit.apex.parser

import java.io.{FileInputStream, File}

import com.neowit.apex.parser.TreeListener.ApexTree
import com.neowit.apex.parser.antlr.{ApexcodeParser, ApexcodeLexer}
import org.antlr.v4.runtime.tree.ParseTreeWalker
import org.antlr.v4.runtime.{CommonTokenStream, ANTLRInputStream}

class SourceScanner (files: List[File]) {
    val completeTree = Map.newBuilder[String, Member]

    private def getLexer(file: File): ApexcodeLexer = {
        val input = new ANTLRInputStream(new FileInputStream(file))
        val lexer = new ApexcodeLexer(input)
        lexer
    }

    def scan(): Unit = {
        val walker = new ParseTreeWalker()
        for (file <- files ) {
            val tokens = new CommonTokenStream(getLexer(file))
            val parser = new ApexcodeParser(tokens)
            //parser.setErrorHandler(new BailErrorStrategy())
            //parser.setErrorHandler(new CompletionErrorStrategy())
            val tree = parser.compilationUnit()
            val extractor = new TreeListener(parser)
            walker.walk(extractor, tree)
            completeTree.++= (extractor.getTree)
        }
        println(getTree) //TODO remove
    }

    def getTree: ApexTree = {
        completeTree.result()
    }
}

