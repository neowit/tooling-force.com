package com.neowit.apex.completion

import com.neowit.apex.parser.antlr.{ApexcodeBaseListener, ApexcodeParser}
import org.antlr.v4.runtime.ParserRuleContext
import org.antlr.v4.runtime.tree.{TerminalNode, ErrorNode}

class CompletionListener (val parser: ApexcodeParser, val line: Int, val column: Int) extends ApexcodeBaseListener{
    override def enterEveryRule(ctx: ParserRuleContext): Unit = {
        super.enterEveryRule(ctx)
        println("line=" + ctx.getStart.getLine)
        //check if this token belongs to the completion text
        println(ctx.getStop.getText)
        if (ctx.getStart.getLine >= line && ctx.getStop.getLine <= line) {
            println(ctx)
        }

    }

    override def visitTerminal(node: TerminalNode): Unit = {
        super.visitTerminal(node)
        val symbol = node.getSymbol
        //println("symbol=" + node.getSymbol)
        if (symbol.getLine == line && symbol.getCharPositionInLine == (column -1) ) {
            println("symbol=" + node.getSymbol)
        }
    }

    override def visitErrorNode(node: ErrorNode): Unit = {
        super.visitErrorNode(node)
        //println("e.symbol=" + node.getSymbol)
    }
}
