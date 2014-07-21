package com.neowit.apex.completion

import org.antlr.v4.runtime._


class CompletionErrorStrategy extends DefaultErrorStrategy {

    override def reportError(recognizer: Parser, e: RecognitionException) {
        if (e != null && e.getOffendingToken != null &&
            e.getOffendingToken.getType == CaretToken.CARET_TOKEN_TYPE) {
            return
        }
        super.reportError(recognizer, e)
    }



    override def recover(recognizer: Parser, e: RecognitionException) {
        /*
        if (recognizer.isInstanceOf[CodeCompletionParser] &&
            recognizer.asInstanceOf[CodeCompletionParser].getInterpreter.getCaretTransitions != null) {
            val parser = recognizer.asInstanceOf[CodeCompletionParser]
            val token = parser.getInterpreter.getCaretToken
            val interpreter = parser.getInterpreter
            throw new CaretReachedException(parser.getContext, token, interpreter.getCaretTransitions, e)
        }
        throw e
        */
        throw new CaretReachedException(recognizer.getContext, e)
    }

}

class CaretReachedException(val finalContext: RuleContext, val cause: RecognitionException)
        extends RuntimeException(cause) {

}

class CompletionIgnoreErrorStrategy extends DefaultErrorStrategy {

    override def reportError(recognizer: Parser, e: RecognitionException) {
        if (e != null && e.getOffendingToken != null &&
            e.getOffendingToken.getType == CaretToken.CARET_TOKEN_TYPE) {
            return
        }
        //super.reportError(recognizer, e)
    }


    /*
    override def recover(recognizer: Parser, e: RecognitionException) {
        super.recover(recognizer, e)
    }
    */

}
