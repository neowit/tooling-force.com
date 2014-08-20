package com.neowit.apex.completion

import org.antlr.v4.runtime._



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
}

class CompletionErrorStrategy extends DefaultErrorStrategy {

    override def reportError(recognizer: Parser, e: RecognitionException) {
        if (e != null && e.getOffendingToken != null &&
            e.getOffendingToken.getType == CaretToken.CARET_TOKEN_TYPE) {
            return
        }
        super.reportError(recognizer, e)
    }




    override def recover(recognizer: Parser, e: RecognitionException) {
        throw new CaretReachedException(recognizer.getContext, e)
    }

}
