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

package com.neowit.apex

import com.neowit.apex.parser.SoqlParserUtils
import com.neowit.apex.parser.antlr.SoqlLexer
import org.antlr.v4.runtime.{Token, TokenStream, ANTLRInputStream, CommonTokenStream}
import org.scalatest.FunSuite

class SoqlParserUtilsTest extends FunSuite {

    test("Find FROM in outer select (with nested select)") {
        val str =
            """[ select AccountNumber, <caret>,
              |			  (select Id from Accounts__r )
              |				from Account
              |				where CreatedDate = TODAY ]
            """.stripMargin
        assert("Account" == getFromObjectTypes(str).head, "incorrect FROM Type: " + str)
    }

    test("Find FROM in outer select (without nested select) #1") {
        val str =
            """[ select AccountNumber, <caret>
              |				from Account
              |				where CreatedDate = TODAY ]
            """.stripMargin
        assert("Account" == getFromObjectTypes(str).head, "incorrect FROM Type: " + str)
    }

    test("Find FROM in outer select (without nested select) #2") {
        val str =
            """[ select <caret>
              |				from Account where CreatedDate = TODAY ]
            """.stripMargin
        assert("Account" == getFromObjectTypes(str).head, "incorrect FROM Type: " + str)
    }

    test("Find FROM in outer select (without nested select), without WHERE") {
        val str =
            """[ select <caret>
              |				from Account]
            """.stripMargin
        assert("Account" == getFromObjectTypes(str).head, "incorrect FROM Type: " + str)
    }

    test("Find FROM in outer select (without nested select) with incomplete WHERE") {
        val str =
            """[ select <caret>
              |				from Account where CreatedDate  ]
            """.stripMargin
        assert("Account" == getFromObjectTypes(str).head, "incorrect FROM Type: " + str)
    }

    test("Find FROM in nested select ") {
        val str =
            """[ select AccountNumber,
              |			  (select Id, <caret> from Accounts__r )
              |				from Account
              |				where CreatedDate = TODAY ]
            """.stripMargin
        assert("Accounts__r" == getFromObjectTypes(str).head, "incorrect FROM Type: " + str)
    }

    test("Find FROM in nested select in WHERE part") {
        val str =
            """[ select AccountNumber from Account
              |				where CreatedDate = TODAY and (select <caret> from Accounts__r )]
            """.stripMargin
        assert("Accounts__r" == getFromObjectTypes(str).head, "incorrect FROM Type: " + str)
    }

    test("Find FROM in nested select (with multiple nested selects) ") {
        val str =
            """[ select AccountNumber,
              |			  (select Id, Name from Accounts__r ),
              |			 Id,
              |			  (select Name, <caret> from SomeObjects__r )
              |				from Account
              |				where CreatedDate = TODAY ]
            """.stripMargin
        assert("SomeObjects__r" == getFromObjectTypes(str).head, "incorrect FROM Type: " + str)
    }
    /////////////////////////////////////////////////////////////////////////////////////////////////
    private def getFromObjectTypes(soqlStatement: String): List[String] = {
        val (tokens, caretToken) = getTest(soqlStatement)
        val fromToken = SoqlParserUtils.findFromToken(tokens, caretToken)
        assert(fromToken.isDefined, "failed to find FROM Token in: " + soqlStatement)
        List(tokens.get(fromToken.get.getTokenIndex + 1).getText)

    }

    private def getTokens(soqlStatement: String): TokenStream = {
        val input = new ANTLRInputStream(soqlStatement)
        val lexer = new SoqlLexer(input)
        val tokens = new CommonTokenStream(lexer)
        tokens.fill()
        tokens
    }

    private def getTest(soqlStr: String):(TokenStream, Token) = {
        val start = soqlStr.indexOf("<caret>")
        val tokens = getTokens(soqlStr.replace("<caret>", ""))
        //find position of caret
        var i = 0
        while (i < tokens.size()) {
            val token = tokens.get(i)
            if (token.getStartIndex >= start || (token.getStartIndex < start && token.getStopIndex > start)) {
                return (tokens, token)
            }
            i += 1

        }
        assert(false, "failed to detect <caret> token position in :" + soqlStr)
        (tokens, null)
    }
}
