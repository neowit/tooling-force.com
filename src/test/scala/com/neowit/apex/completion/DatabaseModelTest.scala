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

package com.neowit.apex.completion

import org.scalatest.funsuite.AnyFunSuite

/**
  * Author: Andrey Gavrikov (westbrook)
  * Date: 25/09/2016
  */
class DatabaseModelTest extends AnyFunSuite {

    test("Namespace Detection (absent)") {
        assertResult(0)(DatabaseModel.hasNamespacePrefix("de"))
        assertResult(0)(DatabaseModel.hasNamespacePrefix("de__c"))
        assertResult(0)(DatabaseModel.hasNamespacePrefix("de_ef"))
        assertResult(0)(DatabaseModel.hasNamespacePrefix("de_ef__c"))
    }

    test("Namespace Detection (present)") {
        assertResult(1)(DatabaseModel.hasNamespacePrefix("abc__de"))
        assertResult(1)(DatabaseModel.hasNamespacePrefix("a__de"))
        assertResult(1)(DatabaseModel.hasNamespacePrefix("abc__de__c"))
        assertResult(1)(DatabaseModel.hasNamespacePrefix("abc__de_ef"))
        assertResult(1)(DatabaseModel.hasNamespacePrefix("abc__de_ef__c"))
    }
    test("Namespace Extraction (namespace present)") {
        assertResult(Some("abc"))(DatabaseModel.getNamespacePrefix("abc__de"))
        assertResult(Some("a"))(DatabaseModel.getNamespacePrefix("a__de"))
        assertResult(Some("abc"))(DatabaseModel.getNamespacePrefix("abc__de__c"))
        assertResult(Some("abc"))(DatabaseModel.getNamespacePrefix("abc__de_ef"))
        assertResult(Some("abc"))(DatabaseModel.getNamespacePrefix("abc__de_ef__c"))
    }

    test("Namespace Extraction (namespace absent)") {
        assertResult(None)(DatabaseModel.getNamespacePrefix("de"))
        assertResult(None)(DatabaseModel.getNamespacePrefix("de__c"))
        assertResult(None)(DatabaseModel.getNamespacePrefix("de_ef"))
        assertResult(None)(DatabaseModel.getNamespacePrefix("de_ef__c"))
    }

    test("Sort: Namespace last") {
        val collection = Array[String] ("de__ef__c", "de", "ef__gh__c", "de_ef", "de_ef__c", "ef__c", "ef")
        def nameExtractor(s: String): String = s
        assertResult(Array("de", "de_ef", "de_ef__c", "ef__c", "ef", "de__ef__c", "ef__gh__c"))(DatabaseModel.sortNamespaceLast(collection, nameExtractor))
    }
}
