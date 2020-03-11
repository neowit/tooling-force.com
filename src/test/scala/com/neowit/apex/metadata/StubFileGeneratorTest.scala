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

package com.neowit.apex.metadata

import com.neowit.apex.StubFileGenerator
import org.scalatest.funsuite.AnyFunSuite

class StubFileGeneratorTest extends AnyFunSuite{

    test("Object type name in Trigger detection 1") {
        val text = """trigger MyTrigger on Task ( before insert, after update) {} """
        val objectTypeName = StubFileGenerator.getTriggerObjectTypeName("MyTrigger", text).getOrElse("")
                //check that resulting file contains a dummy version and object name was inserted
        assertResult("Task")(objectTypeName)
    }

    test("Object type name in Trigger detection 2 - with blank line") {
        val text =
            """
              |trigger MyTrigger on Task ( before insert, after update) {}
              |""".stripMargin
        val objectTypeName = StubFileGenerator.getTriggerObjectTypeName("MyTrigger", text).getOrElse("")
        //check that resulting file contains a dummy version and object name was inserted
        assertResult("Task")(objectTypeName)
    }

    test("Object type name in Trigger detection 3 - with comments") {
        val text =
            """
              |// some comments before trigger body
              |trigger MyTrigger on Task (before insert, after update) {}
              |""".stripMargin
        val objectTypeName = StubFileGenerator.getTriggerObjectTypeName("MyTrigger", text).getOrElse("")
        //check that resulting file contains a dummy version and object name was inserted
        assertResult("Task")(objectTypeName)
    }

    test("Object type name in Trigger detection 4 - with long comments") {
        val text =
            """
              |/**
              | * some comments before trigger body
              | */
              |trigger MyTrigger on Task__c(before insert, after update) {}
              |""".stripMargin
        val objectTypeName = StubFileGenerator.getTriggerObjectTypeName("MyTrigger", text).getOrElse("")
        //check that resulting file contains a dummy version and object name was inserted
        assertResult("Task__c")(objectTypeName)
    }

    test("Object type name in Trigger detection 5 - with long trigger body") {
        val text =
            """
              |/**
              | * some comments before trigger body
              | */
              |trigger MyTrigger on Task    (after undelete, after update,
              |                             after delete, before insert) {
              | System.debug('');
              |}
              |""".stripMargin
        val objectTypeName = StubFileGenerator.getTriggerObjectTypeName("MyTrigger", text).getOrElse("")
        //check that resulting file contains a dummy version and object name was inserted
        assertResult("Task")(objectTypeName)
    }

    test("Object type name in Trigger detection 6 - with multi-line definition") {
        val text =
            """
              |/**
              | * some comments before trigger body
              | */
              |trigger MyTrigger
              | on Task
              | (after undelete, after update,
              |                             after delete, before insert) {
              | System.debug('');
              |}
              |""".stripMargin
        val objectTypeName = StubFileGenerator.getTriggerObjectTypeName("MyTrigger", text).getOrElse("")
        //check that resulting file contains a dummy version and object name was inserted
        assertResult("Task")(objectTypeName)
    }

    test("Object type name in Trigger detection 7 - single line body") {
        val text =
            """trigger todelete1 on Contact (after update) { //to be deleted }
              |""".stripMargin
        val objectTypeName = StubFileGenerator.getTriggerObjectTypeName("ToDelete1", text).getOrElse("")
        //check that resulting file contains a dummy version and object name was inserted
        assertResult("Contact")(objectTypeName)
    }
}
