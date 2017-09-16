/*
 *
 *  * Copyright (c) 2017 Andrey Gavrikov.
 *  * this file is part of tooling-force.com application
 *  * https://github.com/neowit/tooling-force.com
 *  *
 *  * This program is free software: you can redistribute it and/or modify
 *  * it under the terms of the GNU Lesser General Public License as published by
 *  * the Free Software Foundation, either version 3 of the License, or
 *  * (at your option) any later version.
 *  *
 *  * This program is distributed in the hope that it will be useful,
 *  * but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  * GNU Lesser General Public License for more details.
 *  *
 *  * You should have received a copy of the GNU Lesser General Public License
 *  * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 */

package com.neowit.apex.completion

import java.io.{File, FileInputStream}
import java.nio.file.{Path, Paths}
import java.util.Properties

import com.neowit.apex.{ProjectsCache, Session}
import com.neowit.apexscanner.Project
import com.neowit.apexscanner.antlr.CaretUtils
import com.neowit.apexscanner.completion.CompletionFinder
import com.neowit.utils.BasicConfig
import org.scalatest.FunSuite

/**
  * Created by Andrey Gavrikov
  *
  * Test completions that require access to Salesforce DB
  */
class ListCompletionsTest extends FunSuite {


    private val projectPath: Path = Paths.get(System.getProperty("java.io.tmpdir") + File.separator + "ListCompletionsTest")

    test("ListCompletions: `new Opportunity(<CARET>`") {
        val text =
            """
              |class CompletionTester {
              | new Opportunity(<CARET>
              |}
            """.stripMargin
        val resultNodes = listCompletions(text, loadSobjectLib = true)
        assert(resultNodes.length > 1, "Expected to find non empty result")
        assert(resultNodes.exists(_.symbolName == "Name"), "Expected symbol not found")
    }

    test("ListCompletions: `new Opportunity(Name = 'Test', <CARET>`") {
        val text =
            """
              |class CompletionTester {
              | new Opportunity(Name = 'Test', <CARET>
              |}
            """.stripMargin
        val resultNodes = listCompletions(text, loadSobjectLib = true)
        assert(resultNodes.length > 1, "Expected to find non empty result")
        assert(resultNodes.exists(_.symbolName == "Amount"), "Expected symbol not found")
    }

    test("ListCompletions: `User - `usr.<CARET>`") {
        val text =
            """
              |class CompletionTester {
              | User usr;
              | usr.<CARET>
              |}
            """.stripMargin
        val resultNodes = listCompletions(text, loadSobjectLib = true)
        assert(resultNodes.length > 1, "Expected to find non empty result")
        assert(resultNodes.exists(_.symbolName == "CreatedDate"), "Expected symbol not found")
        assert(resultNodes.exists(_.symbolName == "Username"), "Expected symbol not found")
    }

    test("ListCompletions: User - `usr.profile.<CARET>`") {
        val text =
            """
              |class CompletionTester {
              | User usr;
              | usr.profile.<CARET>
              |}
            """.stripMargin
        val resultNodes = listCompletions(text, loadSobjectLib = true)
        assert(resultNodes.length > 1, "Expected to find non empty result")
        assert(resultNodes.exists(_.symbolName == "CreatedDate"), "Expected symbol not found")
        assert(resultNodes.exists(_.symbolName == "Name"), "Expected symbol not found")
        assert(resultNodes.exists(_.symbolName == "Description"), "Expected symbol not found")

    }

    test("ListCompletions: `select from <CARET>`") {
        val text =
            """
              |class CompletionTester {
              | System.debug([select from <CARET> ]);
              |}
            """.stripMargin
        val resultNodes = listCompletions(text, loadSobjectLib = true)
        assert(resultNodes.length > 1, "Expected to find non empty result")
        assert(resultNodes.exists(_.symbolName == "Account"), "Expected symbol not found")
        assert(resultNodes.exists(_.symbolName == "Contact"), "Expected symbol not found")
        assert(resultNodes.exists(_.symbolName == "User"), "Expected symbol not found")
    }

    test("ListCompletions: `select from A<CARET>`") {
        val text =
            """
              |class CompletionTester {
              | System.debug([select from A<CARET> ]);
              |}
            """.stripMargin
        val resultNodes = listCompletions(text, loadSobjectLib = true)
        assert(resultNodes.length > 1, "Expected to find non empty result")
        assert(resultNodes.exists(_.symbolName == "Account"), "Expected symbol not found")
        assert(resultNodes.exists(_.symbolName == "AccountHistory"), "Expected symbol not found")
        assert(resultNodes.exists(_.symbolName == "Attachment"), "Expected symbol not found")
    }

    test("ListCompletions: `select from A<CARET>` - multiline") {
        val text =
            """
              |class CompletionTester {
              | System.debug([select from
              | A<CARET> ]);
              |}
            """.stripMargin
        val resultNodes = listCompletions(text, loadSobjectLib = true)
        assert(resultNodes.length > 1, "Expected to find non empty result")
        assert(resultNodes.exists(_.symbolName == "Account"), "Expected symbol not found")
        assert(resultNodes.exists(_.symbolName == "AccountHistory"), "Expected symbol not found")
        assert(resultNodes.exists(_.symbolName == "Attachment"), "Expected symbol not found")
    }

    test("ListCompletions: `select <CARET> from Account` - single line") {
        val text =
            """
              |class CompletionTester {
              | System.debug([select <CARET> from Account]);
              |}
            """.stripMargin
        val resultNodes = listCompletions(text, loadSobjectLib = true)
        assert(resultNodes.length > 1, "Expected to find non empty result")
        assert(resultNodes.exists(_.symbolName == "CreatedDate"), "Expected symbol not found")
        assert(resultNodes.exists(_.symbolName == "Name"), "Expected symbol not found")
        assert(resultNodes.exists(_.symbolName == "Description"), "Expected symbol not found")
        assert(resultNodes.exists(_.symbolName == "ParentId"), "Expected symbol not found")

    }
    test("ListCompletions: `select <CARET> from Account` - multi line") {
        val text =
            """
              |class CompletionTester {
              | System.debug([select
              |<CARET> from Account]);
              |}
            """.stripMargin
        val resultNodes = listCompletions(text, loadSobjectLib = true)
        assert(resultNodes.length > 1, "Expected to find non empty result")
        assert(resultNodes.exists(_.symbolName == "CreatedDate"), "Expected symbol not found")
        assert(resultNodes.exists(_.symbolName == "Name"), "Expected symbol not found")
        assert(resultNodes.exists(_.symbolName == "Description"), "Expected symbol not found")
        assert(resultNodes.exists(_.symbolName == "ParentId"), "Expected symbol not found")

    }
    test("ListCompletions: `select id, (select from <CARET>) from Account`") {
        val text =
            """
              |class CompletionTester {
              | System.debug([select id, (select from <CARET>) from Account]);
              |}
            """.stripMargin
        val resultNodes = listCompletions(text, loadSobjectLib = true)
        assert(resultNodes.length > 1, "Expected to find non empty result")
        assert(resultNodes.exists(_.symbolName == "Contacts"), "Expected symbol not found")
        assert(resultNodes.exists(_.symbolName == "ChildAccounts"), "Expected symbol not found")

    }

    ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    private def listCompletions(text: String, loadSobjectLib: Boolean = false, documentName: String = "test"): Seq[com.neowit.apexscanner.symbols.Symbol] = {
        val projectOpt =
            if (loadSobjectLib) {
                val is = getClass.getClassLoader.getResource("paths.properties").openStream()
                val paths = new Properties()
                paths.load(is)

                val loginCredentialsPath = paths.getProperty("loginCredentialsPath")
                val loginProperties = new Properties()
                loginProperties.load(new FileInputStream(loginCredentialsPath))

                val basicConfig: BasicConfig = new BasicConfig()
                basicConfig.setProperty("authConfigPath", loginCredentialsPath)
                //val config: ConfigWithSfdcProject = new ConfigWithSfdcProject(basicConfig)
                val session: Session = new Session(basicConfig, isReadOnly = true)
                ProjectsCache.getProject(projectPath.toFile, session, loadStdLib = true, loadSobjectLib )
            } else {
                Option(Project(projectPath))
            }

        projectOpt match {
            case Some(project) =>
                val caretInDocument = CaretUtils.getCaret(text, Paths.get(documentName))
                project.getAst(caretInDocument.document) match {
                    case Some(result) =>
                        val completionFinder = new CompletionFinder(project)
                        completionFinder.listCompletions(caretInDocument)
                    case _ =>
                        Seq.empty
                }
            case None =>
                throw new IllegalStateException("Failed to initialise Project & Session")
        }
    }
}
