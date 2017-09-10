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
import com.neowit.apexscanner.ast.QualifiedName
import com.neowit.apexscanner.nodes.{AstNode, IsTypeDefinition}
import com.neowit.apexscanner.resolvers.AscendingDefinitionFinder
import com.neowit.utils.BasicConfig
import org.scalatest.FunSuite

/**
  * Created by Andrey Gavrikov 
  */
class FindDefinitionTest extends FunSuite {
    private val projectPath: Path = Paths.get(System.getProperty("java.io.tmpdir") + File.separator + "FindDefinitionTest")

    test("findDefinition: inside SOQL statement: Name") {
        val text =
            """
              |class CompletionTester {
              | Integer i = [select Na<CARET>me from Account];
              |}
            """.stripMargin

        val resultNodes = findDefinition(text)
        assert(resultNodes.nonEmpty, "Expected to find non empty result")
        assertResult(1,"Wrong number of results found") (resultNodes.length)
        resultNodes.head match {
            case typeDefinition: IsTypeDefinition =>
                assertResult(Option(QualifiedName(Array("Account", "Name"))), "Wrong caret type detected.")(typeDefinition.qualifiedName)
                assertResult(Option(QualifiedName(Array("String"))), "Wrong caret type detected.")(typeDefinition.getValueType.map(_.qualifiedName))
            case _ =>
                fail( "Failed to locate correct node. Expected method1()")
        }
    }
    test("findDefinition: inside SOQL statement: a.Name") {
        val text =
            """
              |class CompletionTester {
              | Integer i = [select a.Na<CARET>me from Account a];
              |}
            """.stripMargin
        //val resultNodes = findDefinition(text).futureValue
        val resultNodes = findDefinition(text)
        assert(resultNodes.nonEmpty, "Expected to find non empty result")
        assertResult(1,"Wrong number of results found") (resultNodes.length)
        resultNodes.head match {
            case typeDefinition: IsTypeDefinition =>
                assertResult(Option(QualifiedName(Array("Account", "Name"))), "Wrong caret type detected.")(typeDefinition.qualifiedName)
                assertResult(Option(QualifiedName(Array("String"))), "Wrong caret type detected.")(typeDefinition.getValueType.map(_.qualifiedName))
            case _ =>
                fail( "Failed to locate correct node. Expected method1()")
        }
    }

    var _projectWithLibs: Option[Project] = None
    private def findDefinition(text: String, loadStdLib: Boolean = true, loadSobjectLib: Boolean = true): scala.Seq[AstNode] = {
        val projectOpt: Option[Project] =
            if (loadStdLib || loadSobjectLib) {
                _projectWithLibs match {
                    case Some(_project) =>
                        // re-use previously loaded project because loading StdLib takes a lot of time
                        Option(_project)
                    case None =>
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
                }
            }  else {
                Option(Project(projectPath))
            }

        projectOpt match {
            case Some(project) =>
                val caretInDocument = CaretUtils.getCaret(text, Paths.get("test"))
                project.getAst(caretInDocument.document) match {
                    case Some(result) =>
                        val finder = new AscendingDefinitionFinder()
                        finder.findDefinition(result.rootNode, caretInDocument.position)
                    case _ =>
                        Seq.empty
                }
            case None =>
                throw new IllegalStateException("Failed to initialise Project & Session")
        }
    }

}
