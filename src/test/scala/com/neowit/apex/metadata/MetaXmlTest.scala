/*
 * Copyright (c) 2013 Andrey Gavrikov.
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

import org.scalatest.{FunSuite, PrivateMethodTester}
import java.io.{File, FileWriter, FileNotFoundException}
import java.util.Properties
import java.lang.IllegalArgumentException
import com.neowit.utils.{Config, OptionProperties, InvalidCommandLineException}

class MetaXmlTest extends FunSuite with PrivateMethodTester {
    val appConfig = Config.getConfig

    def withPropertiesFile(testCode: (File, FileWriter) => Any) {
        val file = File.createTempFile("test", ".properties") // create the fixture
        val writer = new FileWriter(file)
        try {
            testCode(file, writer) // "loan" the fixture to the test
        } finally {
            // clean up the fixture
            writer.close()
            file.delete()
        }
    }
    def withPackageXmlFile(testCode: (File, FileWriter) => Any) {
        val file = File.createTempFile("test", ".xml") // create the fixture
        val writer = new FileWriter(file)
        //there must be no blank line at th etop of xml file defined with
        //<?xml version="1.0" ...
        //otherwise get: The processing instruction target matching "[xX][mM][lL]" is not allowed
        val packageXmlText =
            """<?xml version="1.0" encoding="UTF-8"?>
              |<Package xmlns="http://soap.sforce.com/2006/04/metadata">
              |    <types>
              |        <members>*</members>
              |        <name>ApexClass</name>
              |    </types>
              |    <types>
              |        <members>Component1</members>
              |        <members>Component2</members>
              |        <members>ComponentX</members>
              |        <name>ApexComponent</name>
              |    </types>
              |    <types>
              |        <members>*</members>
              |        <name>ApexPage</name>
              |    </types>
              |    <types>
              |        <members>*</members>
              |        <name>ApexTrigger</name>
              |    </types>
              |    <types>
              |        <members>*</members>
              |        <name>CustomLabels</name>
              |    </types>
              |    <types>
              |        <members>*</members>
              |        <name>Scontrol</name>
              |    </types>
              |    <types>
              |        <members>*</members>
              |        <name>StaticResource</name>
              |    </types>
              |    <version>19.0</version>
              |</Package>
              |
            """.stripMargin
        writer.write(packageXmlText)
        writer.flush()
        //writer.close()
        try {
            testCode(file, writer) // "loan" the fixture to the test
        } finally {
            // clean up the fixture
            writer.close()
            file.delete()
        }
    }

    /*
    test("Package generation") {
        withPackageXmlFile { (file, writer) =>

            appConfig.load(List("--projectPath=" + file.getAbsolutePath))
            val metaXml = new MetaXml(appConfig) {
                override def getPackageXml = file
            }
            val mTypes = metaXml.listMetadataTypes()

            //check couple of types which must be present
            assert( mTypes.exists(_.xmlName == "ApexClass") , "Expected To find ApexClass in the list of types")
            assert( mTypes.exists(_.xmlName == "CustomLabels"), "Expected To find CustomLabels in the list of types")

            val _package = metaXml.getPackage
            assertResult("19.0", "Wrong API version") {_package.getVersion}
            assertResult(7, "Wrong number of types returned") {_package.getTypes.length}
            //check if correct members were set
            assertResult(List("*"), "Wrong wildcard member list returned") {
                _package.getTypes.find(_.getName == "ApexClass").get.getMembers.toList
            }
            assertResult(List[String]("Component1", "Component2", "ComponentX"), "Wrong named member list returned") {
                _package.getTypes.find(_.getName == "ApexComponent").get.getMembers.toList
            }

        }
    }
    */
}
