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

import com.neowit.apex.{MetaXml, NodeIterator}
import com.neowit.utils.FileUtils
import org.scalatest.FunSuite


class MetaXmlTest extends FunSuite {

    test("getPackage") {
        val is = getClass.getClassLoader.getResource("meta-xml/package.xml").openStream()
        val _package = MetaXml.getPackage(is)

        for (name <- List("ApexPage", "ApexComponent", "ApexTrigger", "CustomLabels", "DataCategoryGroup",
                            "Scontrol", "StaticResource")) {
            assert( _package.getTypes.exists(_.getName == name), s"'$name' missing in package.xml")
        }
        assertResult("33.0")(_package.getVersion)

        val names = _package.getTypes.find(_.getName =="ApexComponent")
        assert(names.isDefined, "Expected members of ApexComponent")
        for (name <- List("MyComponent1", "MyComponent2", "*")) {
            assert(names.get.getMembers.contains(name), s"expected ApexComponent: member = '$name'")
        }
    }


    test("packageToXml") {
        val is = getClass.getClassLoader.getResource("meta-xml/package.xml").openStream()
        val _existingPackage = MetaXml.getPackage(is)

        val factory = javax.xml.parsers.DocumentBuilderFactory.newInstance()
        val docBuilder = factory.newDocumentBuilder()
        val etalonDocument = docBuilder.parse(getClass.getClassLoader.getResource("meta-xml/package.xml").openStream())
        etalonDocument.getDocumentElement.normalize()

        val packageXml = MetaXml.packageToXml(_existingPackage)
        val outXmlFile = FileUtils.createTempFile("package", "xml")
        FileUtils.writeFile(packageXml, outXmlFile)

        val testDocument = docBuilder.parse(outXmlFile)
        testDocument.getDocumentElement.normalize()


        assertResult("Package", "Top document element must be Package") (testDocument.getDocumentElement.getNodeName)

        assertResult(_existingPackage.getTypes.length, "Number of <types> elements")(testDocument.getElementsByTagName("types").getLength)

        assert(packageXml.contains("<version>33.0</version>"), "Version attribute not found")

        for (name <- List("ApexPage", "ApexComponent", "ApexTrigger", "CustomLabels", "DataCategoryGroup",
            "Scontrol", "StaticResource")) {
            assert(packageXml.contains(s"<name>$name</name>"), s"Type $name not found")
        }


        val allTypes = Set("ApexClass", "ApexPage", "ApexComponent", "ApexTrigger", "CustomLabels", "DataCategoryGroup",
            "Scontrol", "StaticResource")
        val typesIterator = new NodeIterator(testDocument.getElementsByTagName("types"))

        val foundTypes = List.newBuilder[String]
        for (_type <- typesIterator) {
            val members = NodeIterator(_type.getChildNodes).filter(n => n.getNodeName == "members").toList
            val typeName = NodeIterator(_type.getChildNodes).find(n => n.getNodeName == "name") match {
                case Some(node) => node.getTextContent
                case None => ""
            }
            //check that ApexComponent has all relevant members
            if ("ApexComponent" == typeName) {
                assert(members.exists(_.getTextContent == "MyComponent1"), "Expected to find MyComponent1")
                assert(members.exists(_.getTextContent == "MyComponent2"), "Expected to find MyComponent2")
                assert(members.exists(_.getTextContent == "*"), "Expected to find '*'")
            }
            foundTypes += typeName
        }
        val foundTypesSet = foundTypes.result().toSet
        val diff = allTypes -- foundTypesSet
        assert(diff.isEmpty, "No all expected types found in package.xml. Missing types: " + diff.mkString(", "))
    }

}
