package com.neowit.apex

import com.neowit.utils.FileUtils
import com.sun.xml.internal.ws.util.xml.NodeListIterator
import org.scalatest.FunSuite
import org.w3c.dom.{Node, NodeList}


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
        //check that ApexComponent has all relevant members
        val typesIterator = new NodeIterator(testDocument.getElementsByTagName("types"))

        val foundTypes = List.newBuilder[String]
        for (_type <- typesIterator) {
            val members = new NodeIterator(_type.getChildNodes).filter(n => null != n && n.getNodeName == "members").toList
            val typeName = new NodeIterator(_type.getChildNodes).find(n => null != n && n.getNodeName == "name") match {
                case Some(node) => node.getTextContent
                case None => ""
            }
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


    class NodeIterator(nodes: NodeList) extends Iterator[org.w3c.dom.Node] {

        private var index = 0
        override def hasNext: Boolean = nodes.getLength > index

        override def next(): Node = {
            var node = nodes.item(index)

            while (null != node && node.getNodeType != Node.ELEMENT_NODE && hasNext) {
                index += 1
                node = nodes.item(index)
            }
            if (null != node && Node.ELEMENT_NODE == node.getNodeType) {
                index += 1
                node
            } else {
                null
            }
        }
    }

}
