/*
 * Copyright (c) 2014 Andrey Gavrikov.
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

import com.sforce.soap.metadata.PackageTypeMembers
import com.neowit.utils.{FileUtils, Config}
import java.io.{FileInputStream, InputStream, File}

import org.w3c.dom.{Node, NodeList}

import scala.util.Try

class InvalidProjectStructure(msg: String)  extends Error(msg: String)

object MetaXml {

    /**
     * @return Map which looks like so:
     *         "ApexClass" -> List("MyClass.cls", "OtherClass.cls", ...)
     *         "ApexPage" -> List("MyPage.page", "OtherPage.page", ...)
     */
    def getMembersByType(packageXmlFile: File): Map[String, List[String]] = {
        val _package = MetaXml.getPackage(packageXmlFile)

        val membersByType = Map.newBuilder[String, List[String]]
        for( packageMember <- _package.getTypes) {
            val typeName = packageMember.getName
            val members = packageMember.getMembers.toList
            membersByType += typeName -> members
        }

        membersByType.result()
    }

    /**
     * assemble com.sforce.soap.metadata.Package  from package.xml file
     * @return
     */
    def getPackage(packageXmlFile: File): com.sforce.soap.metadata.Package = {
        getPackage(new FileInputStream(packageXmlFile))
    }

    def getPackage(packageXmlFileStream: InputStream): com.sforce.soap.metadata.Package = {

        val factory = javax.xml.parsers.DocumentBuilderFactory.newInstance()
        val docBuilder = factory.newDocumentBuilder()
        val document = docBuilder.parse(packageXmlFileStream)
        document.getDocumentElement.normalize()

        val versionElems = document.getElementsByTagName("version")
        val apiVersion = versionElems.item(0).getTextContent

        val _package = new com.sforce.soap.metadata.Package()
        _package.setVersion(apiVersion)

        val members =
            for (typeNode <- NodeIterator(document.getElementsByTagName("types"))) yield {
                val typeName = NodeIterator(typeNode.getChildNodes).find(n => n.getNodeName == "name") match {
                    case Some(node) => node.getTextContent
                    case None => ""
                }
                val members = NodeIterator(typeNode.getChildNodes).filter(n => n.getNodeName == "members").toList
                val ptm = new PackageTypeMembers()
                ptm.setName(typeName)
                ptm.setMembers(members.map(_.getTextContent).toArray)
                ptm
            }

        _package.setTypes(members.toArray)
        _package
    }

    /**
     * using provided apex file generate appropriate <filename>-meta.xml
     * @param apexFile - apex file, e.g. MyClass.cls
     *                 result will be file: MyClass.cls-meta.xml
     * @param extraTags - map: tagName -> text to use if any of the default values need to be overridden
     * @return - generated meta file
     */
    def generateMetaXml(apiVersion: String, apexFile: File, extraTags: Map[String, String] = Map()): Try[File] = {
        def getClassMetaXmlContent: String = {
            val metaXml =
                s"""<?xml version="1.0" encoding="UTF-8"?>
                  |<ApexClass xmlns="http://soap.sforce.com/2006/04/metadata">
                  |    <apiVersion>$apiVersion</apiVersion>
                  |    <status>${extraTags.getOrElse("status", "Active")}</status>
                  |</ApexClass>
                """.stripMargin
            metaXml
        }

        def getPageMetaXmlContent: String = {
            val metaXml =
                s"""<?xml version="1.0" encoding="UTF-8"?>
                  |<ApexPage xmlns="http://soap.sforce.com/2006/04/metadata">
                  |    <apiVersion>$apiVersion</apiVersion>
                  |    <label>${FileUtils.removeExtension(apexFile)}</label>
                  |</ApexPage>
                """.stripMargin
            metaXml
        }

        def getTriggerMetaXmlContent: String = {
            val metaXml =
                s"""<?xml version="1.0" encoding="UTF-8"?>
                  |<ApexTrigger xmlns="http://soap.sforce.com/2006/04/metadata">
                  |    <apiVersion>$apiVersion</apiVersion>
                  |    <status>${extraTags.getOrElse("status", "Active")}</status>
                  |</ApexTrigger>
                """.stripMargin
            metaXml
        }

        def getComponentMetaXmlContent: String = {
            val metaXml =
                s"""<?xml version="1.0" encoding="UTF-8"?>
                  |<component xmlns="http://soap.sforce.com/2006/04/metadata">
                  |    <apiVersion>$apiVersion</apiVersion>
                  |    <label>${FileUtils.removeExtension(apexFile)}</label>
                  |    <description>${extraTags.getOrElse("description", "this is a component without description")}</description>
                  |</component>
                """.stripMargin
            metaXml
        }

        Try ({
            val metaXml = FileUtils.getExtension(apexFile) match {
                case "cls" => getClassMetaXmlContent
                case "trigger" => getTriggerMetaXmlContent
                case "page" => getPageMetaXmlContent
                case "component" => getComponentMetaXmlContent
                case x => throw new UnsupportedApexTypeException(x)
            }
            val metaXmlFile = new File(apexFile.getParentFile, apexFile.getName + "-meta.xml")
            FileUtils.writeFile(metaXml, metaXmlFile)
            metaXmlFile
        })

    }

    def packageToXml(_package: com.sforce.soap.metadata.Package): String = {
        val newLine = System.getProperty("line.separator")
        val xmlStringBuilder = new StringBuilder()
        xmlStringBuilder.append("""<Package xmlns="http://soap.sforce.com/2006/04/metadata">""" + newLine)
        for (typeMember <- _package.getTypes) {
            xmlStringBuilder.append("    <types>" + newLine)
            for (member <- typeMember.getMembers) {
                xmlStringBuilder.append(s"        <members>$member</members>" + newLine)
            }
            xmlStringBuilder.append(s"        <name>${typeMember.getName}</name>" + newLine)
            xmlStringBuilder.append("    </types>" + newLine)
        }
        xmlStringBuilder.append(s"    <version>${_package.getVersion}</version>" + newLine)
        xmlStringBuilder.append("</Package>")
        xmlStringBuilder.toString()
    }
}

class MetaXml(config: Config) {
    //parse package
    def getPackage: com.sforce.soap.metadata.Package = {
        val packageFile = getPackageXml
        val _package = MetaXml.getPackage(packageFile)
        _package
    }

    def getPackageXml:File = {
        val projectSrcDir = config.srcDir
        if (projectSrcDir.isDirectory && projectSrcDir.canRead) {
            val packageXml = new File(projectSrcDir, "package.xml")
            if (!packageXml.isFile || !packageXml.canRead) {
                throw new InvalidProjectStructure("Can not read 'package.xml' in " + projectSrcDir.getAbsolutePath)
            }
            packageXml
        } else {
            throw new InvalidProjectStructure("Invalid 'SRC' dir" + projectSrcDir.getAbsolutePath)
        }
    }

    def createPackage(apiVersion: Double, typesMap: Map[String, List[String]]): com.sforce.soap.metadata.Package = {
        val _package = new com.sforce.soap.metadata.Package()
        _package.setVersion(apiVersion.toString)
        val members = for (typeName <- typesMap.keys) yield {
            val ptm = new PackageTypeMembers()
            ptm.setName(typeName)
            //* or individual object names
            val objNames = typesMap.getOrElse(typeName, Nil) match {
                case Nil =>
                    List[String]("*").toArray
              case _objNames =>
                  _objNames.toArray
            }

            ptm.setMembers(objNames)
            ptm
        }
        _package.setTypes(members.toArray)
        _package
    }

    //scala.xml.XML.save("therm1.xml", node)
    def packageToXml(_package: com.sforce.soap.metadata.Package): String = {
        MetaXml.packageToXml(_package)
    }
}

object NodeIterator {
    def apply(nodes: NodeList): NodeIterator = new NodeIterator(nodes)
}
class NodeIterator(nodes: NodeList) extends Iterator[org.w3c.dom.Node] {

    private var index = 0
    private var nextNode = _next()

    override def hasNext: Boolean = null != nextNode

    private def _next(): Node = {
        nextNode =  if (nodes.getLength > index) {
                        index += 1
                        nodes.item(index - 1)
                    } else
                        null

        while (null != nextNode && nextNode.getNodeType != Node.ELEMENT_NODE && hasNext) {
            nextNode = nodes.item(index)
            index += 1
        }
        nextNode
    }
    override def next(): Node = {
        val node = nextNode
        nextNode = _next()
        node
    }
}