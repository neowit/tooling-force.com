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
import com.neowit.utils.Config
import java.io.File

class InvalidProjectStructure(msg: String)  extends Error(msg: String)

class MetaXml(config: Config) {
    //parse package
    def getPackage: com.sforce.soap.metadata.Package = {
        val _package = new com.sforce.soap.metadata.Package()
        val packageFile = getPackageXml
        val packageXml = xml.XML.loadFile(packageFile)

        val apiVersion =(packageXml \ "version").text
        _package.setVersion(apiVersion)

        val members =
            for (typeNode <- (packageXml  \ "types")) yield {
                val name = (typeNode \ "name").text
                val currMembers = (typeNode \ "members") match {
                    case n if n.isInstanceOf[xml.Node] =>
                        Array(n.asInstanceOf[xml.Node].text)

                    case n if n.isInstanceOf[xml.NodeSeq] =>
                        n.asInstanceOf[xml.NodeSeq].map(_.text).toArray

                }
                val ptm = new PackageTypeMembers()
                ptm.setName(name)
                ptm.setMembers(currMembers)
                ptm
            }

        _package.setTypes(members.toArray)
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
    def packageToXml(_package: com.sforce.soap.metadata.Package): scala.xml.Elem = {
        val packageXml =
                <Package xmlns="http://soap.sforce.com/2006/04/metadata">
                {
                    for (typeMember <- _package.getTypes) yield {
                        <types>
                            {for (member <- typeMember.getMembers) yield
                            <members>{member}</members>
                            }
                            <name>{typeMember.getName}</name>
                        </types>
                    }
                }
                    <version>{_package.getVersion}</version>
                </Package>

        packageXml
    }
}
