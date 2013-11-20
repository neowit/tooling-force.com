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

import java.io.File
import com.sforce.soap.partner.sobject.SObject
import com.neowit.utils.{Config, Logging}
import com.sforce.soap.metadata.PackageTypeMembers

class InvalidProjectStructure(msg: String)  extends IllegalArgumentException(msg: String)

class MetadataType(val xmlName: String) extends Logging{

    def getLastModifiedDate(obj: SObject): String = {
        obj.getField("LastModifiedDate") match {
            case d if null != d => d.toString
            case _ => "1970-01-01T00:00:00.000Z"
        }
    }

    def getValueMap(obj: SObject):Map[String, String] = {
        val lastModifiedDate = getLastModifiedDate(obj).toString
        obj.getType match {
          case "StaticResource" =>
              Map("Id" -> obj.getId, "Name" -> obj.getField("Name").toString, "LastModifiedDate" -> lastModifiedDate)
          case _ =>
              Map("Id" -> obj.getId, "Name" -> obj.getField("Name").toString, "ApiVersion" -> obj.getField("ApiVersion").toString, "LastModifiedDate" -> lastModifiedDate)
        }
    }
    def getQueriableFields = {
        val fNames = List("Id", "Name", "LastModifiedDate")
        xmlName match {
            case "StaticResource" => fNames
            case _ =>
                "ApiVersion" :: fNames
        }

    }
    def getKey(obj: SObject): String = {xmlName + "." + obj.getField("Name")}
}

/**
 * package XML and -meta.xml files manipulation
 * @param appConfig - application config
 */
class MetaXml(appConfig: Config) {
    val QUERYABLE_TYPES = List("ApexClass", "ApexTrigger", "ApexPage", "ApexComponent", "RecordType", "Report")

    def listMetadataTypes(queryableOnly: Boolean = true):List[MetadataType] = {
        val packageFile = getPackageXml
        val packageXml = xml.XML.loadFile(packageFile)
        (packageXml  \ "types" \ "name").map(elem => new MetadataType(elem.text)).toList
    }

    def getPackageXml:File = {
        val projectSrcDir = appConfig.srcDir
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

}
