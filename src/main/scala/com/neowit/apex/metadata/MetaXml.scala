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

import com.neowit.apex.session.SfdcSession
import java.io.File
import com.sforce.soap.partner.sobject.SObject
import com.neowit.utils.Logging

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
 * @param session
 */
class MetaXml(session: SfdcSession) {
    val QUERYABLE_TYPES = List("ApexClass", "ApexTrigger", "ApexPage", "ApexComponent", "RecordType", "Report")
    val appConfig = session.getConfig

    def listMetadataTypes(queryableOnly: Boolean = true):List[MetadataType] = {
        val projectSrcDir = appConfig.srcDir

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

}
