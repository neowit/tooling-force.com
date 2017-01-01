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

package com.neowit.apex.actions.tooling

import com.sforce.soap.tooling._
import com.neowit.apex.Session
import java.io.File
import com.neowit.apex.actions.DescribeMetadata
import com.neowit.utils.FileUtils

class UnsupportedTypeForToolingException(msg: String) extends Exception(msg: String)

object ApexMember {
    val APEX_CLASS = "ApexClass"
    val APEX_PAGE = "ApexPage"
    val APEX_TRIGGER = "ApexTrigger"
    val APEX_COMPONENT = "ApexComponent"
    val STATIC_RESOURCE = "StaticResource"
    private val METADATA_CONTAINER_TYPES = Set[String](APEX_CLASS, APEX_PAGE, APEX_TRIGGER, APEX_COMPONENT)
    private val SUPPORTED_TYPES = METADATA_CONTAINER_TYPES + STATIC_RESOURCE

    def getInstance(file: File, session: Session): ApexMember = {
        val member = DescribeMetadata.getXmlNameBySuffix(session, FileUtils.getExtension(file)) match {
            case Some(xmlType) => xmlType match {
                case APEX_CLASS => new ClassMember
                case APEX_PAGE => new PageMember
                case APEX_TRIGGER => new TriggerMember
                case APEX_COMPONENT => new ComponentMember
                case _ =>
                    throw new UnsupportedTypeForToolingException("File " + file.getName + " with type=" + xmlType + " is not supported with Tooling API")
            }
            case None => throw new UnsupportedTypeForToolingException("File " + file.getName + " is not supported with Tooling API")
        }
        member.setBody(file)
        val key = session.getKeyByFile(file)
        session.getData(key).get("Id") match {
            case Some(id) =>
                member.setContentEntityId(id.asInstanceOf[String])
            case None =>
        }
        member
    }

    def isSupportedTypeWithMetadataContainer(file: File, session: Session): Boolean  = {
        DescribeMetadata.getXmlNameBySuffix(session, FileUtils.getExtension(file)) match {
            case Some(xmlType) =>
                METADATA_CONTAINER_TYPES.contains(xmlType)
            case None => false
        }
    }
    def isSupportedType(file: File, session: Session): Boolean  = {
        DescribeMetadata.getXmlNameBySuffix(session, FileUtils.getExtension(file)) match {
            case Some(xmlType) =>
                SUPPORTED_TYPES.contains(xmlType)
            case None => false
        }
    }

}

trait ApexMember {
    val xmlType: String
    def setBody(text: String): Unit
    def setMetadataContainerId(containerId: String): Unit
    def setContentEntityId(id: String): Unit
    def getContentEntityId: String

    def setBody(file: File): Unit = {
        setBody(FileUtils.readFile(file).mkString)
    }
}

class ClassMember extends ApexClassMember with ApexMember {
    val xmlType = ApexMember.APEX_CLASS
}

class TriggerMember extends ApexTriggerMember with ApexMember {
    val xmlType = ApexMember.APEX_TRIGGER
}

class PageMember extends ApexPageMember with ApexMember {
    val xmlType = ApexMember.APEX_PAGE
}

class ComponentMember extends ApexComponentMember with ApexMember {
    val xmlType = ApexMember.APEX_COMPONENT
}
