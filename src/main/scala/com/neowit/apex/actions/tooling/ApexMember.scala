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
    private val SUPPORTED_TYPES = Set[String](APEX_CLASS, APEX_PAGE, APEX_TRIGGER, APEX_COMPONENT)

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
    def setBody(text: String)
    def setMetadataContainerId(containerId: String)
    def setContentEntityId(id: String)
    def getContentEntityId: String

    def setBody(file: File) {
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
