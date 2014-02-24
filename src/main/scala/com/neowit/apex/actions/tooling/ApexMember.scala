package com.neowit.apex.actions.tooling

import com.sforce.soap.tooling.{ApexPageMember, ApexTriggerMember, ApexClassMember, SObject}
import com.neowit.apex.Session
import java.io.File
import com.neowit.apex.actions.DescribeMetadata
import com.neowit.utils.FileUtils

class UnsupportedTypeForToolingException(msg: String) extends Exception(msg: String)

object ApexMember {
    val SUPPORTED_TYPES = Set("ApexClass", "ApexTrigger", "ApexPage")
    def getInstance(file: File, session: Session): ApexMember = {
        val member = DescribeMetadata.getXmlNameBySuffix(session, FileUtils.getExtension(file)) match {
            case Some(xmlType) =>
                xmlType match {
                    case "ApexClass" => new ClassMember
                    case "ApexTrigger" => new TriggerMember
                    case "ApexPage" => new PageMember
                    case _ => throw new UnsupportedTypeForToolingException("File " + file.getName + " with type=" + xmlType + " is not supported with Tooling API")
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
    private var entityId: String = ""

    def setMetadataContainerId(containerId: String) {setMetadataContainerIdImpl(containerId)}
    def setBody(text: String) {setBodyImpl(text)}
    def setBody(file: File) {
        setBodyImpl(scala.io.Source.fromFile(file).mkString)
    }
    def setContentEntityId(id: String) {
        entityId = id
        setContentEntityIdImpl(id)
    }

    def getEntityId: String = {
        entityId
    }

    def getInstance: SObject = { getInstanceImpl }

    def getXmlType: String
    protected def getInstanceImpl: SObject
    protected def setMetadataContainerIdImpl(containerId: String)
    protected def setBodyImpl(text: String)
    protected def setContentEntityIdImpl(id: String)
}

class ClassMember extends ApexMember {
    private val instance = new ApexClassMember()

    def getXmlType = "ApexClass"
    protected def getInstanceImpl: SObject = instance

    protected def setContentEntityIdImpl(id: String): Unit = instance.setContentEntityId(id)

    protected def setBodyImpl(text: String): Unit = instance.asInstanceOf[ApexClassMember].setBody(text)

    protected def setMetadataContainerIdImpl(containerId: String): Unit = instance.asInstanceOf[ApexClassMember].setMetadataContainerId(containerId)

}

class TriggerMember extends ApexMember {
    private val instance = new ApexTriggerMember()

    def getXmlType = "ApexTrigger"
    protected def getInstanceImpl: SObject = instance

    protected def setContentEntityIdImpl(id: String): Unit = instance.asInstanceOf[ApexTriggerMember].setContentEntityId(id)

    protected def setBodyImpl(text: String): Unit = instance.asInstanceOf[ApexTriggerMember].setBody(text)

    protected def setMetadataContainerIdImpl(containerId: String): Unit = instance.setMetadataContainerId(containerId)

}

class PageMember extends ApexMember {
    private val instance = new ApexPageMember()

    def getXmlType = "ApexPage"
    protected def getInstanceImpl: SObject = instance

    protected def setContentEntityIdImpl(id: String): Unit = instance.setContentEntityId(id)

    protected def setBodyImpl(text: String): Unit = instance.setBody(text)

    protected def setMetadataContainerIdImpl(containerId: String): Unit = instance.setMetadataContainerId(containerId)

}
