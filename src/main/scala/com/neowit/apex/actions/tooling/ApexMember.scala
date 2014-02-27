package com.neowit.apex.actions.tooling

import com.sforce.soap.tooling._
import com.neowit.apex.Session
import java.io.File
import com.neowit.apex.actions.DescribeMetadata
import com.neowit.utils.FileUtils

class UnsupportedTypeForToolingException(msg: String) extends Exception(msg: String)

object ApexMember {
    type MemberGenerator = () => ApexMember
    def newClassMember:MemberGenerator = () => new ClassMember
    def newPageMember:MemberGenerator = () => new PageMember
    def newTriggerMember:MemberGenerator = () => new TriggerMember
    def newComponentMember:MemberGenerator = () => new ComponentMember

    var SUPPORTED_TYPES_MAP = Map[String, MemberGenerator]()
    //register known types
    registerApexMemberType(newClassMember)
    registerApexMemberType(newPageMember)
    registerApexMemberType(newTriggerMember)
    registerApexMemberType(newComponentMember)

    def getInstance(file: File, session: Session): ApexMember = {
        val member = DescribeMetadata.getXmlNameBySuffix(session, FileUtils.getExtension(file)) match {
            case Some(xmlType) =>
                SUPPORTED_TYPES_MAP.get(xmlType) match {
                    case Some(generator) => generator()
                    case None =>
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
                SUPPORTED_TYPES_MAP.contains(xmlType)
            case None => false
        }
    }

    private def registerApexMemberType(f: MemberGenerator) {
        val member = f()
        SUPPORTED_TYPES_MAP += member.xmlType -> f
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

    def getEntityId: String = { entityId }

    def getInstance: SObject = { getInstanceImpl }

    val xmlType: String
    protected def getInstanceImpl: SObject
    protected def setMetadataContainerIdImpl(containerId: String)
    protected def setBodyImpl(text: String)
    protected def setContentEntityIdImpl(id: String)
}

class ClassMember extends ApexMember {
    private val instance = new ApexClassMember()

    val xmlType = "ApexClass"

    protected def getInstanceImpl: SObject = instance

    protected def setContentEntityIdImpl(id: String): Unit = instance.setContentEntityId(id)

    protected def setBodyImpl(text: String): Unit = instance.setBody(text)

    protected def setMetadataContainerIdImpl(containerId: String): Unit = instance.setMetadataContainerId(containerId)
}

class TriggerMember extends ApexMember {
    private val instance = new ApexTriggerMember()

    val xmlType = "ApexTrigger"
    protected def getInstanceImpl: SObject = instance

    protected def setContentEntityIdImpl(id: String): Unit = instance.setContentEntityId(id)

    protected def setBodyImpl(text: String): Unit = instance.asInstanceOf[ApexTriggerMember].setBody(text)

    protected def setMetadataContainerIdImpl(containerId: String): Unit = instance.setMetadataContainerId(containerId)
}

class PageMember extends ApexMember {
    private val instance = new ApexPageMember()

    val xmlType = "ApexPage"
    protected def getInstanceImpl: SObject = instance

    protected def setContentEntityIdImpl(id: String): Unit = instance.setContentEntityId(id)

    protected def setBodyImpl(text: String): Unit = instance.setBody(text)

    protected def setMetadataContainerIdImpl(containerId: String): Unit = instance.setMetadataContainerId(containerId)
}

class ComponentMember extends ApexMember {
    private val instance = new ApexComponentMember()

    val xmlType = "ApexComponent"
    protected def getInstanceImpl: SObject = instance

    protected def setContentEntityIdImpl(id: String): Unit = instance.setContentEntityId(id)

    protected def setBodyImpl(text: String): Unit = instance.setBody(text)

    protected def setMetadataContainerIdImpl(containerId: String): Unit = instance.setMetadataContainerId(containerId)
}
