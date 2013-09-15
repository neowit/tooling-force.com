package com.neowit.apex.tooling

import com.sforce.soap.tooling._
import java.io._
import scala.Some
import scala.sys.process.BasicIO

/**
 * User: andrey
 * Date: 18/09/2013
 */
trait TypeHelper extends Logging {
    def getValueMap(obj: SObject): Map[String, String]
    val typeName: String
    val fileExtension: String
    val directoryName: String
    def getApiVersion(obj: SObject): Double
    def getName(obj: SObject): String
    def getBody(obj: SObject): String
    def getLabel(obj: SObject): String
    def getDescription(obj: SObject): String
    def newMemberInstance: Member
    val filenameFilter: FilenameFilter = new FilenameFilter {
        def accept(p1: File, p2: String): Boolean = p2.endsWith(fileExtension)
    }

    def getKey(obj: SObject): String = {typeName + "." + getName(obj)}
    def getKey(f: File): String = {typeName + "." + (fileExtension + "$").r.replaceFirstIn(f.getName, "")}
    def getKey(name: String): String = {typeName + "." + name}

    def getContentSOQL: String

    def getMemberInstance(sessionData: SessionData, f: File) = {
        val metadataContainerId = sessionData.getField("MetadataContainer", "Id").get
        val objectId = sessionData.getField(getKey(f), "Id").get
        val member = newMemberInstance
        member.setContentEntityId(objectId)
        member.setBody(getBody(f))
        member.setMetadataContainerId(metadataContainerId)
        member.getSObject
    }
    def listFiles(srcDir: File): Array[File] = {
        srcDir.listFiles().find(directoryName == _.getName) match {
          case Some(x) => x.listFiles(filenameFilter)
          case None => Array()
        }
    }
    def getBody(f: File): String = {
        scala.io.Source.fromFile(f).mkString
    }
    def bodyToFile(appConfig: Config, record: SObject) = {
        val buffer = getBody(record).getBytes
        if (buffer.length > 0) {
            val fileName = getName(record) + fileExtension
            val file = new File(appConfig.mkdirs(directoryName) + File.separator + fileName)
            try {
                val output = new FileOutputStream(file)
                //output.write(Base64.decode(buffer))
                output.write(buffer)
                output.close()
                //generate meta-xml

                val meta =
                    s"""|<?xml version="1.0" encoding="UTF-8"?>
                        |<$typeName xmlns="http://soap.sforce.com/2006/04/metadata">
                        |    <apiVersion>${getApiVersion(record)}</apiVersion>
                        |    <description>${getDescription(record)}</description>
                        |    <label>${getLabel(record)}</label>
                        |</$typeName>""".stripMargin
                BasicIO.transferFully(new ByteArrayInputStream(meta.getBytes("UTF-8")), new FileOutputStream(file.getAbsolutePath + "-meta.xml"))
            } catch {
                case ex: FileNotFoundException =>
                    logger.error("Error: Unable to save file: '" + fileName + "'\n using path: " + file.getAbsolutePath)
                    logger.error(ex.getMessage)
            }
        }
    }
}

object TypeHelpers {
    val list = List(new ClassHelper(), new TriggerHelper(), new PageHelper())

    class ClassHelper extends TypeHelper {
        val typeName: String = "ApexClass"
        val directoryName: String = "classes"
        val fileExtension: String = ".cls"
        def getApiVersion(obj: SObject): Double = obj.asInstanceOf[ApexClass].getApiVersion
        def getName(obj: SObject): String = obj.asInstanceOf[ApexClass].getName
        def getBody(obj: SObject): String = obj.asInstanceOf[ApexClass].getBody
        def getLabel(obj: SObject): String = getName(obj)
        def getDescription(obj: SObject): String = ""
        def newMemberInstance: Member = Member.toMember(new ApexClassMember())
        def getContentSOQL: String = "select Id, Name, ApiVersion, LastModifiedDate, Body from " + typeName

        def getValueMap(obj: SObject) = {
            val objTyped = obj.asInstanceOf[ApexClass]
            Map("Id" -> objTyped.getId, "Name" -> objTyped.getName,
                "LastModifiedDate" -> objTyped.getLastModifiedDate.getTimeInMillis.toString,
                "ApiVersion" -> objTyped.getApiVersion.toString)
        }
    }
    class TriggerHelper extends TypeHelper {
        val typeName: String = "ApexTrigger"
        val directoryName: String = "triggers"
        val fileExtension: String = ".trigger"
        def getApiVersion(obj: SObject): Double = obj.asInstanceOf[ApexTrigger].getApiVersion
        def getName(obj: SObject): String = obj.asInstanceOf[ApexTrigger].getName
        def getBody(obj: SObject): String = obj.asInstanceOf[ApexTrigger].getBody
        def getLabel(obj: SObject): String = getName(obj)
        def getDescription(obj: SObject): String = ""
        def newMemberInstance: Member = Member.toMember(new ApexTriggerMember())
        def getContentSOQL: String = "select Id, Name, ApiVersion, LastModifiedDate, Body from " + typeName

        def getValueMap(obj: SObject) = {
            val objTyped = obj.asInstanceOf[ApexTrigger]
            Map("Id" -> objTyped.getId, "Name" -> objTyped.getName,
                "LastModifiedDate" -> objTyped.getLastModifiedDate.getTimeInMillis.toString,
                "ApiVersion" -> objTyped.getApiVersion.toString)
        }
    }
    class PageHelper extends TypeHelper {
        val typeName: String = "ApexPage"
        val directoryName: String = "pages"
        val fileExtension: String = ".page"
        def getApiVersion(obj: SObject): Double = obj.asInstanceOf[ApexPage].getApiVersion
        def getName(obj: SObject): String = obj.asInstanceOf[ApexPage].getName
        def getBody(obj: SObject): String = obj.asInstanceOf[ApexPage].getMarkup
        def getLabel(obj: SObject): String = obj.asInstanceOf[ApexPage].getMasterLabel
        def getDescription(obj: SObject): String = obj.asInstanceOf[ApexPage].getDescription
        def newMemberInstance: Member = Member.toMember(new ApexPageMember())
        def getContentSOQL: String = "select Id, Name, ApiVersion, LastModifiedDate, Markup, MasterLabel, Description from " + typeName

        def getValueMap(obj: SObject) = {
            val objTyped = obj.asInstanceOf[ApexPage]
            Map("Id" -> objTyped.getId, "Name" -> objTyped.getName,
                "LastModifiedDate" -> objTyped.getLastModifiedDate.getTimeInMillis.toString,
                "ApiVersion" -> objTyped.getApiVersion.toString)
        }
    }

    class ComponentHelper extends TypeHelper {
        val typeName: String = "ApexComponent"
        val directoryName: String = "components"
        val fileExtension: String = ".component"
        def getApiVersion(obj: SObject): Double = obj.asInstanceOf[ApexComponent].getApiVersion
        def getName(obj: SObject): String = obj.asInstanceOf[ApexComponent].getName
        def getBody(obj: SObject): String = obj.asInstanceOf[ApexComponent].getMarkup
        def getLabel(obj: SObject): String = obj.asInstanceOf[ApexComponent].getMasterLabel
        def getDescription(obj: SObject): String = obj.asInstanceOf[ApexComponent].getDescription
        def newMemberInstance: Member = Member.toMember(new ApexComponentMember())
        def getContentSOQL: String = "select Id, Name, ApiVersion, LastModifiedDate, Markup, MasterLabel, Description from " + typeName

        def getValueMap(obj: SObject) = {
            val objTyped = obj.asInstanceOf[ApexComponent]
            Map("Id" -> objTyped.getId, "Name" -> objTyped.getName,
                "LastModifiedDate" -> objTyped.getLastModifiedDate.getTimeInMillis.toString,
                "ApiVersion" -> objTyped.getApiVersion.toString)
        }
    }
}


/**
 * SFDC Tooling SOAP API has broken object hierarchy and all Apex<X>Member classes inherit directly from SObject which
 * does not have any of Apex<X>Member methods, like getMetadataContainerId, etc
 * Having to fix that using this ugly workaround in order to avoid direct casting everywhere in the code
 */
trait Member {
    def getSObject: SObject

    def getBody: String
    def getContentEntityId: String
    def getMetadataContainerId: String
    def setContentEntityId(contentEntityId: String)
    def setBody(body: String)
    def setMetadataContainerId(metadataContainerId: String)

}
object Member {
    def toMember(m: SObject) = m match {
        case m: ApexClassMember => new MemberClass(m)
        case m: ApexPageMember => new MemberPage(m)
        case m: ApexTriggerMember => new MemberTrigger(m)
        case m: ApexComponentMember => new MemberComponent(m)
        case _ => throw new IllegalArgumentException("Unsupported member type: " + m.getClass)
    }
}

class MemberClass(m: ApexClassMember) extends Member {
    def getSObject: SObject = this.asInstanceOf[ApexClassMember]

    def getBody: String = this.asInstanceOf[ApexClassMember].getBody
    def getContentEntityId: String = this.asInstanceOf[ApexClassMember].getContentEntityId
    def getMetadataContainerId: String= this.asInstanceOf[ApexClassMember].getMetadataContainerId
    def setContentEntityId(contentEntityId: String) {this.asInstanceOf[ApexClassMember].setContentEntityId(contentEntityId)}
    def setBody(body: String) {this.asInstanceOf[ApexClassMember].setBody(body)}
    def setMetadataContainerId(metadataContainerId: String) {this.asInstanceOf[ApexClassMember].setMetadataContainerId(metadataContainerId)}
}
class MemberPage(m: ApexPageMember) extends Member {
    def getSObject: SObject = this.asInstanceOf[ApexPageMember]

    def getBody: String = this.asInstanceOf[ApexPageMember].getBody
    def getContentEntityId: String = this.asInstanceOf[ApexPageMember].getContentEntityId
    def getMetadataContainerId: String= this.asInstanceOf[ApexPageMember].getMetadataContainerId
    def setContentEntityId(contentEntityId: String) {this.asInstanceOf[ApexPageMember].setContentEntityId(contentEntityId)}
    def setBody(body: String) {this.asInstanceOf[ApexPageMember].setBody(body)}
    def setMetadataContainerId(metadataContainerId: String) {this.asInstanceOf[ApexPageMember].setMetadataContainerId(metadataContainerId)}
}
class MemberTrigger(m: ApexTriggerMember) extends Member {
    def getSObject: SObject = this.asInstanceOf[ApexTriggerMember]

    def getBody: String = this.asInstanceOf[ApexTriggerMember].getBody
    def getContentEntityId: String = this.asInstanceOf[ApexTriggerMember].getContentEntityId
    def getMetadataContainerId: String= this.asInstanceOf[ApexTriggerMember].getMetadataContainerId
    def setContentEntityId(contentEntityId: String) {this.asInstanceOf[ApexTriggerMember].setContentEntityId(contentEntityId)}
    def setBody(body: String) {this.asInstanceOf[ApexTriggerMember].setBody(body)}
    def setMetadataContainerId(metadataContainerId: String) {this.asInstanceOf[ApexTriggerMember].setMetadataContainerId(metadataContainerId)}
}
class MemberComponent(m: ApexComponentMember) extends Member {
    def getSObject: SObject = this.asInstanceOf[ApexComponentMember]

    def getBody: String = this.asInstanceOf[ApexComponentMember].getBody
    def getContentEntityId: String = this.asInstanceOf[ApexComponentMember].getContentEntityId
    def getMetadataContainerId: String= this.asInstanceOf[ApexComponentMember].getMetadataContainerId
    def setContentEntityId(contentEntityId: String) {this.asInstanceOf[ApexComponentMember].setContentEntityId(contentEntityId)}
    def setBody(body: String) {this.asInstanceOf[ApexComponentMember].setBody(body)}
    def setMetadataContainerId(metadataContainerId: String) {this.asInstanceOf[ApexComponentMember].setMetadataContainerId(metadataContainerId)}
}

