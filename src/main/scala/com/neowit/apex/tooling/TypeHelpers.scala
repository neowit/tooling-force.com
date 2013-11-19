package com.neowit.apex.tooling

import com.sforce.soap.tooling._
import java.io._
import scala.Some
import scala.sys.process.BasicIO
import scala.util.parsing.json.JSONObject
import com.neowit.utils.{Config, Logging}
import com.neowit.apex.session.SessionData

trait TypeHelper extends Logging {
    def API_VERSION = Config.getConfig.apiVersion //default API version
    def getValueMap(obj: SObject):Map[String, String] = {
        val lastModifiedDate = getLastModifiedDate(obj).toString
        Map("Id" -> obj.getId, "Name" -> getName(obj), "ApiVersion" -> getApiVersion(obj).toString, "LastModifiedDate" -> lastModifiedDate)
    }
    val typeName: String
    val fileExtension: String
    val directoryName: String
    def getApiVersion(obj: SObject): Double = {
        getApiVersionImpl(obj)match {
          case x:Double if x > 0 => x
          case _ => API_VERSION
        }
    }
    def getApiVersion(file: File) = {
        val metaXmlFile = new File(file.getAbsolutePath + "-meta.xml")
        val apiVersion =
            if (metaXmlFile.exists()) {
                val metaXml = xml.XML.loadFile(metaXmlFile)
                val apiVersionNode = metaXml \\ "apiVersion"
                apiVersionNode.text.toDouble
            } else API_VERSION
        apiVersion
    }
    protected def getApiVersionImpl(obj: SObject): Double
    //name without extension
    def getName(f: File): String = stripExtension(f.getName)

    def getName(obj: SObject): String
    def getBody(obj: SObject): String
    def getBody(resourcePath: String): String = {
        scala.io.Source.fromFile(resourcePath).mkString
    }
    def getLabel(obj: SObject): String
    def getDescription(obj: SObject): String
    def getLastModifiedDate(obj: SObject): Long = {
        getLastModifiedDateImpl(obj) match {
            case d if null != d => d.getTimeInMillis
            case _ => 0
        }
    }
    protected def getLastModifiedDateImpl(obj: SObject): java.util.Calendar

    def newMemberInstance: Member
    def newSObjectInstance(file: File): SObject
    val filenameFilter: FilenameFilter = new FilenameFilter {
        def accept(p1: File, p2: String): Boolean = p2.endsWith(fileExtension)
    }

    def unapply(resource: File): Option[(String, String)] = {
        if (resource.getName.endsWith(fileExtension)) {
            val nameWithExt = resource.getName
            Option((stripExtension(nameWithExt), fileExtension))
        } else {
            None
        }
    }
    def unapply(resource: SObject): Option[(String, String)] = {
        if (resource.getClass.getSimpleName == typeName) {
            val name = getName(resource)
            Option(name, fileExtension)
        } else {
            None
        }
    }
    def unapply(json: JSONObject): Option[(String, String)] = {
        if (json.obj("extent") == this.typeName) {
            val name = json.obj("name").asInstanceOf[String]
            Option(name, fileExtension)
        } else {
            None
        }
    }
    protected def stripExtension(fileName: String) = (fileExtension + "$").r.replaceFirstIn(fileName, "")

    def getKey(obj: SObject): String = {typeName + "." + getName(obj)}
    def getKey(f: File): String = {typeName + "." + getName(f)}
    def getKey(name: String): String = {typeName + "." + stripExtension(name)}

    protected def getContentSoqlWhere: String = ""
    protected def getContentSOQL: String
    def getContentSOQL(whereOverrideStr: String = ""): String = getContentSOQL + " " + (whereOverrideStr  match {
        case x if x.isEmpty => getContentSoqlWhere
        case _ => whereOverrideStr
    })
    def isHiddenBody(record:SObject) = false

    def getMemberInstance(sessionData: SessionData, f: File) = {
        val metadataContainerId = sessionData.getField("MetadataContainer", "Id") match {
          case Some(x) => x
          case None => throw new IllegalStateException("Missing MetadataContainer.Id in session data")
        }
        val key = getKey(f)
        val objectId = sessionData.getField(key, "Id").get
        val member = newMemberInstance
        member.setContentEntityId(objectId)
        member.setBody(getBody(f))
        member.setMetadataContainerId(metadataContainerId)
        // DEBUG
        //member.getSObject.setId(sessionData.getField(key, "ApexClassMemberId").get)
        // END DEBUG
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

    def getMetaXml(record:SObject): String

    def bodyToFile(appConfig: Config, record: SObject) = {
        val buffer = getBody(record).getBytes
        if (buffer.length > 0) {
            val fileName = getName(record) + fileExtension
            val file = new File(appConfig.mkdirs(directoryName) + File.separator + fileName)
            try {
                val output = new FileOutputStream(file)
                output.write(buffer)
                output.close()
                //generate meta-xml
                val meta = getMetaXml(record)
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
    val list = List(new ClassHelper(), new TriggerHelper(), new PageHelper(), new ComponentHelper())

    def getHelper(resource: File): TypeHelper = resource match {
        case ClassHelper(name, ext) => new ClassHelper
        case TriggerHelper(name, ext) => new TriggerHelper
        case PageHelper(name, ext) => new PageHelper
        case ComponentHelper(name, ext) => new ComponentHelper
        case _ => throw new UnsupportedOperationException("Not implemented yet for resourcePath=" + resource.getAbsolutePath)
    }
    def getHelper(resource: SObject): TypeHelper = resource match {
        case ClassHelper(name, ext) => new ClassHelper
        case TriggerHelper(name, ext) => new TriggerHelper
        case PageHelper(name, ext) => new PageHelper
        case ComponentHelper(name, ext) => new ComponentHelper
        case _ => throw new UnsupportedOperationException("Not implemented yet for resource=" + resource.getClass.getName)
    }

    def getHelper(json: JSONObject): TypeHelper = json match {
        case ClassHelper(name, ext) => new ClassHelper
        case TriggerHelper(name, ext) => new TriggerHelper
        case PageHelper(name, ext) => new PageHelper
        case ComponentHelper(name, ext) => new ComponentHelper
        case _ => throw new UnsupportedOperationException("Not implemented yet for resource=" + json)
    }

    object ClassHelper {
        def unapply(resource: File): Option[(String, String)] = new ClassHelper().unapply(resource)
        def unapply(resource: SObject): Option[(String, String)] = new ClassHelper().unapply(resource)
        def unapply(json: JSONObject): Option[(String, String)] = new ClassHelper().unapply(json)
    }
    class ClassHelper extends TypeHelper {
        val typeName: String = "ApexClass"
        val directoryName: String = "classes"
        val fileExtension: String = ".cls"
        def getApiVersionImpl(obj: SObject): Double = obj.asInstanceOf[ApexClass].getApiVersion
        def getName(obj: SObject): String = obj.asInstanceOf[ApexClass].getName
        def getBody(obj: SObject): String = obj.asInstanceOf[ApexClass].getBody
        def getLabel(obj: SObject): String = getName(obj)
        def getDescription(obj: SObject): String = ""
        protected def getLastModifiedDateImpl(obj: SObject) = obj.asInstanceOf[ApexClass].getLastModifiedDate

        def newMemberInstance: Member = Member.toMember(new ApexClassMember())
        def newSObjectInstance(file: File): SObject = {
            val obj = new ApexClass()
            obj.setBody(getBody(file))
            obj.setName(stripExtension(file.getName))
            obj.setApiVersion(getApiVersion(file))
            obj
        }
        override protected def getContentSoqlWhere: String = "where BodyCrc > 0" //exclude hidden bodies of managed classes
        protected def getContentSOQL: String = "select Id, Name, ApiVersion, LastModifiedDate, Body, Status, BodyCrc, LengthWithoutComments from " + typeName

        //in theory we should filter hidden classes as "where LengthWithoutComments  > 0" but this does not work, SFDC ignores this SOQL condition
        override def isHiddenBody(obj:SObject) = obj.asInstanceOf[ApexClass].getLengthWithoutComments <= 0

        def getMetaXml(record:SObject) = {
            val status = record.asInstanceOf[ApexClass].getStatus
            val meta =
                s"""|<?xml version="1.0" encoding="UTF-8"?>
                        |<$typeName xmlns="http://soap.sforce.com/2006/04/metadata">
                        |    <apiVersion>${getApiVersion(record)}</apiVersion>
                        |    <status>$status</status>
                        |</$typeName>""".stripMargin
            meta
        }

    }

    object TriggerHelper {
        def unapply(resource: File): Option[(String, String)] = new TriggerHelper().unapply(resource)
        def unapply(resource: SObject): Option[(String, String)] = new TriggerHelper().unapply(resource)
        def unapply(json: JSONObject): Option[(String, String)] = new ClassHelper().unapply(json)
    }
    class TriggerHelper extends TypeHelper {
        val typeName: String = "ApexTrigger"
        val directoryName: String = "triggers"
        val fileExtension: String = ".trigger"
        def getApiVersionImpl(obj: SObject): Double = obj.asInstanceOf[ApexTrigger].getApiVersion
        def getName(obj: SObject): String = obj.asInstanceOf[ApexTrigger].getName
        def getBody(obj: SObject): String = obj.asInstanceOf[ApexTrigger].getBody
        def getLabel(obj: SObject): String = getName(obj)
        def getDescription(obj: SObject): String = ""
        protected def getLastModifiedDateImpl(obj: SObject) = obj.asInstanceOf[ApexTrigger].getLastModifiedDate

        def newMemberInstance: Member = Member.toMember(new ApexTriggerMember())
        def newSObjectInstance(file: File): SObject = {
            val obj = new ApexTrigger()
            obj.setBody(getBody(file))
            obj.setName(stripExtension(file.getName))
            obj.setApiVersion(getApiVersion(file))
            obj
        }

        override protected def getContentSoqlWhere: String = "where BodyCrc > 0" //exclude hidden bodies of managed classes
        protected def getContentSOQL: String = "select Id, Name, ApiVersion, LastModifiedDate, Body, Status, BodyCrc, LengthWithoutComments from " + typeName

        //in theory we should filter hidden classes as "where [BodyCrc / LengthWithoutComments]  > 0" but this does not work, SFDC ignores this SOQL condition
        override def isHiddenBody(obj:SObject) = obj.asInstanceOf[ApexTrigger].getLengthWithoutComments <= 0

        def getMetaXml(record:SObject) = {
            val status = record.asInstanceOf[ApexTrigger].getStatus
            val meta =
                s"""|<?xml version="1.0" encoding="UTF-8"?>
                        |<$typeName xmlns="http://soap.sforce.com/2006/04/metadata">
                        |    <apiVersion>${getApiVersion(record)}</apiVersion>
                        |    <status>$status</status>
                        |</$typeName>""".stripMargin
            meta
        }

    }

    object PageHelper {
        def unapply(resource: File): Option[(String, String)] = new PageHelper().unapply(resource)
        def unapply(resource: SObject): Option[(String, String)] = new PageHelper().unapply(resource)
        def unapply(json: JSONObject): Option[(String, String)] = new ClassHelper().unapply(json)
    }
    class PageHelper extends TypeHelper {
        val typeName: String = "ApexPage"
        val directoryName: String = "pages"
        val fileExtension: String = ".page"
        def getApiVersionImpl(obj: SObject): Double = obj.asInstanceOf[ApexPage].getApiVersion
        def getName(obj: SObject): String = obj.asInstanceOf[ApexPage].getName
        def getBody(obj: SObject): String = obj.asInstanceOf[ApexPage].getMarkup
        def getLabel(obj: SObject): String = obj.asInstanceOf[ApexPage].getMasterLabel
        def getDescription(obj: SObject): String = obj.asInstanceOf[ApexPage].getDescription
        protected def getLastModifiedDateImpl(obj: SObject) = obj.asInstanceOf[ApexPage].getLastModifiedDate

        def newMemberInstance: Member = Member.toMember(new ApexPageMember())
        def newSObjectInstance(file: File): SObject = {
            val obj = new ApexPage()
            obj.setMarkup(getBody(file))
            obj.setName(stripExtension(file.getName))
            obj.setApiVersion(getApiVersion(file))
            obj.setMasterLabel(obj.getName)
            obj
        }
        protected def getContentSOQL: String = "select Id, Name, ApiVersion, LastModifiedDate, Markup, MasterLabel, Description from " + typeName

        def getMetaXml(record:SObject) = {
            val meta =
                s"""|<?xml version="1.0" encoding="UTF-8"?>
                        |<$typeName xmlns="http://soap.sforce.com/2006/04/metadata">
                        |    <apiVersion>${getApiVersion(record)}</apiVersion>
                        |    <description>${getDescription(record)}</description>
                        |    <label>${getLabel(record)}</label>
                        |</$typeName>""".stripMargin
            meta
        }
    }

    object ComponentHelper {
        def unapply(resource: File): Option[(String, String)] = new ComponentHelper().unapply(resource)
        def unapply(resource: SObject): Option[(String, String)] = new ComponentHelper().unapply(resource)
        def unapply(json: JSONObject): Option[(String, String)] = new ClassHelper().unapply(json)
    }
    class ComponentHelper extends TypeHelper {
        val typeName: String = "ApexComponent"
        val directoryName: String = "components"
        val fileExtension: String = ".component"
        def getApiVersionImpl(obj: SObject): Double = obj.asInstanceOf[ApexComponent].getApiVersion
        def getName(obj: SObject): String = obj.asInstanceOf[ApexComponent].getName
        def getBody(obj: SObject): String = obj.asInstanceOf[ApexComponent].getMarkup
        def getLabel(obj: SObject): String = obj.asInstanceOf[ApexComponent].getMasterLabel
        def getDescription(obj: SObject): String = obj.asInstanceOf[ApexComponent].getDescription
        protected def getLastModifiedDateImpl(obj: SObject) = obj.asInstanceOf[ApexComponent].getLastModifiedDate

        def newMemberInstance: Member = Member.toMember(new ApexComponentMember())
        def newSObjectInstance(file: File): SObject = {
            val obj = new ApexComponent()
            obj.setMarkup(getBody(file))
            obj.setName(stripExtension(file.getName))
            obj.setApiVersion(getApiVersion(file))
            obj.setMasterLabel(obj.getName)
            obj
        }
        protected def getContentSOQL: String = "select Id, Name, ApiVersion, LastModifiedDate, Markup, MasterLabel, Description from " + typeName

        def getMetaXml(record:SObject) = {
            val meta =
                s"""|<?xml version="1.0" encoding="UTF-8"?>
                        |<$typeName xmlns="http://soap.sforce.com/2006/04/metadata">
                        |    <apiVersion>${getApiVersion(record)}</apiVersion>
                        |    <label>${getLabel(record)}</label>
                        |</$typeName>""".stripMargin
            meta
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
    def getSObject: SObject = m

    def getBody: String = m.getBody
    def getContentEntityId: String = m.getContentEntityId
    def getMetadataContainerId: String= m.getMetadataContainerId
    def setContentEntityId(contentEntityId: String) {m.setContentEntityId(contentEntityId)}
    def setBody(body: String) {m.setBody(body)}
    def setMetadataContainerId(metadataContainerId: String) {m.setMetadataContainerId(metadataContainerId)}
}
class MemberPage(m: ApexPageMember) extends Member {
    def getSObject: SObject = m

    def getBody: String = m.getBody
    def getContentEntityId: String = m.getContentEntityId
    def getMetadataContainerId: String= m.getMetadataContainerId
    def setContentEntityId(contentEntityId: String) {m.setContentEntityId(contentEntityId)}
    def setBody(body: String) {m.setBody(body)}
    def setMetadataContainerId(metadataContainerId: String) {m.setMetadataContainerId(metadataContainerId)}
}
class MemberTrigger(m: ApexTriggerMember) extends Member {
    def getSObject: SObject = m

    def getBody: String = m.getBody
    def getContentEntityId: String = m.getContentEntityId
    def getMetadataContainerId: String= m.getMetadataContainerId
    def setContentEntityId(contentEntityId: String) {m.setContentEntityId(contentEntityId)}
    def setBody(body: String) {m.setBody(body)}
    def setMetadataContainerId(metadataContainerId: String) {m.setMetadataContainerId(metadataContainerId)}
}
class MemberComponent(m: ApexComponentMember) extends Member {
    def getSObject: SObject = m

    def getBody: String = m.getBody
    def getContentEntityId: String = m.getContentEntityId
    def getMetadataContainerId: String= m.getMetadataContainerId
    def setContentEntityId(contentEntityId: String) {m.setContentEntityId(contentEntityId)}
    def setBody(body: String) {m.setBody(body)}
    def setMetadataContainerId(metadataContainerId: String) {m.setMetadataContainerId(metadataContainerId)}
}

