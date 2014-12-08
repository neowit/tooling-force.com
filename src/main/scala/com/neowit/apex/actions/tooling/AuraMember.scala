package com.neowit.apex.actions.tooling

import java.io.File

import com.neowit.apex.{MetadataType, MetaXml, Session}
import com.neowit.apex.actions.BulkRetrieve
import com.neowit.utils.FileUtils
import com.sforce.soap.metadata.AuraDefinitionBundle
import com.sforce.soap.tooling.AuraDefinition

object AuraMember {
    val EXTENSIONS = Set("app", "cmp", "evt", "intf", "js", "css", "auradoc")
    val XML_TYPE = "AuraDefinition"
    val BUNDLE_XML_TYPE = "AuraDefinitionBundle"
    def isSupportedType(file: File): Boolean = {
        //is this file in "aura" folder
        FileUtils.getParentByName(file, Set("aura")) match {
            case Some(x) =>
                val extension = FileUtils.getExtension(file)
                EXTENSIONS.contains(extension)
            case None => false //not in aura folder
        }
    }

    /**
     * @param file - if this file is part of aura bundle then name of the bundle is returned
     * @return
     */
    def getAuraBundleDir(file: File): Option[File] = {
        var currentDir = if (file.isDirectory) file else file.getParentFile
        var dir:Option[File] = None

        while (null != currentDir && "aura" != currentDir.getName) {
            dir = Some(currentDir)
            currentDir = currentDir.getParentFile
        }
        dir
    }

    def getInstanceForCreate(file: File, session: Session): AuraDefinitionMember = {
        if (!isSupportedType(file)) {
            throw new UnsupportedTypeForToolingException("File " + file.getName + " is not supported with Tooling API")
        }
        val member = new AuraDefinitionMember
        member.setSource(file)
        member.setFormat(file)
        member.setDefType(file)
        val key = session.getKeyByFile(file)
        session.getData(key).get("Id") match {
            case Some(id) =>
                member.setId(id.asInstanceOf[String])
            case None =>
        }
        val bundleKey = session.getKeyByFile(file.getParentFile)
        session.getData(bundleKey).get("Id") match {
            case Some(id) =>
                member.setAuraDefinitionBundleId(id.asInstanceOf[String])
            case None =>
        }
        member

    }
    def getInstanceUpdate(file: File, session: Session): AuraDefinitionMember = {
        if (!isSupportedType(file)) {
            throw new UnsupportedTypeForToolingException("File " + file.getName + " is not supported with Tooling API")
        }
        val member = new AuraDefinitionMember
        member.setSource(file)
        val key = session.getKeyByFile(file)
        session.getData(key).get("Id") match {
            case Some(id) =>
                member.setId(id.asInstanceOf[String])
            case None =>
        }
        member

    }
    /**
     * presently the only way of matching local file names to aura object Ids is via metadata retrieve
     * AuraDefinition objects do not have Names and retrieve seems to be the only truly reliable way of matching local file names to SFDC Ids
     * @param auraFiles - expect only aura files here
     * @param idsOnly - set to true if no data must be changed except Ids
     */
    def updateAuraDefinitionData(session: Session, auraFiles: List[File], idsOnly: Boolean): Unit = {
        if (auraFiles.isEmpty)
            return

        val auraFilesByBundleName = auraFiles.groupBy(f => {
            AuraMember.getAuraBundleDir(f).map(_.getName).getOrElse("")
        } ).filterNot(_._1.isEmpty)

        val tempFolder =  FileUtils.createTempDir(session.config)
        val bulkRetrieve = new BulkRetrieve {
            override protected def isUpdateSessionDataOnSuccess: Boolean = false

            override protected def getTypesFileFormat: String = "packageXml"

            override protected def getSpecificTypesFile: File = {
                val metaXml = new MetaXml(session.getConfig)
                val _package = metaXml.createPackage(config.apiVersion, Map(AuraMember.BUNDLE_XML_TYPE -> auraFilesByBundleName.keys.toList))
                val packageXml = metaXml.packageToXml(_package)
                val tempFile = FileUtils.createTempFile("package", "xml")
                tempFile.delete()
                scala.xml.XML.save(tempFile.getAbsolutePath, packageXml, enc = "UTF-8" )
                new File(tempFile.getAbsolutePath)
            }
        }
        bulkRetrieve.load[BulkRetrieve](session.basicConfig)

        val bulkRetrieveResult = bulkRetrieve.doRetrieve(tempFolder)
        val calculateMD5 = session.config.useMD5Hash

        for(file <- auraFiles) {
            val key = session.getKeyByFile(file)
            bulkRetrieveResult.getFileProps(key) match {
                case Some(props) if null != props.getId =>
                    val data = session.getData(key)
                    if (idsOnly) {
                        val newData = Map("Id" -> props.getId)
                        session.setData(key, data ++ newData)
                    } else {
                        val localMills: Long = file.lastModified()
                        val (md5Hash: String, crc32: Long) = if (calculateMD5) (FileUtils.getMD5Hash(file), -1L) else ("", FileUtils.getCRC32Hash(file))
                        val metaMills: Long = -1L
                        val metaMd5Hash: String = ""
                        val metaCRC32: Long = -1L
                        val newData = MetadataType.getValueMap(props, localMills, md5Hash, crc32,
                            metaMills, metaMd5Hash, metaCRC32)
                        session.setData(key, data ++ newData)
                    }
                case _ => //this file has not been returned, assume it has been deleted
                    session.removeData(key)
            }
        }
    }
}

trait AuraMember {

}
class AuraDefinitionMember extends AuraDefinition with AuraMember {
    def setSource(file: File) {
        setSource(scala.io.Source.fromFile(file).mkString)
    }

    def setFormat(file: File): Unit = {
        val fName = file.getName.toLowerCase
        val format = fName.toLowerCase match {
            case "application.app" => "XML"
            case "component.cmp" => "XML"
            case "event.evt" => "XML"
            case x if x.endsWith(".auradoc") => "XML"
            case x if x.endsWith(".js") => "JS"
            case x if x.endsWith(".css") => "CSS"
        }
        setFormat(format)
    }
    def setDefType(file: File): Unit = {
        val fName = file.getName.toLowerCase
        val defType = fName.toLowerCase match {
            case x if x.endsWith(".app") => "APPLICATION"
            case x if x.endsWith(".cmp") => "COMPONENT"
            case x if x.endsWith(".evt") => "EVENT"
            case x if x.endsWith(".auradoc") => "DOCUMENTATION"
            case x if x.endsWith(".css") => "STYLE"
            case x if x.endsWith("controller.js") => "CONTROLLER"
            case x if x.endsWith("helper.js") => "HELPER"
            case x if x.endsWith("renderer.js") => "RENDERER"
            case _ => throw new IllegalArgumentException("Failed to determine aura definition for file: " + file.getName )
        }
        setDefType(defType)
    }
}
class AuraBundleMember extends AuraDefinitionBundle {
    
}
