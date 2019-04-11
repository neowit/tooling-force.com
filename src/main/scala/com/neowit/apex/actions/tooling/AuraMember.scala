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

import java.io.File

import com.neowit.apex.{MetadataType, MetaXml, Session}
import com.neowit.apex.actions.BulkRetrieve
import com.neowit.utils.FileUtils
import com.sforce.soap.metadata.{FileProperties, AuraDefinitionBundle}
import com.sforce.soap.tooling.sobject.AuraDefinition

object AuraMember {
    val EXTENSIONS: Set[String] = Set("app", "cmp", "evt", "intf", "js", "css", "auradoc", "tokens", "design", "svg")
    val XML_TYPE = "AuraDefinition"
    val BUNDLE_XML_TYPE = "AuraDefinitionBundle"
    def isSupportedType(file: File): Boolean = {
        //is this file in "aura" folder
        FileUtils.getParentByName(file, Set("aura")) match {
            case Some(x) if !file.isDirectory =>
                val extension = FileUtils.getExtension(file)
                EXTENSIONS.contains(extension) || file.getName.endsWith("-meta.xml")
            case _ => false //not in aura folder or is a directory
        }
    }

    /**
     * check if this file sits in aura/something folder
     * @param file - if this file is part of aura bundle then name of the bundle is returned
     * @return
     */
    def getAuraBundleDir(file: File): Option[File] = {
        //if we have reached one of these dirs then the search is over
        val topLevelDirs = Set("src", "unpackaged", "aura")

        def ascend(file: File): Option[File] = {
           if (null != file && !topLevelDirs.contains(file.getName)) {
               val parentFile = file.getParentFile
               if (null != parentFile && "aura" == parentFile.getName) {
                   Some(file)
               } else {
                   ascend(file.getParentFile)
               }
           } else {
               None
           }
        }
        ascend(file)
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
    def updateAuraDefinitionData(session: Session, auraFiles: List[File], idsOnly: Boolean): Map[File, FileProperties] = {
        if (auraFiles.isEmpty)
            return Map()

        val auraBundleNames = auraFiles.map(AuraMember.getAuraBundleDir(_).map(_.getName).get).toSet


        val bulkRetrieve = new BulkRetrieve {
            override protected def isUpdateSessionDataOnSuccess: Boolean = false

            override protected def getTypesFileFormat: String = "packageXml"

            override protected def getSpecificTypesFile: File = {
                val metaXml = new MetaXml(session.getConfig)
                val _package = metaXml.createPackage(config.apiVersion, Map(AuraMember.BUNDLE_XML_TYPE -> auraBundleNames.toList))
                val packageXml = metaXml.packageToXml(_package)
                val tempFilePath = FileUtils.getTempFilePath("package", "xml")
                val file = new File(tempFilePath)
                FileUtils.writeFile(packageXml, file)
                file
            }
        }
        bulkRetrieve.load[BulkRetrieve](session)

        val tempFolder =  FileUtils.createTempDir(session.config)
        val bulkRetrieveResult = bulkRetrieve.doRetrieve(tempFolder)
        val calculateMD5 = session.config.useMD5Hash

        val conflictingFiles = Map.newBuilder[File, FileProperties]
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
                        val newData = MetadataType.getValueMap(props, localMills, md5Hash, crc32,
                                                                metaMills = -1L, metaMd5Hash = "", metaCRC32 = -1L)

                        val millsLocal = session.getData(key).getOrElse("LastModifiedDateMills", 0).toString.toLong
                        val millsRemote = MetadataType.getLastModifiedDateMills(props)
                        if (millsRemote > millsLocal) {
                            conflictingFiles += file -> props
                        }
                        session.setData(key, data ++ newData)
                    }
                case _ => //this file has not been returned, assume it has been deleted
                    session.removeData(key)
            }
        }
        conflictingFiles.result()
    }
}

trait AuraMember {

}
class AuraDefinitionMember extends AuraDefinition with AuraMember {
    def setSource(file: File): Unit = {
        setSource(FileUtils.readFile(file).mkString)
    }

    def setFormat(file: File): Unit = {
        val fName = file.getName.toLowerCase
        val format = fName.toLowerCase match {
            case "application.app" => "XML"
            case "component.cmp" => "XML"
            case "event.evt" => "XML"
            case x if x.endsWith(".auradoc") => "XML"
            case x if x.endsWith(".design") => "XML"
            case x if x.endsWith(".intf") => "XML"
            case x if x.endsWith(".js") => "JS"
            case x if x.endsWith(".css") => "CSS"
            case x if x.endsWith(".tokens") => "XML"
        }
        setFormat(format)
    }
    def setDefType(file: File): Unit = {
        val fName = file.getName.toLowerCase
        val defType = fName.toLowerCase match {
            case x if x.endsWith(".app") => "APPLICATION"
            case x if x.endsWith(".cmp") => "COMPONENT"
            case x if x.endsWith(".design") => "DESIGN"
            case x if x.endsWith(".intf") => "INTERFACE"
            case x if x.endsWith(".evt") => "EVENT"
            case x if x.endsWith(".auradoc") => "DOCUMENTATION"
            case x if x.endsWith(".css") => "STYLE"
            case x if x.endsWith("controller.js") => "CONTROLLER"
            case x if x.endsWith("helper.js") => "HELPER"
            case x if x.endsWith("renderer.js") => "RENDERER"
            case x if x.endsWith(".tokens") => "TOKENS"
            case x if x.endsWith(".svg") => "SVG"
            case _ => throw new IllegalArgumentException("Failed to determine aura definition for file: " + file.getName )
        }
        setDefType(defType)
    }
}
class AuraBundleMember extends AuraDefinitionBundle {
    
}
