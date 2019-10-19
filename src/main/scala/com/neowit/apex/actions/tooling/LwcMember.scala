/*
 * Copyright (c) 2019 Andrey Gavrikov.
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

import com.neowit.apex.{MetaXml, MetadataType, Session}
import com.neowit.apex.actions.BulkRetrieve
import com.neowit.utils.FileUtils
import com.sforce.soap.metadata.FileProperties
import com.sforce.soap.tooling.sobject.{LightningComponentResource, SObject}

object LwcMemberHelper {
  val helper = new LwcMemberHelper
}
class LwcMemberHelper extends BundleMemberHelper {
  val EXTENSIONS: Set[String] = Set("css", "html", "js", "json", "svg", "xml")
  val XML_TYPE = "LightningComponentResource"
  val BUNDLE_XML_TYPE = "LightningComponentBundle"
  val DIR_NAME = "lwc"


  def getBundleMemberHelper: BundleMemberHelper = LwcMemberHelper.helper

  def isSupportedType(file: File): Boolean = {
    //is this file in "lwc" folder
    FileUtils.getParentByName(file, Set(DIR_NAME)) match {
      case Some(x) if !file.isDirectory =>
        val extension = FileUtils.getExtension(file)
        // make sure to exclude __tests__ folder, it may be present but must not be saved to SFDC
        val res = FileUtils.getParentByName(file, Set("__tests__")).isEmpty &&
                    (EXTENSIONS.contains(extension) || file.getName.endsWith("-meta.xml"))
        res
      case _ => false //not in lwc folder or is a directory
    }
  }

  /**
   * check if this file sits in lwc/something folder
   * @param file - if this file is part of lwc bundle then name of the bundle is returned
   * @return
   */
  def getBundleDir(file: File): Option[File] = {
    //if we have reached one of these dirs then the search is over
    val topLevelDirs = Set("src", "unpackaged", "lwc")

    @scala.annotation.tailrec
    def ascend(file: File): Option[File] = {
      if (null != file && !topLevelDirs.contains(file.getName)) {
        val parentFile = file.getParentFile
        if (null != parentFile && "lwc" == parentFile.getName) {
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

  /*
  def getInstanceForCreate(file: File, session: Session): LightningComponentResourceMember = {
    if (!isSupportedType(file)) {
      throw new UnsupportedTypeForToolingException("File " + file.getName + " is not supported with Tooling API")
    }
    val member = new LightningComponentResourceMember
    member.setSource(file)
    member.setFormat(file)
    //member.setDefType(file)
    val key = session.getKeyByFile(file)
    session.getData(key).get("Id") match {
      case Some(id) =>
        member.setId(id.asInstanceOf[String])
      case None =>
    }
    val bundleKey = session.getKeyByFile(file.getParentFile)
    session.getData(bundleKey).get("Id") match {
      case Some(id) =>
        member.setLightningComponentBundleId(id.asInstanceOf[String])
      case None =>
    }
    member

  }
   */

  def getInstanceUpdate(file: File, session: Session): SObject = {
    if (!isSupportedType(file)) {
      throw new UnsupportedTypeForToolingException("File " + file.getName + " is not supported with Tooling API")
    }
    val member = new LightningComponentResourceMember
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
   * presently the only way of matching local file names to lwc object Ids is via metadata retrieve
   * LwcDefinition objects do not have Names and retrieve seems to be the only truly reliable way of matching local file names to SFDC Ids
   * @param bundledFiles - expect only lwc files here
   * @param idsOnly - set to true if no data must be changed except Ids
   */
  def updateDefinitionData(session: Session, bundledFiles: List[File], idsOnly: Boolean): Map[File, FileProperties] = {
    if (bundledFiles.isEmpty)
      return Map()

    val lwcBundleNames = bundledFiles.map(getBundleDir(_).map(_.getName).get).toSet


    val bulkRetrieve = new BulkRetrieve {
      override protected def isUpdateSessionDataOnSuccess: Boolean = false

      override protected def getTypesFileFormat: String = "packageXml"

      override protected def getSpecificTypesFile: File = {
        val metaXml = new MetaXml(session.getConfig)
        val _package = metaXml.createPackage(config.apiVersion, Map(BUNDLE_XML_TYPE -> lwcBundleNames.toList))
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
    for(file <- bundledFiles) {
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

            val isMetaXml = file.getName.endsWith("-meta.xml")
            val newData = if (!isMetaXml) {
              MetadataType.getValueMap(props, localMills, md5Hash, crc32, metaMills = -1L, metaMd5Hash = "", metaCRC32 = -1L)
            } else {
              MetadataType.getValueMap(props, localMills = -1L, md5Hash = "", crc32 = -1L, metaMills = localMills, metaMd5Hash = md5Hash, metaCRC32 = crc32)
            }

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

class LightningComponentResourceMember extends LightningComponentResource with BundleMember {

  override def getBundleMemberHelper: BundleMemberHelper = LwcMemberHelper.helper

  def setSource(file: File): Unit = {
    //setSource(FileUtils.readFile(file).mkString)
    setSource(FileUtils.readFileAsText(file))
  }

  def setFormat(file: File): Unit = {
    /*
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
    */
    val format = FileUtils.getExtension(file)
    setFormat(format)
  }
}
