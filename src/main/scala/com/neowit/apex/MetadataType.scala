/*
 * Copyright (c) 2014 Andrey Gavrikov.
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

package com.neowit.apex

import com.sforce.soap.partner.sobject.SObject
import com.neowit.utils.{FileUtils, Config, ZuluTime, Logging}
import java.util.{Calendar, TimeZone}
import java.io.File
import com.sforce.soap.metadata.{DeployResult, DeployMessage}

object MetadataType extends Logging {
    val LOCAL_MILLS = "LocalMills"
    val MD5 = "md5"
    val CRC32 = "crc32"
    import com.sforce.soap.metadata.FileProperties


    def getKey(props: FileProperties): String = {
        props.getFileName
    }
    private def getName(props: FileProperties): String = {
        val fName = new File(props.getFileName).getName
        fName
    }

    def getValueMap(props: FileProperties, localMills: Long, md5Hash: String, crc32: Long,
                    metaMills: Long, metaMd5Hash: String, metaCRC32: Long):Map[String, Any] = {
        /*
        val lastModifiedDateText = getLastModifiedDateText(props)
        val lastModifiedDateMills = getLastModifiedDateMills(props)
        val data = Map("Id" -> props.getId, "Type" -> props.getType , "Name" -> getName(props), "LastModifiedDate" -> lastModifiedDateText,
            "LastModifiedDateMills" -> lastModifiedDateMills, LOCAL_MILLS -> localMills, MD5 -> md5Hash, CRC32 -> crc32)
        if (metaMills > 0)
            Map("meta" + LOCAL_MILLS -> metaMills, "meta" + MD5 -> metaMd5Hash, "meta" + CRC32 -> metaCRC32) ++ data
        else
            data
        */
        getValueMap(getName(props), props.getType, Some(props.getId), props.getLastModifiedDate, localMills, md5Hash, crc32, metaMills, metaMd5Hash, metaCRC32)
    }

    def getValueMap(deployResult: DeployResult, message: DeployMessage, xmlType: String, localMills: Long, md5Hash: String, crc32: Long,
                    metaMills: Long, metaMd5Hash: String, metaCRC32: Long):Map[String, Any] = {
        val id = if (null == message.getId) None else Some(message.getId)
        getValueMap(message.getFullName, xmlType, id, deployResult.getLastModifiedDate, localMills, md5Hash, crc32, metaMills, metaMd5Hash, metaCRC32)
    }

    def getValueMap(config: Config, file: File, xmlType: String, id: Option[String], lastModifiedDate: Calendar, fileMeta: Option[File] ):Map[String, Any] = {
        val calculateMD5 = config.useMD5Hash
        val calculateCRC32 = !calculateMD5  //by default use only CRC32

        val localMills = file.lastModified()
        val md5Hash = if (calculateMD5) FileUtils.getMD5Hash(file) else ""
        val crc32Hash = if (calculateCRC32) FileUtils.getCRC32Hash(file) else -1L
        val (metaLocalMills: Long, metaMD5Hash: String, metaCRC32Hash:Long) = fileMeta match {
            case Some(fMeta) if fMeta.canRead =>
                (   fMeta.lastModified(),
                    if (calculateMD5) FileUtils.getMD5Hash(fMeta) else "",
                    if (calculateCRC32) FileUtils.getCRC32Hash(fMeta) else -1L
                )
            case None => (-1L, "", -1L)
        }
        getValueMap(file.getName, xmlType, id, lastModifiedDate, localMills, md5Hash, crc32Hash, metaLocalMills, metaMD5Hash, metaCRC32Hash)
    }
    def getValueMap(fileName: String, xmlType: String, id: Option[String], lastModifiedDate: Calendar, localMills: Long,
                    md5Hash: String, crc32: Long, metaMills: Long = -1L, metaMd5Hash: String = "", metaCRC32: Long = -1L):Map[String, Any] = {
        val lastModifiedDateMills = lastModifiedDate.getTime.getTime
        val idVal = id match {
          case Some(s) => Map("Id" -> s)
          case None => Map()
        }
        val data = Map("Type" -> xmlType, "Name" -> fileName, "LastModifiedDate" -> ZuluTime.formatDateGMT(lastModifiedDate),
            "LastModifiedDateMills" -> lastModifiedDateMills, LOCAL_MILLS -> localMills, MD5 -> md5Hash, CRC32 -> crc32) ++ idVal

        if (metaMills > 0)
            Map("meta" + LOCAL_MILLS -> metaMills, "meta" + MD5 -> metaMd5Hash, "meta" + CRC32 -> metaCRC32) ++ data
        else
            data
    }

    def getLastModifiedDateText(props: FileProperties) = {
        //2013-09-25T09:31:08.000Z - simulate output from datetime returned by SOQL query
        val lastModifiedDate = ZuluTime.formatDateGMT(props.getLastModifiedDate)
        lastModifiedDate
    }
    def getLastModifiedDateMills(props: FileProperties) = {
        val lastModifiedDate = props.getLastModifiedDate.getTime.getTime
        lastModifiedDate
    }

}
class MetadataType(val xmlName: String) extends Logging{

    def getLastModifiedDate(obj: SObject): String = {
        obj.getField("LastModifiedDate") match {
            case d if null != d => d.toString
            case _ => "1970-01-01T00:00:00.000Z"
        }
    }

    /*
    def getValueMap(obj: SObject):Map[String, String] = {
        val lastModifiedDate = getLastModifiedDate(obj).toString
        obj.getType match {
            case "StaticResource" =>
                Map("Id" -> obj.getId, "Name" -> obj.getField("Name").toString, "LastModifiedDate" -> lastModifiedDate)
            case _ =>
                Map("Id" -> obj.getId, "Name" -> obj.getField("Name").toString, "ApiVersion" -> obj.getField("ApiVersion").toString, "LastModifiedDate" -> lastModifiedDate)
        }
    }
    */
}

