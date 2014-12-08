/*
 * Copyright (c) 2013 Andrey Gavrikov.
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

package com.neowit.utils

import java.io._
import java.util.zip.CRC32
import java.security.MessageDigest
import java.util.regex.Matcher

object FileUtils {

    val regexSafeSeparator = Matcher.quoteReplacement(File.separator)
    /**
     * in order to be used for session 'key' purpose file name must contain unix separator '/' as opposed to Windows one
     * @return turns path\\to\file into path/to/file
     */
    def normalizePath(filePath: String) = filePath.replaceAll(FileUtils.regexSafeSeparator, "/")

    /**
     * using provided folder try to find src/ or unpackaged/ child folder
     * if such a folder found then it is returned as SRC folder
     * @param folder - any folder which may contain a child with name src/ or unpackaged/
     * @return - Some[File] if found src/ or unpackaged/ child
     */
    def findSrcFolder(folder: File): Option[File] = {
        if (folder.isDirectory) {
            val files = folder.listFiles(new FilenameFilter {
                override def accept(dir: File, name: String): Boolean = "unpackaged" == name || "src" == name
            })
            if (files.isEmpty)
                None
            else
                Some(files.head)
        } else {
            None
        }
    }

    /**
     *
     * @param file - path like
     *             /tmp/.../unpackaged/classes/MyClass.cls
     * @param parentNames - List("src", "unpackaged")
     * @return
     *         for example above
     *         /tmp/.../unpackaged
     */
    def getParentByName(file: File, parentNames: Set[String]): Option[File] = {

        if (file.isDirectory) {
            if (parentNames.contains(file.getName)) {
                Some(file)
            } else {
                 if (null != file.getParentFile) {
                     getParentByName(file.getParentFile, parentNames)
                 } else {
                     None //reached the top folder, but required names still not found
                 }
            }
        } else {
            getParentByName(file.getParentFile, parentNames)
        }
    }
    def createTempDir(appConfig: Config): File = {
        val name = appConfig.action
        val outputFolder =
        appConfig.tempFolderPath match {
          case Some(tempFolderPath) =>
              //clean-up
              delete(new File(tempFolderPath + File.separator + name))

              val outputFolder = new File(tempFolderPath + File.separator + name)
              outputFolder.mkdirs()
              outputFolder
          case None => createTempDir(name)
        }

        outputFolder
    }

    def createTempDir(prefix: String): File = {
        val outputFolder = File.createTempFile(prefix, "" + System.nanoTime())
        if (outputFolder.delete()) {
            outputFolder.mkdir()
        } else {
            throw new RuntimeException("Failed to create temp folder. ")
        }
        outputFolder
    }

    def delete(file: File): Boolean = {
        if (file.exists()) {
            if (!file.isDirectory) {
                file.delete()
            } else {
                for (f <- file.listFiles()) {
                    delete(f)
                }
                file.delete()
            }
        }
        //return result
        !file.exists()

    }
    def createTempFile(prefix: String, suffix: String) = {
        File.createTempFile(prefix, suffix)
    }

    /**
     * @return path to temporary file which does not exist
     */
    def getTempFilePath(prefix: String, suffix: String): String = {
        val tempFile = createTempFile(prefix, suffix)
        tempFile.delete()
        tempFile.getAbsolutePath
    }

    private def isIgnored(file: File) = {
        file.getName.startsWith(".") || file.getName.contains("~")
    }

    def getExtension(file: File): String = {
        val extStart = file.getName.lastIndexOf(".")
        if (extStart > 0) {
            //file.getName.substring(extStart + 1)
            file.getName.drop(extStart + 1)
        } else {
            ""
        }
    }
    def removeExtension(file: File): String = {
        val extStart = file.getName.lastIndexOf(".")
        if (extStart > 0) {
            file.getName.take(extStart)
        } else {
            file.getName
        }
    }
    /**
     * a very basic file lister which will cause "out of memory" on a very deep directory tree
     * with lots of files
     * @param dir - top of the file tree
     * @param includeFolders - set to false if list should be single level, not recursive and without nested folders
     * @return list of all files under dir (including dir)
     */
    def listFiles(dir: File, descentIntoFolders: Boolean = true, includeFolders: Boolean = true):List[File] = {
        def listOnelevel(f: File):List[File] = {
            if (f.isDirectory && descentIntoFolders) {
                val files = f.listFiles().filter(ff => ff.canRead && !isIgnored(ff) && (!ff.isDirectory || includeFolders)).flatMap(listOnelevel).toList
                if (includeFolders) f :: files else files
            } else {
                List(f)
            }
        }
        val files = List() ++ listOnelevel(dir)
        files
    }

    /**
     * file copy methods
     */
    def copy(src:File, dest:File) {
        new FileOutputStream(dest).getChannel.transferFrom( new FileInputStream(src).getChannel, 0, Long.MaxValue )
    }
    def copy(srcPath:String, destPath:String) {
        copy(new File(srcPath), new File(destPath))
    }

    def getMD5Hash(file: File): String = {
        val in = new FileInputStream(file)
        val bytes = new Array[Byte](file.length.toInt)
        in.read(bytes)
        in.close()
        getMD5Hash(bytes)
    }

    def getMD5Hash(bytes : Array[Byte]): String = {
        val md5 = MessageDigest.getInstance("MD5")
        md5.reset()
        md5.update(bytes)

        md5.digest().map(0xFF & _).map { "%02x".format(_) }.foldLeft(""){_ + _}
    }


    def getCRC32Hash(file: File): Long = {
        val in = new FileInputStream(file)
        val bytes = new Array[Byte](file.length.toInt)
        in.read(bytes)
        in.close()
        getCRC32Hash(bytes)
    }

    def getCRC32Hash(bytes : Array[Byte]): Long = {
        val crc = new CRC32()
        crc.update(bytes)
        crc.getValue
    }

    def writeFile(text: String, file: File, append: Boolean = false) = {
        val writer = new FileWriter(file, append)
        try{
            writer.write(text)
        } finally{
            writer.close()
        }
    }

    def writeFile(xmlBody: scala.xml.Elem, filePath: String): File = {
        scala.xml.XML.save(filePath, xmlBody, enc = "UTF-8" )
        new File(filePath)
    }
}
