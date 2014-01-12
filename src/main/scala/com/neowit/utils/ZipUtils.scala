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
import java.util.zip.{ZipInputStream, ZipEntry, ZipOutputStream}
import java.nio.channels.{ReadableByteChannel, Channels, WritableByteChannel, FileChannel}
import java.nio.ByteBuffer

object ZipUtils extends Logging{

    /**
     * list all file names in the given archive
     * @param zip - zip archive to list file names from
     * @return list of file names (with path inside zip archive)
     */
    def listContent(zip: File): List[String] = {
        val zin = new ZipInputStream(new FileInputStream(zip))

        var entry = zin.getNextEntry
        val res = scala.collection.mutable.MutableList[String]()
        while (entry != null) {
            val name = entry.getName
            entry = zin.getNextEntry
            res += name
        }
        zin.close()
        res.toList
    }


    /**
     * zip provided file or folder
     * @param fPath using provided path as file or top of the file tree zip everything into outputZipPath
     * @param outputZipPath - where to store the result
     */
    def zipDir(fPath: String, outputZipPath: String) {

        val bos: ByteArrayOutputStream = new ByteArrayOutputStream
        val zos: ZipOutputStream = new ZipOutputStream(bos)
        //zos.setLevel(9)
        zipFiles("", Array[File](new File(fPath)), zos)
        zos.close()
        //dump to file
        val outputStream = new FileOutputStream (outputZipPath)
        bos.writeTo(outputStream)
        outputStream.close()
        //bos.toByteArray
    }

    /**
     * extract content of zipFile into specified outputFolderPath
     * @param zipFile - zip file
     * @param outputFolder - folder to extract to
     */
    def extract(zipFile: File, outputFolder: File):Map[String, Long] = {
        if (!outputFolder.exists()) {
            outputFolder.mkdirs()
        }
        val zin = new ZipInputStream(new FileInputStream(zipFile))

        var entry = zin.getNextEntry
        val fileMap = collection.mutable.HashMap[String, Long]()
        while (entry != null) {
            val fileName = entry.getName
            val newFile = new File(outputFolder, fileName)
            //create all missing folders
            if (entry.isDirectory)
                newFile.mkdirs()
            else {
                new File(newFile.getParent).mkdirs()
                val fos = new FileOutputStream(newFile)
                transfer(zin, fos, keepInOpen = true)
                fos.close()
                //record local lastModified for future use
                fileMap += fileName -> newFile.lastModified()
            }

            entry = zin.getNextEntry
        }
        zin.close()
        fileMap.toMap
    }

    def zipDirToBytes(rootDir: File): Array[Byte] = {
        val bos: ByteArrayOutputStream = new ByteArrayOutputStream
        val zos: ZipOutputStream = new ZipOutputStream(bos)
        zipFiles("", Array[File](rootDir), zos)
        zos.close()
        bos.toByteArray
    }

    private def zipFiles(relPath: String, files: Array[File], zos: ZipOutputStream) {
        for (file <- files) {
            zipFile(relPath, file, zos)
        }
    }

    private def isIgnored(file: File) = {
        file.getName.startsWith(".") || file.getName.endsWith("~")
    }

    private def zipFile(relPath: String, file: File, zos: ZipOutputStream) {
        if (!isIgnored(file)) {
            val filePath: String = relPath + file.getName
            if (file.isDirectory) {
                val dirPath = filePath + '/'
                val dir = new ZipEntry(dirPath)
                dir.setTime(file.lastModified)
                zos.putNextEntry(dir)
                zos.closeEntry()
                zipFiles(dirPath, file.listFiles, zos)
            } else {
                addFile(filePath, file, zos)
            }
        }
    }

    private def addFile(filename: String, file: File, zos: ZipOutputStream): ZipEntry = {
        val entry: ZipEntry = new ZipEntry(filename)
        entry.setTime(file.lastModified)
        entry.setSize(file.length)
        zos.putNextEntry(entry)
        val is: FileInputStream = new FileInputStream(file)
        try {
            val src: FileChannel = is.getChannel
            val dest: WritableByteChannel = Channels.newChannel(zos)
            copy(src, dest)
            zos.closeEntry()
            entry
        }
        finally {
            is.close()
        }
    }

    private def copy(src: ReadableByteChannel, dest: WritableByteChannel) {
        val buffer: ByteBuffer = ByteBuffer.allocate(8092)
        while (src.read(buffer) != -1) {
            buffer.flip
            while (buffer.hasRemaining) {
                dest.write(buffer)
            }
            buffer.clear
        }
    }

    private def transfer(in: InputStream, out: OutputStream, keepInOpen:Boolean = false) {
        val bytes = new Array[Byte](8092) //8092 bytes - Buffer size
        try {
            Iterator
                .continually (in.read(bytes))
                .takeWhile (-1 !=)
                .foreach (read=> out.write(bytes,0,read))
        }
        finally {
            if (!keepInOpen) {
                in.close()
            }
        }

    }

    /*
    def getBytes(zip: File) = {
        val fos = new FileInputStream(zip)
        val bos = new ByteArrayOutputStream()
        transfer(fos, bos)
        bos.toByteArray
    }
    */
}
