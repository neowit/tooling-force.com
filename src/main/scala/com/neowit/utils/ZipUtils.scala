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

class ZipUtils extends Logging{

    def getBytes(zip: File) = {
        io.Source.fromFile(zip).toArray[Byte]
    }
    /**
     * list all file names in the given archive
     * @param zip - zip archive to list file names from
     * @return list of file names (with path inside zip archive)
     */
    def listArhiveContent(zip: File): List[String] = {
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
     * add a bunch of files to the root of EXISTING zip folder
     * @param files
     * @param zipFile
     */
    def addToZip(files: List[File], zipFile: File) {
        //get name of temp file
        val tempFile = File.createTempFile(zipFile.getName, null)
        // delete it, because we need only name, not actual file
        tempFile.delete()

        if (!zipFile.renameTo(tempFile)) {
            throw new RuntimeException("Failed to rename the zip "+zipFile.getAbsolutePath+" to "+tempFile.getAbsolutePath)
        }

        val zin = new ZipInputStream(new FileInputStream(tempFile))
        val zos = new ZipOutputStream(new FileOutputStream(zipFile))

        //make files list searchable
        val fileSet = files.map(_.getName).toSet
        //re-archive existing zip into the new zip
        var entry = zin.getNextEntry
        while (entry != null) {
            val name = entry.getName
            if (!fileSet.contains(name)) {
                zos.putNextEntry(new ZipEntry(name))
                transfer(zin, zos, keepOpen = true)
            }
            entry = zin.getNextEntry
        }
        zin.close()

        //add extra files to the new zip
        for (f <- files) {
            logger.trace("Adding entry " + f.getName + "...")
            if (f.isDirectory) {
                addFolder(zos, f.getAbsolutePath, f.getAbsolutePath)

            } else {
                zos.putNextEntry(new ZipEntry(f.getName))
                transfer(new FileInputStream(f), zos)
                zos.closeEntry()
                logger.trace("OK!")
            }
        }

        zos.close()
        tempFile.delete()
    }

    /**
     * zip provided file or folder
     * @param fPath using provided path as file or top of the file tree zip everything into outputZipPath
     * @param outputZipPath - where to store the result
     */
    def compress(fPath: String, outputZipPath: String) {
        val fos = new FileOutputStream(outputZipPath)
        val zos = new ZipOutputStream(fos)
        zos.setLevel(9)
        logger.trace("Start compressing folder/file : " + fPath + " to " + outputZipPath)
        val f = new File(fPath)
        if (f.isDirectory) {
            addFolder(zos, fPath, fPath)
        } else {
            addFolder(zos, fPath, f.getParent)
        }
        zos.close()

    }

    /**
     * a very basic file lister which will cause "out of memory" on a very deep directory tree
     * @param rootFolder - top of the file tree
     * @return list of all files under rootFolder (including rootFolder)
     */
    def listFiles(rootFolder: File):List[File] = {
        def listOnelevel(f: File):List[File] = {
            if (f.isDirectory) {
                f.listFiles().filter(_.canRead).flatMap(listOnelevel).toList
            } else {
                List(f)
            }
        }
        val files = List() ++ listOnelevel(rootFolder)
        files
    }

    private def transfer(in: InputStream, out: OutputStream, keepOpen:Boolean = false) {
        val bytes = new Array[Byte](1024) //1024 bytes - Buffer size
        try {
            Iterator
                .continually (in.read(bytes))
                .takeWhile (-1 !=)
                .foreach (read=> out.write(bytes,0,read))
        }
        finally {
            if (!keepOpen) {
                in.close()
            }
        }

    }
    private def addFolder(zos: ZipOutputStream, folderName: String, baseFolderPath:String) {

        val f = new File(folderName)
        if (f.exists()) {
            if (f.isDirectory) {
                for (curFile <- f.listFiles) {
                    addFolder(zos, curFile.getAbsolutePath, baseFolderPath)
                }
            } else {
                val entryName = folderName.substring(baseFolderPath.length)
                logger.trace("Adding entry " + f.getName + "...")
                val ze = new ZipEntry(entryName)
                zos.putNextEntry(ze)
                //val in = Source.fromFile(folderName).bufferedReader()
                val in = new FileInputStream(folderName)
                transfer(in, zos)
                zos.closeEntry()
                logger.trace("OK!")
            }
        } else {
            logger.trace("File or directory not found " + folderName)
        }
    }

}
