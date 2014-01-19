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

import java.io.{File,FileInputStream,FileOutputStream}
import java.security.MessageDigest

object FileUtils {

    def createTempDir(appConfig: Config): File = {
        val name = appConfig.action
        val outputFolder =
        appConfig.tempFolderPath match {
          case Some(tempFolderPath) =>
              //clean-up
              delete(new File(appConfig.tempFolderPath + File.separator + name))

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
            }
        }
        //return result
        !file.exists()

    }
    def createTempFile(prefix: String, suffix: String) = {
        File.createTempFile(prefix, suffix)
    }

    private def isIgnored(file: File) = {
        file.getName.startsWith(".") || file.getName.contains("~")
    }
    /**
     * a very basic file lister which will cause "out of memory" on a very deep directory tree
     * with lots of files
     * @param dir - top of the file tree
     * @param includeFolders - set to false if list should be single level, not recursive and without nested folders
     * @return list of all files under dir (including dir)
     */
    def listFiles(dir: File, includeFolders: Boolean = true):List[File] = {
        def listOnelevel(f: File):List[File] = {
            if (f.isDirectory && includeFolders) {
                f.listFiles().filter(ff => ff.canRead && !isIgnored(ff)).flatMap(listOnelevel).toList
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
}
