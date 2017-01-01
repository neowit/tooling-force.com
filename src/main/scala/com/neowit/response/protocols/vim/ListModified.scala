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

package com.neowit.response.protocols.vim

import java.io.File

import com.neowit.response._

/**
  * Author: Andrey Gavrikov
  */
class ListModified(writer: ResponseWriterVim) {

    def send(result: ListModifiedResult): Unit = {
        val modifiedFiles = result.modified
        val deletedFiles = result.deleted

        writer.println("FILE_COUNT=" + modifiedFiles.length + deletedFiles.length)
        if (modifiedFiles.nonEmpty) {
            reportModifiedFiles(modifiedFiles, INFO, writer)
        } else {
            //responseWriter.println(new Message(INFO, "No Modified file(s) detected."))
            writer.println(InfoMessage("No Modified file(s) detected."))
        }

        if (deletedFiles.nonEmpty) {
            reportDeletedFiles(deletedFiles, writer)
        } else {
            writer.println(InfoMessage("No Deleted file(s) detected."))
        }
    }
    protected def reportModifiedFiles(modifiedFiles: List[File],
                            messageType: MessageType = INFO,
                            responseWriter: ResponseWriterVim): Unit = {
        val msg = ArbitraryTypeMessage(messageType, "Modified file(s) detected.", Map("code" -> "HAS_MODIFIED_FILES"))
        responseWriter.println(msg)

        for(f <- modifiedFiles) {
            responseWriter.println(MessageDetailMap(msg, Map("filePath" -> f.getAbsolutePath, "text" -> writer.getRelativePath(f))))
        }
        responseWriter.println("HAS_MODIFIED_FILES=true")

        responseWriter.startSection("MODIFIED FILE LIST")
        for(f <- modifiedFiles) {
            //responseWriter.println("MODIFIED_FILE=" + session.getRelativePath(f))
            responseWriter.println("MODIFIED_FILE=" + f.getName)
        }
        responseWriter.endSection("MODIFIED FILE LIST")
        Unit

    }

    private def reportDeletedFiles(deletedFiles: List[File], responseWriter: ResponseWriterVim): Unit = {
        val msg = InfoMessage("Deleted file(s) detected.", Map("code" -> "HAS_DELETED_FILES"))
        responseWriter.println(msg)

        for(f <- deletedFiles) {
            responseWriter.println(MessageDetailMap(msg, Map("filePath" -> f.getAbsolutePath, "text" -> writer.getRelativePath(f))))
        }
        responseWriter.println("HAS_DELETED_FILES=true")

        responseWriter.startSection("DELETED FILE LIST")
        for(f <- deletedFiles) {
            responseWriter.println("DELETED_FILE=" + writer.getRelativePath(f))
        }
        responseWriter.endSection("DELETED FILE LIST")
    }

}
