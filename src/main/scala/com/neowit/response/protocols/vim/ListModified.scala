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

object ListModified {
    def reportModifiedFiles(writer: ResponseWriterVim, modifiedFiles: List[File],
                                      messageType: MessageType = INFO,
                                      responseWriter: ResponseWriterVim): Unit = {
        if (modifiedFiles.nonEmpty) {
            val msg = ArbitraryTypeMessage(messageType, "Modified file(s) detected.", Map("code" -> "HAS_MODIFIED_FILES"))
            responseWriter.send(msg)

            // display messages in message window
            for(f <- modifiedFiles) {
                responseWriter.println(MessageDetailMap(msg, Map("filePath" -> f.getAbsolutePath, "text" -> ResponseWriter.getRelativePath(f))))
            }
            responseWriter.send("HAS_MODIFIED_FILES=true")

            responseWriter.startSection("MODIFIED FILE LIST")
            for(f <- modifiedFiles) {
                // data for vim script to decide which route to take
                responseWriter.send("MODIFIED_FILE=" + ResponseWriter.getRelativePath(f))
            }
            responseWriter.endSection("MODIFIED FILE LIST")
        }
        Unit

    }

    def reportDeletedFiles(writer: ResponseWriterVim, deletedFiles: List[File], responseWriter: ResponseWriterVim): Unit = {
        val msg = InfoMessage("Deleted file(s) detected.", Map("code" -> "HAS_DELETED_FILES"))
        responseWriter.send(msg)

        for(f <- deletedFiles) {
            responseWriter.println(MessageDetailMap(msg, Map("filePath" -> f.getAbsolutePath, "text" -> ResponseWriter.getRelativePath(f))))
        }
        responseWriter.send("HAS_DELETED_FILES=true")

        responseWriter.startSection("DELETED FILE LIST")
        for(f <- deletedFiles) {
            responseWriter.send("DELETED_FILE=" + ResponseWriter.getRelativePath(f))
        }
        responseWriter.endSection("DELETED FILE LIST")
    }

}
class ListModified(writer: ResponseWriterVim) extends VimProtocol[ListModifiedResult] {
    import ListModified._

    def send(result: ListModifiedResult): Unit = {
        val modifiedFiles = result.modified
        val deletedFiles = result.deleted

        writer.send("FILE_COUNT=" + modifiedFiles.length + deletedFiles.length)
        if (modifiedFiles.nonEmpty) {
            reportModifiedFiles(writer, modifiedFiles, INFO, writer)
        } else {
            //responseWriter.println(new Message(INFO, "No Modified file(s) detected."))
            writer.send(InfoMessage("No Modified file(s) detected."))
        }

        if (deletedFiles.nonEmpty) {
            reportDeletedFiles(writer, deletedFiles, writer)
        } else {
            writer.send(InfoMessage("No Deleted file(s) detected."))
        }
    }

}
