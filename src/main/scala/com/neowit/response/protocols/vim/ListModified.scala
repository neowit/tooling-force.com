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
    def getRelativePath(f: File): String = {
        //todo implement properly
        f.getName
    }
    protected def reportModifiedFiles(modifiedFiles: List[File],
                            messageType: MessageType = INFO,
                            responseWriter: ResponseWriterVim): Unit = {
        val msg = ArbitraryTypeMessage(messageType, "Modified file(s) detected.", Map("code" -> "HAS_MODIFIED_FILES"))
        responseWriter.println(msg)

        for(f <- modifiedFiles) {
            responseWriter.println(MessageDetailMap(msg, Map("filePath" -> f.getAbsolutePath, "text" -> getRelativePath(f))))
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
            responseWriter.println(MessageDetailMap(msg, Map("filePath" -> f.getAbsolutePath, "text" -> getRelativePath(f))))
        }
        responseWriter.println("HAS_DELETED_FILES=true")

        responseWriter.startSection("DELETED FILE LIST")
        for(f <- deletedFiles) {
            responseWriter.println("DELETED_FILE=" + getRelativePath(f))
        }
        responseWriter.endSection("DELETED FILE LIST")
    }

}
