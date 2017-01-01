package com.neowit.response.protocols.vim

import com.neowit.response.{INFO, RefreshMetadataResult}

/**
  * Author: Andrey Gavrikov
  */
class RefreshMetadata(writer: ResponseWriterVim) extends ListModified(writer){
    def send(result: RefreshMetadataResult): Unit = {
        val modifiedFiles = result.modified
        if (modifiedFiles.nonEmpty) {
            reportModifiedFiles(modifiedFiles, INFO, writer)
        }
    }
}
