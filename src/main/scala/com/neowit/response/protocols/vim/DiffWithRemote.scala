/*
 *
 *  * Copyright (c) 2017 Andrey Gavrikov.
 *  * this file is part of tooling-force.com application
 *  * https://github.com/neowit/tooling-force.com
 *  *
 *  * This program is free software: you can redistribute it and/or modify
 *  * it under the terms of the GNU Lesser General Public License as published by
 *  * the Free Software Foundation, either version 3 of the License, or
 *  * (at your option) any later version.
 *  *
 *  * This program is distributed in the hope that it will be useful,
 *  * but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  * GNU Lesser General Public License for more details.
 *  *
 *  * You should have received a copy of the GNU Lesser General Public License
 *  * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 */

package com.neowit.response.protocols.vim

import com.neowit.apex.actions.{DiffWithRemoteReport, DiffWithRemoteReportFailure, DiffWithRemoteReportSuccess}
import com.neowit.response._
import com.neowit.utils.ZuluTime

object DiffWithRemote {
    protected def writeReportToResponseFile(writer: ResponseWriterVim, report: DiffWithRemoteReport): Unit = {
        report match {
            case DiffWithRemoteReportFailure(errors) =>
                errors.foreach(writer.send(_))
            case DiffWithRemoteReportSuccess(bulkRetrieveResult, remoteSrcFolderPath) =>
                writer.send("REMOTE_SRC_FOLDER_PATH=" + remoteSrcFolderPath)
                writer.send(InfoMessage("Remote version is saved in: " + remoteSrcFolderPath))

                val conflictingFilesMap = report.getConflictingFiles
                val localFilesMissingOnRemoteMap = report.getLocalFilesMissingOnRemote
                val remoteFilesMissingLocallyMap = report.getRemoteFilesMissingLocally

                if (report.hasSomethingToReport) {

                    if (conflictingFilesMap.nonEmpty) {
                        //list files where remote version has different size compared to local version
                        //Modified Files
                        val msg = WarnMessage("Different file sizes")
                        writer.send(msg)

                        for (relativePath <- conflictingFilesMap.keys.toList.sortWith((left, right) => left.compareTo(right) < 0)) {
                            conflictingFilesMap.get(relativePath) match {
                                case Some(conflictingFile) =>
                                    val props = conflictingFile.remoteProp
                                    val sizeLocal = conflictingFile.fileLocal.length()
                                    val sizeRemote = conflictingFile.fileRemote.length()
                                    val text = conflictingFile.fileLocal.getName +
                                        " => Modified By: " + props.getLastModifiedByName +
                                        "; at: " + ZuluTime.formatDateGMT(props.getLastModifiedDate) +
                                        s"; Local size: $sizeLocal; remote size: $sizeRemote"
                                    writer.println(MessageDetailMap(msg, Map("filePath" -> relativePath, "text" -> text)))
                                case None =>
                            }
                        }
                    }

                    if (localFilesMissingOnRemoteMap.nonEmpty) {
                        //list files that exist on locally but do not exist on remote
                        val msg = WarnMessage("Files exist locally but not on remote (based on current package.xml)")
                        writer.send(msg)

                        for(relativePath <- localFilesMissingOnRemoteMap.keys) {
                            localFilesMissingOnRemoteMap.get(relativePath) match {
                                case Some(localFile) =>
                                    val text = localFile.getName +
                                        " => exists locally but missing on remote"
                                    val echoText = localFile.getName
                                    writer.println(MessageDetailMap(msg, Map("filePath" -> relativePath, "text" -> text, "echoText" -> echoText)))
                                case None =>
                            }
                        }
                    }

                    if (remoteFilesMissingLocallyMap.nonEmpty) {
                        //list files that exist on remote but do not exist locally
                        val msg = WarnMessage("Files exist on remote but not locally (based on current package.xml)")
                        writer.send(msg)

                        for(relativePath <- remoteFilesMissingLocallyMap.keys.toList.sortWith( (left, right) => left.compareTo(right) < 0)) {
                            remoteFilesMissingLocallyMap.get(relativePath) match {
                                case Some(remoteFile) =>
                                    bulkRetrieveResult.getFileProps(relativePath) match {
                                        case Some(props) =>
                                            val sizeRemote = remoteFile.length()
                                            val text = remoteFile.getName +
                                                " => Modified By: " + props.getLastModifiedByName +
                                                "; at: " + ZuluTime.formatDateGMT(props.getLastModifiedDate) +
                                                s"; remote size: $sizeRemote"
                                            writer.println(MessageDetailMap(msg, Map("filePath" -> relativePath, "text" -> text)))
                                        case None =>
                                    }
                                case None =>
                            }
                        }
                    }
                } else {
                    writer.send(InfoMessage("No differences between local version and remote Org detected"))
                }
            case _ => ()
        }




    }
}
class DiffWithRemote(writer: ResponseWriterVim) extends VimProtocol[DiffWithRemoteResult] {
    def send(result: DiffWithRemoteResult): Unit = {
        DiffWithRemote.writeReportToResponseFile(writer, result.report)
    }
}
