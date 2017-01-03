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

import com.neowit.apex.actions.DeploymentConflictsReport
import com.neowit.response.{InfoMessage, ListConflictingResult, MessageDetailMap}

object ListConflicting {

    def sendConflictDetails(writer: ResponseWriterVim, conflictReport: DeploymentConflictsReport): Unit = {
        val msg = InfoMessage("Outdated file(s) detected.")
        writer.send(msg)
        conflictReport.conflicts
            .foreach(conflict =>
                writer.println(
                    MessageDetailMap(
                        msg,
                        Map(
                            "filePath" -> conflict.file.getAbsolutePath,
                            "text" ->
                                (
                                    s"${conflict.file.getName} => " +
                                        s"Modified By: ${conflict.lastModifiedByName.getOrElse("")}; " +
                                        s"at: ${conflict.remoteLastModifiedDate.getOrElse("")}; " +
                                        s"Local version saved at: ${conflict.localLastModifiedDate.getOrElse("")}"
                                    )
                        )
                    )
                )
            )
    }
}
class ListConflicting(writer: ResponseWriterVim) extends VimProtocol[ListConflictingResult] {
    def send(result: ListConflictingResult): Unit = {
        if (result.conflictReport.hasConflicts) {
            ListConflicting.sendConflictDetails(writer, result.conflictReport)
        } else {
            writer.send(InfoMessage("No outdated files detected"))
        }
    }
}
