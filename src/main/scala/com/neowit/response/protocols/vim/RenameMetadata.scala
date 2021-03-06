/*
 * Copyright (c) 2020 Andrey Gavrikov.
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

import com.neowit.response.RenameMetadataResult

class RenameMetadata(writer: ResponseWriterVim) extends VimProtocol[RenameMetadataResult] {
    def send(result: RenameMetadataResult): Unit = {
        if (result.errors.nonEmpty) {
            result.errors.foreach(errorStr =>
                writer.send("ERROR", Map("text" -> errorStr))
            )
        }
    }
}


