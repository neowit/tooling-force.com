/*
 * Copyright (c) 2018 Andrey Gavrikov.
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

package com.neowit.apex.lsp

import io.circe._
//import io.circe.generic.semiauto._
/**
  * Created by Andrey Gavrikov 
  */
sealed trait WorkspaceCommand {
    def name: String
}

object WorkspaceCommand {
    case object ToolingVersion extends WorkspaceCommand {
        override val name: String = "toolingVersion"
    }
    case object ApexDeploy extends WorkspaceCommand {
        override val name: String = "deploy"
    }

    def apply(name: String): WorkspaceCommand = {
        name match {
            case ToolingVersion.name => ToolingVersion
            case ApexDeploy.name => ApexDeploy
            case x => throw new IllegalArgumentException("WorkspaceCommand " + x + " is not supported")
        }
    }

    trait JsonSupport {
        implicit val WorkspaceCommandEncoder: Encoder[WorkspaceCommand] =
            Encoder.forProduct1("name")(_.name)

        implicit val WorkspaceCommandDecoder: Decoder[WorkspaceCommand] =
            Decoder.forProduct1("name")(WorkspaceCommand.apply)
    }
}
