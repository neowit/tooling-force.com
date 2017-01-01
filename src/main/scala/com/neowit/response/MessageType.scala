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

package com.neowit.response

/**
  * Author: Andrey Gavrikov
  */
sealed trait MessageType {
    def getTypeString: String
    override def toString: String = getTypeString
}
case object INFO extends MessageType {
    def getTypeString: String = "INFO"
}
case object WARN extends MessageType {
    def getTypeString: String = "WARN"
}
case object ERROR extends MessageType {
    def getTypeString: String = "ERROR"
}
case object DEBUG extends MessageType {
    def getTypeString: String = "DEBUG"
}
case object SECTION extends MessageType {
    def getTypeString: String = "SECTION"
}
case object KEY_VALUE extends MessageType {
    def getTypeString: String = "KEY_VALUE"
}
case class CustomMessageType(msgType: String) extends MessageType {
    def getTypeString: String = msgType
}
