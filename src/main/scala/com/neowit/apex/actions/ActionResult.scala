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

package com.neowit.apex.actions

import com.neowit.response._

sealed trait ActionResult

object ActionSuccess {
    def apply(): ActionSuccess = {
        new ActionSuccess(Nil, None)
    }
    def apply(messages: List[Message]): ActionSuccess = {
        new ActionSuccess(messages, None)
    }
    def apply(messages: List[Message], result: BaseResult): ActionSuccess = {
        new ActionSuccess(messages, Option(result))
    }
    def apply(result: BaseResult): ActionSuccess = {
        new ActionSuccess(Nil, Option(result))
    }
    def apply(msg: Message, result: BaseResult): ActionSuccess = {
        new ActionSuccess(List(msg), Option(result))
    }
    def apply(msg: Message): ActionSuccess = {
        new ActionSuccess(List(msg), None)
    }
    def apply(msg: String): ActionSuccess = {
        new ActionSuccess(List(InfoMessage(msg)), None)
    }
}


object ActionFailure {
    def apply(): ActionFailure = {
        new ActionFailure(Nil, None)
    }
    def apply(msg: Message): ActionFailure = {
        new ActionFailure(List(msg), None)
    }
    def apply(msg: String): ActionFailure = {
        new ActionFailure(List(ErrorMessage(msg)), None)
    }
    def apply(result: BaseResult): ActionFailure = {
        new ActionFailure(Nil, Option(result))
    }
    def apply(messages: List[Message]): ActionFailure = {
        new ActionFailure(messages, None)
    }

}

case class ActionSuccess(messages: List[Message], result: Option[BaseResult]) extends ActionResult
case class ActionFailure(messages: List[Message], result: Option[BaseResult]) extends ActionResult
