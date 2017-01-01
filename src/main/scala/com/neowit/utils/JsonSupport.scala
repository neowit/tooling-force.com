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

package com.neowit.utils

import spray.json.{DefaultJsonProtocol, JsArray, JsFalse, JsNull, JsNumber, JsObject, JsString, JsTrue, JsValue, JsonFormat}

/**
  * Author: Andrey Gavrikov
  * Date: 06/12/2016
  */
trait JsonSupport extends DefaultJsonProtocol {
    implicit object AnyJsonFormat extends JsonFormat[Any] {
        def write(x: Any) = x match {
            case n: Long => JsNumber(n)
            case n: Int => JsNumber(n)
            case s: String => JsString(s)
            case b: Boolean if b => JsTrue
            case b: Boolean if !b => JsFalse
            case a: JsArray => a
            case o: JsObject => o
            case other if null == other => JsNull
            case other => JsString(other.toString)
        }
        def read(value: JsValue) = value match {
            case JsNumber(n) if n.isValidInt => n.intValue()
            case JsNumber(n) if n.isValidLong => n.longValue()
            case JsString(s) => s
            case JsTrue => true
            case JsFalse => false
            case JsObject(o) => o.map {
                case (key, _value) => key -> read(_value)
            }
            case JsNull => null
            case JsArray(a) => a.map(read(_))
        }
    }

}
