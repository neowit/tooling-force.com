package com.neowit.utils
import spray.json._

/**
 * helpers for generic JSON conversions not supported by spray "out-of-the-box"
 */
object JsonUtils {

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
