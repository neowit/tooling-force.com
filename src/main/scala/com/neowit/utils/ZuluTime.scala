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

import java.util.{GregorianCalendar, Calendar, TimeZone}
import com.sforce.ws.bind.CalendarCodec

object ZuluTime {

    val codec = new CalendarCodec()

    def formatDateGMT(cal: Calendar):String = {
        //it is tempting to extract SimpleDateFormat initialisation as instance variable, but DO NOT,
        // as this will not work in multi threaded cases
        val dateFormatGmt = new java.text.SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss")
        dateFormatGmt.setTimeZone(TimeZone.getTimeZone("GMT"))
        dateFormatGmt.format(cal.getTime) + ".000Z"
    }
    def deserialize(dateStr: String): Calendar = codec.deserialize(dateStr)

    def toCalendar(millis: Long): Calendar = {
        val calendar = new GregorianCalendar(TimeZone.getTimeZone("GMT"))
        calendar.setTimeInMillis(millis)
        calendar
    }



}
