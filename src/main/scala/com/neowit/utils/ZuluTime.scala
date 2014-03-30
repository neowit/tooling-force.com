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
