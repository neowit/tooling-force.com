/*
 * Copyright (c) 2013 Andrey Gavrikov.
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

import java.io.{PrintStream, PrintWriter, StringWriter}
import java.util.concurrent.TimeUnit

import org.apache.commons.logging.impl.SimpleLog
import org.apache.commons.logging.{Log, LogFactory}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
/**
 * a simple "logger".
 * Used to conserve .jar size
 * A proper logger like Logback and its dependencies add 3MB+ to .jar size
 * In the future, if necessary, it shall be very easy to switch to something like typesafe.scalalogging
 */
trait Logging {
    private[this] val log: Log = LogFactory.getLog( this.getClass )

    def fatal(msg: Any) = if (log.isFatalEnabled) log.fatal(msg)
    def fatal(msg: Any, ex: Throwable) = if (log.isFatalEnabled) log.fatal(msg, ex)

    //def error(msg: Any) = if (log.isErrorEnabled) log.error(msg)
    //def error(msg: Any, ex: Throwable) = if (log.isErrorEnabled) log.error(msg, ex)
    def error(msg: String) = if (log.isErrorEnabled) log.error(msg)
    def error(ex: Throwable) = if (log.isErrorEnabled) Logging.log(ex, log.error)
    def error(msg: String, ex: Throwable) = if (log.isErrorEnabled) Logging.log(msg, ex, log.error)

    def warn(msg: Any) = if (log.isWarnEnabled) log.warn(msg)
    def warn(msg: Any, ex: Throwable) = if (log.isWarnEnabled) log.warn(msg, ex)

    def info(msg: Any) = if (log.isInfoEnabled) log.info(msg)
    def info(msg: Any, ex: Throwable) = if (log.isInfoEnabled) log.info(msg, ex)

    //def debug(msg: Any) = if (log.isDebugEnabled) log.debug(msg)
    //def debug(msg: Any, ex: Throwable) = if (log.isDebugEnabled) log.debug(msg, ex)
    def debug(msg: String) = if (log.isDebugEnabled) log.debug(msg)
    def debug[T](msgArr: Array[T]) = if (log.isDebugEnabled) log.debug(msgArr)
    def debug(ex: Throwable) = if (log.isDebugEnabled) Logging.log(ex, log.debug)
    def debug(msg: String, ex: Throwable) = if (log.isDebugEnabled) Logging.log(msg, ex, log.debug)

    //def trace(msg: Any) = if (log.isTraceEnabled) log.trace(msg)
    //def trace(msg: Any, ex: Throwable) = if (log.isTraceEnabled) log.trace(msg, ex)
    def trace(msg: String) = if (log.isTraceEnabled) log.trace(msg)
    def trace[T](msgArr: Array[T]) = if (log.isTraceEnabled) log.trace(msgArr)
    def trace(ex: Throwable) = if (log.isTraceEnabled) Logging.log(ex, log.trace)
    def trace(msg: String, ex: Throwable) = if (log.isTraceEnabled) Logging.log(msg, ex, log.trace)

    def isInfoEnabled: Boolean = log.isInfoEnabled

    def logger = this
}

object Logging {

    /**
      * repeatedly log same message to display progress of long running/blocking operation
      * @param logger - logger to use
      * @param codeBlock - keep logging provided msg while codeBlock is running
      * @param msg - message to log
      * @param out - output stream to use for log output
      *            have to specify one explicitly because working on a different thread
      * @param repeatEveryNSec - how frequently to log new message, in seconds
      * @param scheduler - scheduler to use
      */
    def repeatingInfo(logger: Logging, codeBlock: => Any, msg: Any, out: PrintStream,
                      repeatEveryNSec: Int = 3 )
                     (implicit scheduler: akka.actor.Scheduler): Unit = {
        val startTime = System.currentTimeMillis()
        if (logger.isInfoEnabled) {
            val cancellable = scheduler.schedule(Duration(0, TimeUnit.SECONDS), Duration(repeatEveryNSec, TimeUnit.SECONDS)){
                scala.Console.withOut(out) {
                    val diff = (System.currentTimeMillis - startTime) / 1000
                    if (diff > 0) {
                        logger.info(msg + s" # elapsed: ${diff}s")
                    } else {
                        logger.info(msg)
                    }
                }
            }
            try {
                codeBlock
            } finally {
                cancellable.cancel()
            }
        }
    }

    def getStackTrace(ex: Throwable): Iterable[String] = {
        val sw = new StringWriter
        ex.printStackTrace(new PrintWriter(sw))
        val stackTraceStr = sw.toString
        stackTraceStr.split("""\n""")
    }

    def log(ex: Throwable, logFun: Any => Unit): Unit = {
        if (null != ex) {
            Logging.getStackTrace(ex).foreach(logFun(_))
        }
    }
    def log(msg: String, ex: Throwable, logFun: Any => Unit): Unit = {
        logFun(msg)
        if (null != ex) {
            Logging.getStackTrace(ex).foreach(logFun(_))
        }
    }
}

/**
 * the only purpose of custom LogImpl is to use Console.out instead of System.err which SimpleLog uses
 */
class LogImpl(logName: String) extends SimpleLog (logName) {
    /**
     * SimpleLog writes to System.err which is not suitable in multi-thread environment
     * as one thread blocks out of the other
     * @param buffer - stuff to write into log
     */
    override def write(buffer: StringBuffer): Unit = {
        Console.out.println(buffer.toString)
    }
}

