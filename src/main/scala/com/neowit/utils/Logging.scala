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

import com.typesafe.scalalogging.StrictLogging

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
/**
  * this used to be custom/simple log implementation
  * now it is just an alias for com.typesafe.scalalogging.Logger
 */
trait Logging extends StrictLogging {
}

object Logging {

    /**
      * repeatedly log same message to display progress of long running/blocking operation
      * @param log - logger to use
      * @param codeBlock - keep logging provided msg while codeBlock is running
      * @param msg - message to log
      * @param out - output stream to use for log output
      *            have to specify one explicitly because working on a different thread
      * @param repeatEveryNSec - how frequently to log new message, in seconds
      * @param scheduler - scheduler to use
      */
    def repeatingInfo(log: com.typesafe.scalalogging.Logger, codeBlock: => Any, msg: Any, out: PrintStream,
                      repeatEveryNSec: Int = 3 )
                     (implicit scheduler: akka.actor.Scheduler): Unit = {
        val startTime = System.currentTimeMillis()

        val task = new Runnable {
            def run(): Unit = {
                scala.Console.withOut(out) {
                    val diff = (System.currentTimeMillis - startTime) / 1000
                    if (diff > 0) {
                        log.info(msg.toString + s" # elapsed: ${diff}s")
                    } else {
                        log.info("", msg)
                    }
                }
            }
        }
        //if (logger.isInfoEnabled) {
            //val cancellable = scheduler.schedule(Duration(0, TimeUnit.SECONDS), Duration(repeatEveryNSec, TimeUnit.SECONDS)){
            val cancellable = scheduler.scheduleWithFixedDelay(Duration(0, TimeUnit.SECONDS), Duration(repeatEveryNSec, TimeUnit.SECONDS)){
               task
            }
            try {
                codeBlock
            } finally {
                cancellable.cancel()
            }
        //}
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
/*
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

*/