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

import org.apache.commons.logging.{Log, LogFactory}
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

    def error(msg: Any) = if (log.isInfoEnabled) log.error(msg)
    def error(msg: Any, ex: Throwable) = if (log.isErrorEnabled) log.error(msg, ex)

    def warn(msg: Any) = if (log.isWarnEnabled) log.warn(msg)
    def warn(msg: Any, ex: Throwable) = if (log.isWarnEnabled) log.warn(msg, ex)

    def info(msg: Any) = if (log.isInfoEnabled) log.info(msg)
    def info(msg: Any, ex: Throwable) = if (log.isInfoEnabled) log.info(msg, ex)

    def debug(msg: Any) = if (log.isDebugEnabled) log.debug(msg)
    def debug(msg: Any, ex: Throwable) = if (log.isDebugEnabled) log.debug(msg, ex)

    def trace(msg: Any) = if (log.isTraceEnabled) log.trace(msg)
    def trace(msg: Any, ex: Throwable) = if (log.isTraceEnabled) log.trace(msg, ex)


    def logger = this
}
