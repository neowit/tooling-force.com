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

package com.neowit.apex.tooling

import java.io.File
import com.sforce.soap.tooling.{SoapConnection, ApexClass}
import scala.Array

/**
 * User: andrey
 * Date: 13/09/2013
 */

trait ProcessorBase extends Logging {
    def isFile(resourcePath: String) = {
        val f = new File(resourcePath)
        f.isFile && f.canRead
    }
    def isDirectory(resourcePath: String) = {
        val f = new File(resourcePath)
        f.isDirectory && f.canRead
    }
    def getName(resourcePath: String) = {
        val f = new File(resourcePath)
        f.getName
    }

    def getBody(resourcePath: String): String = {
        scala.io.Source.fromFile(resourcePath).getLines().mkString("\n")
    }

}
trait Processor extends ProcessorBase {
    def process(connection: SoapConnection)

}
object Processor {

    def getProcessor(resourcePath: String): Processor = {
        resourcePath match {
            case ClassProcessor(className, ext) => new ClassProcessor(resourcePath, className)
            case _ => throw new ConfigValueException("Invalid resource path: " + resourcePath)
        }
    }

}

class ClassProcessor(resourcePath: String, className: String) extends Processor {

    def create(connection: SoapConnection) = {
        val apexClass = new ApexClass()
        apexClass.setBody(getBody(resourcePath))
        val saveResult = connection.create(Array(apexClass))
        logger.debug("saveResult=" + saveResult)
    }
    def process(connection: SoapConnection) {
        create(connection)
    }

}
object ClassProcessor  extends ProcessorBase {
    def unapply(resourcePath: String): Option[(String, String)] = {
        if (isFile(resourcePath) && resourcePath.endsWith(".cls")) {
            val nameWithExt = getName(resourcePath)
            Option((nameWithExt.substring(0, nameWithExt.length - ".cls".length), ".cls"))
        } else {
            None
        }
    }

}
