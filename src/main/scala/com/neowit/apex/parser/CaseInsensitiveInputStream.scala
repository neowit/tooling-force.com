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

package com.neowit.apex.parser


import java.io.{InputStreamReader, InputStream, Reader}

import org.antlr.v4.runtime.{IntStream, ANTLRInputStream}

/**
  * NOTE - in order for this CaseInsensitiveInputStream to work all constants
  *        in .g4 grammar must be either *lower* case or *dual-case*
  */
class CaseInsensitiveInputStream(r: Reader, initialSize: Int, readChunkSize: Int)
    extends ANTLRInputStream(r, initialSize, readChunkSize) {

    //lazy is important here because need initiated data[], which is loaded in super class
    private lazy val lowercaseData: Array[Char] = data.map(_.toLower)

    def this (r: Reader) {
        this(r, initialSize = 1024, readChunkSize = 1024)
    }
    def this (input: InputStream) {
       this(new InputStreamReader(input), initialSize = 1024, readChunkSize = 1024)
    }

    override def LA(index: Int): Int = {
        var i = index
        if (i == 0) {
            return 0
        }
        if (i < 0) {
            i += 1
            if ((p + i - 1) < 0) {
                return IntStream.EOF
            }
        }
        if ((p + i - 1) >= n) {
            return IntStream.EOF
        }

        if (null != lowercaseData) {
            lowercaseData(p + i - 1)
        } else {
            data(p + i - 1).toLower
        }
    }

}
