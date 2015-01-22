package com.neowit.apex.parser

import java.io._

import org.antlr.v4.runtime.{IntStream, ANTLRInputStream}

class CaseInsensitiveInputStream(r: Reader, initialSize: Int, readChunkSize: Int)
    extends ANTLRInputStream(r, initialSize, readChunkSize) {

    private var lookAheadData: Array[Char] = new Array[Char](data.length)

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
        //lookAheadData(p + i - 1)
        lookAheadData(p + i - 1)
        data(p + i - 1).toLower
    }

    override def load(r: Reader, size: Int, readChunkSize: Int): Unit = {
        super.load(r, size, readChunkSize)
        lookAheadData = new Array[Char](data.length)
        lookAheadData = data.map(_.toLower)
    }

}
