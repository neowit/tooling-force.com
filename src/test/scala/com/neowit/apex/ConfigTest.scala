/*
 * Copyright (c) 2013 Andrey Gavrikov.
 * this file is part of Backup-force.com application
 * https://github.com/neowit/backup-force.com
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

package com.neowit.apex

import org.scalatest.PrivateMethodTester
import org.scalatest.funsuite.AnyFunSuite
import java.io.{File, FileNotFoundException, FileWriter}
import java.util.Properties

import com.neowit.utils.{BasicConfig, Config, InvalidCommandLineException, OptionProperties}

class ConfigTest extends AnyFunSuite with PrivateMethodTester {
    val appConfig = new Config(new BasicConfig)

    val FAIL = false
    def withFile(testCode: (File, FileWriter) => Any): Unit = {
        val file = File.createTempFile("test", ".properties") // create the fixture
        val writer = new FileWriter(file)
        try {
            testCode(file, writer) // "loan" the fixture to the test
        } finally {
            // clean up the fixture
            writer.close()
            file.delete()
        }

    }

    test("No Command Params") {
        try {
            appConfig.load(List())
        } catch {
            case ex: InvalidCommandLineException => println("OK")
            case ex: Throwable => assert(FAIL, "Expected InvalidCommandLineException for empty command line. " + ex)
        }
    }
    test("Key without value") {
        try {
            appConfig.load(List("--incorrect"))
        } catch {
            case ex: InvalidCommandLineException => println("OK")
            case ex: Throwable => assert(FAIL, "Expected InvalidCommandLineException for empty command line." + ex)
        }
    }
    test("Missing ConfigFile") {
        try {
            appConfig.load(List("--config=/some/path"))
        } catch {
            case ex: FileNotFoundException => println("OK")
            case ex: Throwable => assert(FAIL, "Expected FileNotFoundException for missing config parameter. " + ex)
        }
    }

    test("Existing config, but missing parameter") {
        withFile { (file, writer) =>
            val props = new java.util.Properties()
            props.setProperty("sf.username", "aaa@bb.cc")
            props.setProperty("responseFilePath", "/tmp/test.txt")
            props.store(writer, "")

            appConfig.load(List("--config=" + file.getAbsolutePath))
            assertResult(None) { appConfig.getProperty("password")  }
        }
    }
    test("Command Line - Key Without Value #1") {
        intercept[InvalidCommandLineException]{
            appConfig.load(List("--config"))
        }
    }
    test("Command Line - Key Without Value #2") {
        intercept[InvalidCommandLineException]{
            appConfig.load(List("--config="))
        }
    }
    test("Command Line - Key Without --") {
        intercept[InvalidCommandLineException]{
            appConfig.load(List("config=val"))
        }
    }
    test("Command Line - value enclosed in quotes --") {
        withFile { (file, writer) =>
            appConfig.load(List("--config=" + file.getAbsolutePath, "--key1=\"" + file.getAbsolutePath+"\"" , "--key2=\"val2\""))
            assertResult(Some(file.getAbsolutePath)) { appConfig.getProperty("key1") }
            assertResult(Some("val2")) { appConfig.getProperty("key2") }
        }
    }
    test("Parameter from command line take priority over config") {
        withFile { (file, writer) =>
            val props = new java.util.Properties()
            props.setProperty("param1", "val1")
            props.store(writer, "")

            appConfig.load(List("--config="+ file.getAbsolutePath, "--param1=val2"))
            assertResult(Option("val2")) { appConfig.getProperty("param1")  }
        }
    }
    test("Parameter from command line take priority over --config") {
        withFile { (file, writer) =>
            val props = new Properties() with OptionProperties
            props.setProperty("sf.serverurl", "val1")
            props.store(writer, "")

            appConfig.load(List("--config=" + file.getAbsolutePath, "--config=" + file.getAbsolutePath, "--sf.serverurl=val2"))
            assertResult(Some("val2")) { appConfig.getProperty("sf.serverurl") }
        }
    }

}
