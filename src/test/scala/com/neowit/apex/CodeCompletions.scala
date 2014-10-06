package com.neowit.apex

import java.io.File
import java.util.Properties

import com.neowit.utils.{BasicConfig, Config}
import org.scalatest.FunSuite

import spray.json._
import DefaultJsonProtocol._

class CodeCompletions extends FunSuite {

    val is = getClass.getClassLoader.getResource("paths.properties").openStream()
    val paths = new Properties()
    paths.load(is)

    val projectPath = paths.getProperty("projectPath")
    val loginCredentialsPath = paths.getProperty("loginCredentialsPath")

    val commandLine = Array(
        s"--config=${escapeFilePath(loginCredentialsPath)}"
        ,"--action=listCompletions"
        ,s"--projectPath=${escapeFilePath(projectPath)}"

    )

    case class  TestConfig(lineMarker: String, column: Int, itemsCountMin: Int, items: List[String]) {
        /**
         * find lineMarker in the specified file
         * @return line number where marker is found
         */
        def getLineNumber(filePath: String): Int = {
            val lines = scala.io.Source.fromFile(filePath).getLines().toArray[String]
            var i = 0
            while (i < lines.size) {
                val line = lines(i)
                if (line.contains(lineMarker) && !line.contains("\"lineMarker\"")) {
                    return i + 1
                }

                i += 1
            }
            -1
        }
    }

    case class CompletionItem(realIdentity: String, signature: String, doc: String, identity: String, `type`: String, visibility: String)

    object ApexModelJsonProtocol extends DefaultJsonProtocol {
        implicit val testConfigFormat: JsonFormat[TestConfig] = lazyFormat(jsonFormat(TestConfig, "lineMarker", "column", "itemsCountMin", "items"))
        implicit val testCompletionItemFormat: JsonFormat[CompletionItem] = lazyFormat(jsonFormat(CompletionItem, "realIdentity", "signature", "doc", "identity", "type", "visibility"))
    }

    test("SObject Creator completions") {

        val testApexClassFilePath = projectPath + "/src/classes/SObjectCompletions.cls"
        val responseFile = File.createTempFile("completions", ".json")
        val extraParams = Array(
            s"--currentFilePath=${escapeFilePath(testApexClassFilePath)}",
            s"--responseFilePath=${escapeFilePath(responseFile)}",
            s"--currentFileContentPath=${escapeFilePath(testApexClassFilePath)}"
        )

        val lines = scala.io.Source.fromFile(testApexClassFilePath).getLines().toArray[String]
        var i = 0
        while (i < lines.size) {
            val line = lines(i)
            var jsonTestDescription: String = ""
            if (line.contains("#START")) {
                i = i + 1
                while (!lines(i).contains("#END")) {
                    val jsonLine = lines(i)
                    jsonTestDescription += jsonLine
                    i = i + 1
                }
                //reached end of current test description, now run the test
                runTest(jsonTestDescription, commandLine ++ extraParams, testApexClassFilePath, responseFile)
            }
            i += 1

        }
        responseFile.delete() //clean up

    }

    def runTest(jsonConfigStr: String, commandLineParams: Array[String], apexClassPath: String, responseFile: File): Unit = {
        val jsonAst = JsonParser(jsonConfigStr)
        val config = jsonAst.convertTo[TestConfig](ApexModelJsonProtocol.testConfigFormat)
        val identitiesList = config.items
        val identitiesSet = identitiesList.toSet
        val lineNumber = config.getLineNumber(apexClassPath)
        assert(lineNumber > 0, "could not file line number by marker: " + config.lineMarker)

        val extraParams = Array[String]("--column=" + config.column, "--line=" + lineNumber)
        Runner.main(commandLineParams ++ extraParams)
        //validate result
        val lines = scala.io.Source.fromFile(responseFile).getLines().toArray[String]

        assertResult("RESULT=SUCCESS")(lines(0))
        var i=1
        val foundItems = Set.newBuilder[String]
        while (i < lines.size) {
            val lineJson = lines(i)
            val jsonAst = JsonParser(lineJson)
            val completionItem = jsonAst.convertTo[CompletionItem](ApexModelJsonProtocol.testCompletionItemFormat)

            if (identitiesSet.contains(completionItem.identity)) {
                foundItems += completionItem.identity
            }

            i += 1
        }
        //println(lines.mkString("\n"))
        //"check that all expected items  found")
        val foundItemsSet = foundItems.result()
        val diff = identitiesSet.--(foundItemsSet)
        assertResult(identitiesSet.size, "Scenario: " + config.lineMarker + "; \nMissing item(s): " + diff.mkString(", "))(foundItemsSet.size)

    }
    private def escapeFilePath(file: File): String = {
        escapeFilePath(file.getAbsolutePath)
    }
    private def escapeFilePath(filePath: String): String = {
        "\"" + filePath + "\""
    }

}
