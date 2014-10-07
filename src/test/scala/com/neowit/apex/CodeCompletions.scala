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

    def withResponseFile(testCode: (File) => Any) {
        val responseFile = File.createTempFile("completions", ".json")
        try {
            testCode(responseFile) // "loan" the fixture to the test
        } finally {
            // clean up the fixture
            responseFile.delete()
        }
    }
    case class  TestConfig(lineMarker: String, column: Int, itemsCountMin: Int, identities: Option[List[String]],
                           signatureContains: Option[List[String]]) {
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
        implicit val testConfigFormat: JsonFormat[TestConfig] = lazyFormat(jsonFormat(TestConfig, "lineMarker", "column", "itemsCountMin", "identities", "signatureContains"))
        implicit val testCompletionItemFormat: JsonFormat[CompletionItem] = lazyFormat(jsonFormat(CompletionItem, "realIdentity", "signature", "doc", "identity", "type", "visibility"))
    }


    /**
     * main method which does actual validations
     * @param apexClassPath - full path to the file where test apex code and completion scenarios reside
     */
    private def testCompletionsInFile(apexClassPath: String): Unit = {

        withResponseFile { (responseFile) =>
            val extraParams = Array(
                s"--currentFilePath=${escapeFilePath(apexClassPath)}",
                s"--responseFilePath=${escapeFilePath(responseFile)}",
                s"--currentFileContentPath=${escapeFilePath(apexClassPath)}"
            )
            val lines = scala.io.Source.fromFile(apexClassPath).getLines().toArray[String]
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
                    runSingleCompletionTest(jsonTestDescription, commandLine ++ extraParams, apexClassPath, responseFile)
                }
                i += 1

            }
        }
    }

    def runSingleCompletionTest(jsonConfigStr: String, commandLineParams: Array[String], apexClassPath: String, responseFile: File): Unit = {
        val jsonAst = JsonParser(jsonConfigStr)
        val config = jsonAst.convertTo[TestConfig](ApexModelJsonProtocol.testConfigFormat)
        val lineNumber = config.getLineNumber(apexClassPath)
        assert(lineNumber > 0, "could not file line number by marker: " + config.lineMarker)

        val extraParams = Array[String]("--column=" + config.column, "--line=" + lineNumber)
        Runner.main(commandLineParams ++ extraParams)
        //validate result
        val lines = scala.io.Source.fromFile(responseFile).getLines().toArray[String]

        assertResult("RESULT=SUCCESS")(lines(0))

        val (matchFunc, itemsList) = config.identities match {
            case Some(_identities) => (matchIdentity(_identities)_, _identities)
            case None => config.signatureContains match {
              case Some(signatures) => (matchSignatureContains(signatures)_, signatures)
              case None => throw new IllegalStateException("unexpected config: " + config)
            }
            case _ => throw new IllegalStateException("unexpected config: " + config)
        }

        val foundItemsSet = collectFoundItems(lines, matchFunc)
        //println(lines.mkString("\n"))
        //"check that all expected items  found")
        val diff = itemsList.toSet.--(foundItemsSet)
        assertResult(itemsList.size, "Scenario: " + config.lineMarker + "; \nMissing item(s): " + diff.mkString(", "))(foundItemsSet.size)

    }
    //find exact item identity in the list of all exected identities
    private def matchIdentity(identities: List[String])(item: CompletionItem): String = {
        if (identities.contains(item.identity)) item.identity else ""
    }
    private def matchSignatureContains(signatureSubstrings: List[String])(item: CompletionItem): String = {
        signatureSubstrings.find(sig => item.signature.contains(sig)) match {
          case Some(foundSignature) => item.signature
          case None => ""
        }
    }
    private def collectFoundItems(responseLines: Array[String], matchFunc: CompletionItem => String): Set[String] = {

        var i=1
        val foundItems = Set.newBuilder[String]
        while (i < responseLines.size) {
            val lineJson = responseLines(i)
            val jsonAst = JsonParser(lineJson)
            val completionItem = jsonAst.convertTo[CompletionItem](ApexModelJsonProtocol.testCompletionItemFormat)

            val matchedString = matchFunc(completionItem)
            if (matchedString.nonEmpty) {
                foundItems += matchedString
            }

            i += 1
        }
        foundItems.result()
    }
    private def escapeFilePath(file: File): String = {
        escapeFilePath(file.getAbsolutePath)
    }
    private def escapeFilePath(filePath: String): String = {
        "\"" + filePath + "\""
    }

    test("SObject Creator completions") {
        val testApexClassFilePath = projectPath + "/src/classes/SObjectCompletions.cls"
        testCompletionsInFile(testApexClassFilePath)

    }
}
