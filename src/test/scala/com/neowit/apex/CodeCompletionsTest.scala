package com.neowit.apex

import java.io.File
import java.util.Properties

import com.neowit.utils.FileUtils
import org.scalatest.FunSuite

import spray.json._

import scala.util.{Failure, Try, Success}

import scala.concurrent.ExecutionContext.Implicits.global
//import DefaultJsonProtocol._

class CodeCompletionsTest extends FunSuite {

    private val is = getClass.getClassLoader.getResource("paths.properties").openStream()
    val paths = new Properties()
    paths.load(is)

    private val projectPath = paths.getProperty("completions.projectPath")
    private val loginCredentialsPath = paths.getProperty("loginCredentialsPath")

    val commandLine = Array(
        s"--config=${escapeFilePath(loginCredentialsPath)}"
        ,"--action=listCompletions"
        ,s"--projectPath=${escapeFilePath(projectPath)}"
        ,s"--reportUsage=0"

    )

    def withResponseFile(testCode: (File) => Any): Unit = {
        val responseFile = File.createTempFile("completions", ".json")
        try {
            testCode(responseFile) // "loan" the fixture to the test
        } finally {
            // clean up the fixture
            responseFile.delete()
        }
        Unit
    }
    case class TestConfig(lineMarker: String, column: Int, itemsCountMin: Int, identities: Option[List[String]], identityMustNotContain: Option[List[String]],
                           signatureContains: Option[List[String]], signatureMustNotContain: Option[List[String]],
                           docContains: Option[List[String]], docMustNotContain: Option[List[String]]   ) {
        /**
         * find lineMarker in the specified file
         * @return line number where marker is found
         */
        def getLineNumber(filePath: String): Int = {
            val lines = FileUtils.readFile(filePath).getLines().toArray[String]
            var i = 0
            while (i < lines.length) {
                val line = lines(i)
                val indexOfMarker = line.indexOf(lineMarker)
                //exclude lines where
                //- actual marker is defined as "lineMarker": "something"
                //- where method with same name as marker resides, "(" after marker position
                //- where current test method is also listed in the expected results "signatureContains" : ["methodName"]
                if (indexOfMarker > 0 && !line.contains("\"lineMarker\"") && line.indexOf("(", indexOfMarker) < 0
                    && line.indexOf("\"", indexOfMarker) < 0 ) {
                    return i + 1
                }

                i += 1
            }
            -1
        }
    }

    case class CompletionItem(realIdentity: String, signature: String, doc: String, identity: String, `type`: String, visibility: String)

    object ApexModelJsonProtocol extends DefaultJsonProtocol {
        implicit val testConfigFormat: JsonFormat[TestConfig] = lazyFormat(jsonFormat(TestConfig, "lineMarker", "column", "itemsCountMin", "identities", "identityMustNotContain", "signatureContains", "signatureMustNotContain", "docContains", "docMustNotContain"))
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
            val lines = FileUtils.readFile(apexClassPath).getLines().toArray[String]
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
        assert(lineNumber > 0, "could not find line number by marker: " + config.lineMarker)

        val extraParams = Array[String]("--column=" + config.column, "--line=" + lineNumber)
        val runner = new Executor()
        runner.execute(commandLineParams ++ extraParams)
        //validate result
        val lines = FileUtils.readFile(responseFile).getLines().toArray[String]

        assertResult("RESULT=SUCCESS")(lines(0))

        val (matchFunc, expectedItemsList) = config.identities match {
            case Some(_identities) => (matchIdentity(_identities)_, _identities)
            case None => config.signatureContains match {
              case Some(signatures) => (matchSignatureContains(signatures)_, signatures)
              case None => throw new IllegalStateException("unexpected config: " + config)
            }
            case _ => throw new IllegalStateException("unexpected config: " + config)
        }

        val completionResult = collectFoundItems(config.lineMarker, lines, matchFunc)
        //println(lines.mkString("\n"))
        //"check that all expected items  found")
        val diff = expectedItemsList.toSet.--(completionResult.matchingItemsSet)
        assert(diff.isEmpty, "Found less items than expected, Scenario: " + config.lineMarker + "; \nMissing item(s): " + diff.mkString(", "))

        //check that minimum expected number of items found
        assert(config.itemsCountMin <= completionResult.itemsTotal, "Scenario: " + config.lineMarker + s"; \nExpected ${config.itemsCountMin } items, actual ${completionResult.itemsTotal} " )

        //check signatureMustNotContain
        config.signatureMustNotContain match {
          case Some(signatures) =>
              val completionResult = collectFoundItems(config.lineMarker, lines, matchSignatureMustNotContain(signatures))
              assert(completionResult.matchingItemsSet.isEmpty, "Scenario: " + config.lineMarker + "; did not expect to find following signatures: " + completionResult.matchingItemsSet)
          case None =>
        }

        //check identityMustNotContain
        config.identityMustNotContain match {
            case Some(identities) =>
                val completionResult = collectFoundItems(config.lineMarker, lines, matchIdentityMustNotContain(identities))
                assert(completionResult.matchingItemsSet.isEmpty, "Scenario: " + config.lineMarker + "; did not expect to find following identities: " + completionResult.matchingItemsSet)
            case None =>
        }

        //check docContains
        config.docContains match {
            case Some(strings) =>
                val completionResult = collectFoundItems(config.lineMarker, lines, matchDocContains(strings))
                assert(completionResult.matchingItemsSet.nonEmpty, "Scenario: " + config.lineMarker + "; did not find some or all of the following docs: " + strings)
            case None =>
        }
        //check docMustNotContain
        config.docMustNotContain match {
            case Some(strings) =>
                val completionResult = collectFoundItems(config.lineMarker, lines, matchDocMustNotContain(strings))
                assert(completionResult.matchingItemsSet.isEmpty, "Scenario: " + config.lineMarker + "; did not expect to find following docs: " + completionResult.matchingItemsSet)
            case None =>
        }
    }
    //find exact item identity in the list of all exected identities
    private def matchIdentity(identities: List[String])(item: CompletionItem): String = {
        if (identities.contains(item.identity)) item.identity else ""
    }
    private def matchIdentityMustNotContain(identities: List[String])(item: CompletionItem): String = {
        if (matchIdentity(identities)(item).nonEmpty) item.signature else ""
    }
    private def matchSignatureContains(signatureSubstrings: List[String])(item: CompletionItem): String = {
        matchContains(signatureSubstrings, item => item.signature)(item)
    }

    private def matchSignatureMustNotContain(signatureSubstrings: List[String])(item: CompletionItem): String = {
        if (matchSignatureContains(signatureSubstrings)(item).nonEmpty) item.signature else ""
    }
    private def matchDocContains(docSubstrings: List[String])(item: CompletionItem): String = {
        matchContains(docSubstrings, item => item.doc)(item)
    }
    private def matchDocMustNotContain(docSubstrings: List[String])(item: CompletionItem): String = {
        if (matchDocContains(docSubstrings)(item).nonEmpty) item.doc else ""
    }

    private def matchContains(substrings: List[String], textExtractor: CompletionItem => String)(item: CompletionItem): String = {
        var longestMatch = ""
        for(substring <- substrings) {
            if (textExtractor(item).contains(substring)) {
                if (substring.length > longestMatch.length) {
                    longestMatch = substring
                    substring
                }
            }
        }
        longestMatch
    }

    case class CompletionResult(itemsTotal: Int, matchingItemsSet: Set[String])

    private def collectFoundItems(testName: String, responseLines: Array[String], matchFunc: CompletionItem => String): CompletionResult = {

        var i=1
        val foundItems = Set.newBuilder[String]
        while (i < responseLines.size) {
            val lineJson = responseLines(i)

            val jsonAst = Try(JsonParser(lineJson)) match {
                case Success(_ast) => _ast
                case Failure(ex) => throw new IllegalStateException("Failed to parse JSON response in test: " + testName, ex)
            }
            val completionItem = jsonAst.convertTo[CompletionItem](ApexModelJsonProtocol.testCompletionItemFormat)

            val matchedString = matchFunc(completionItem)
            if (matchedString.nonEmpty) {
                foundItems += matchedString
            }

            i += 1
        }
        new CompletionResult(i-1, foundItems.result())
    }
    private def escapeFilePath(file: File): String = {
        escapeFilePath(file.getAbsolutePath)
    }
    private def escapeFilePath(filePath: String): String = {
        "\"" + filePath + "\""
    }

    test("SObject Creator completions, i.e. completions inside expressions like: new Account(...<caret> )") {
        val testApexClassFilePath = projectPath + "/src/classes/SObjectCompletions.cls"
        testCompletionsInFile(testApexClassFilePath)
    }

    test("Standard Apex Library completions, i.e. completions inside expressions like: String.<caret>") {
        val testApexClassFilePath = projectPath + "/src/classes/ApexLibraryCompletions.cls"
        testCompletionsInFile(testApexClassFilePath)
    }

    test("Apex code completions, i.e. completions involving user defined Apex classes") {
        val testApexClassFilePath = projectPath + "/src/classes/ApexClassCompletions.cls"
        testCompletionsInFile(testApexClassFilePath)
    }

    test("SOQL completions, i.e. completions involving SQOL statements") {
        val testApexClassFilePath = projectPath + "/src/classes/SOQLCompletions.cls"
        testCompletionsInFile(testApexClassFilePath)
    }
}
