package com.neowit.apex

import java.io.File
import java.util.Properties

import com.neowit.utils.FileUtils
import org.scalatest.FunSuite
import spray.json._

import scala.util.{Failure, Success, Try}

//import DefaultJsonProtocol._

class GoToSymbolTest extends FunSuite {

    private val is = getClass.getClassLoader.getResource("paths.properties").openStream()
    val paths = new Properties()
    paths.load(is)

    private val projectPath = paths.getProperty("gotoSymbol.projectPath")
    private val loginCredentialsPath = paths.getProperty("loginCredentialsPath")

    val commandLine = Array(
        s"--config=${escapeFilePath(loginCredentialsPath)}"
        ,"--action=findSymbol"
        ,s"--projectPath=${escapeFilePath(projectPath)}"
        ,s"--reportUsage=0"

    )

    def withResponseFile(testCode: (File) => Any): Unit = {
        val responseFile = File.createTempFile("findSymbol", ".json")
        try {
            testCode(responseFile) // "loan" the fixture to the test
        } finally {
            // clean up the fixture
            responseFile.delete()
        }
        Unit
    }
    case class TestConfig(lineMarker: String, column: Int, itemsCountMin: Int,
                          identities: Option[List[String]],
                          identityContains: Option[List[String]],
                          identityMustNotContain: Option[List[String]]
                         ) {
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

    case class LocationItem(filePath: String, line: Int, column: Int, identity: String)

    object ApexModelJsonProtocol extends DefaultJsonProtocol {
        implicit val testConfigFormat = jsonFormat6(TestConfig)
        implicit val testLocationItemFormat = jsonFormat4(LocationItem)
    }


    /**
     * main method which does actual validations
     * @param apexClassPath - full path to the file where test apex code and find-symbol scenarios reside
     */
    private def testFindSymbolResultsInFile(apexClassPath: String): Unit = {

        withResponseFile { (responseFile) =>
            val extraParams = Array(
                s"--currentFilePath=${escapeFilePath(apexClassPath)}",
                s"--responseFilePath=${escapeFilePath(responseFile)}",
                s"--currentFileContentPath=${escapeFilePath(apexClassPath)}"
            )
            val lines = FileUtils.readFile(apexClassPath).getLines().toArray[String]
            var i = 0
            while (i < lines.length) {
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
                    runSingleFindSymbolTest(jsonTestDescription, commandLine ++ extraParams, apexClassPath, responseFile)
                }
                i += 1

            }
        }
    }

    def runSingleFindSymbolTest(jsonConfigStr: String, commandLineParams: Array[String], apexClassPath: String, responseFile: File): Unit = {
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
            case None => config.identityContains match {
              case Some(signatures) => (matchIdentityContains(signatures)_, signatures)
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

        //check identityMustNotContain
        config.identityMustNotContain match {
            case Some(identities) =>
                val completionResult = collectFoundItems(config.lineMarker, lines, matchIdentityMustNotContain(identities))
                assert(completionResult.matchingItemsSet.isEmpty, "Scenario: " + config.lineMarker + "; did not expect to find following identities: " + completionResult.matchingItemsSet)
            case None =>
        }

    }
    //find exact item identity in the list of all expected identities
    private def matchIdentity(identities: List[String])(item: LocationItem): String = {
        if (identities.contains(item.identity)) item.identity else ""
    }
    private def matchIdentityMustNotContain(identitySubstrings: List[String])(item: LocationItem): String = {
        if (matchIdentityContains(identitySubstrings)(item).nonEmpty) item.identity else ""
    }
    private def matchIdentityContains(identitySubstrings: List[String])(item: LocationItem): String = {
        matchContains(identitySubstrings, item => item.identity)(item)
    }

    private def matchContains(substrings: List[String], textExtractor: LocationItem => String)(item: LocationItem): String = {
        var longestMatch = ""
        for(substring <- substrings) {
            if (textExtractor(item).replaceAll(" ", "").contains(substring.replaceAll(" ", ""))) {
                if (substring.length > longestMatch.length) {
                    longestMatch = substring
                    substring
                }
            }
        }
        longestMatch
    }

    case class CompletionResult(itemsTotal: Int, matchingItemsSet: Set[String])

    private def collectFoundItems(testName: String, responseLines: Array[String], matchFunc: LocationItem => String): CompletionResult = {
        import ApexModelJsonProtocol._

        var i=1
        val foundItems = Set.newBuilder[String]
        var totalItemsCount = 0
        for (
            lineJson <- responseLines.drop(1) // drop "RESULT=SUCCESS" line
        ) {
            val jsonAst = Try(JsonParser(lineJson)) match {
                case Success(_ast) => _ast
                case Failure(ex) => throw new IllegalStateException("Failed to parse JSON response in test: " + testName, ex)
            }
            val completionItems = jsonAst.convertTo[List[LocationItem]]
            totalItemsCount += completionItems.length

            for (
                completionItem <- completionItems
            ) {
                val matchedString = matchFunc(completionItem)
                if (matchedString.nonEmpty) {
                    foundItems += matchedString
                }

            }
        }
        new CompletionResult(totalItemsCount, foundItems.result())
    }
    private def escapeFilePath(file: File): String = {
        escapeFilePath(file.getAbsolutePath)
    }
    private def escapeFilePath(filePath: String): String = {
        "\"" + filePath + "\""
    }

    test("Test single method, same class") {
        val testApexClassFilePath = projectPath + "/src/classes/GoToSymbolTest.cls"
        testFindSymbolResultsInFile(testApexClassFilePath)
    }

}
