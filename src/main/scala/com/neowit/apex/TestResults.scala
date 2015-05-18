package com.neowit.apex

import java.io.{PrintWriter, File}

import com.neowit.apex.actions.DescribeMetadata
import com.neowit.utils.{Config, FileUtils, ResponseWriter}
import com.neowit.utils.ResponseWriter.{MessageDetail, Message}

import scala.util.parsing.json.{JSONArray, JSONObject}


trait RunTestsResult {
    def getCodeCoverage: Array[CodeCoverageResult]
    def getCodeCoverageWarnings: Array[CodeCoverageWarning]
}
trait CodeCoverageResult {
    def getNumLocations: Int
    def getNumLocationsNotCovered: Int
    def getName: String
    def getLocationsNotCovered: Array[CodeLocation]
}
trait CodeLocation {
    def getLine: Int
}

trait CodeCoverageWarning {
    def getName: String
    def getMessage: String
}

object TestResults {

    /**
     * process code coverage results
     * @param runTestResult - deployDetails.getRunTestResult
     * @return set of file names (e.g. MyClass) for which we had coverage
     */
    def processCodeCoverage(runTestResult: com.neowit.apex.RunTestsResult, session: Session,
                                    responseWriter: ResponseWriter ): Option[File] = {
        val config: Config = session.getConfig
        val metadataByXmlName = DescribeMetadata.getMap(session)
        val (classDir, classExtension) = metadataByXmlName.get("ApexClass") match {
            case Some(describeObject) => (describeObject.getDirectoryName, describeObject.getSuffix)
            case None => ("classes", "cls")
        }
        val (triggerDir, triggerExtension) = metadataByXmlName.get("ApexTrigger") match {
            case Some(describeObject) => (describeObject.getDirectoryName, describeObject.getSuffix)
            case None => ("triggers", "trigger")
        }
        //only display coverage details of files included in deployment package
        val coverageDetails = new Message(ResponseWriter.WARN, "Code coverage details")
        val hasCoverageData = runTestResult.getCodeCoverage.nonEmpty
        var coverageFile: Option[File] = None
        val coverageWriter = config.getProperty("reportCoverage").getOrElse("false") match {
            case "true" if hasCoverageData =>
                coverageFile = Some(FileUtils.createTempFile("coverage", ".txt"))
                val writer = new PrintWriter(coverageFile.get)
                Some(writer)
            case _ => None
        }

        if (runTestResult.getCodeCoverage.nonEmpty) {
            responseWriter.println(coverageDetails)
        }

        val reportedNames = Set.newBuilder[String]
        val coverageRelatedMessages = Map.newBuilder[String, MessageDetail]
        for ( coverageResult <- runTestResult.getCodeCoverage) {
            reportedNames += coverageResult.getName
            val linesCovered = coverageResult.getNumLocations - coverageResult.getNumLocationsNotCovered
            val coveragePercent = if (coverageResult.getNumLocations > 0) linesCovered * 100 / coverageResult.getNumLocations else 0
            coverageRelatedMessages += coverageResult.getName -> new MessageDetail(coverageDetails,
                Map("text" ->
                    (coverageResult.getName +
                        ": lines total " + coverageResult.getNumLocations +
                        "; lines not covered " + coverageResult.getNumLocationsNotCovered +
                        "; covered " + coveragePercent + "%"),
                    "type" -> (if (coveragePercent >= 75) ResponseWriter.INFO else ResponseWriter.WARN)
                )
            )

            coverageWriter match {
                case Some(writer) =>
                    val filePath = session.getRelativePath(classDir, coverageResult.getName + "." + classExtension) match {
                        case Some(relPath) => Some(relPath)
                        case None =>
                            //check if this is a trigger name
                            session.getRelativePath(triggerDir, coverageResult.getName + "." + triggerExtension) match {
                                case Some(relPath) => Some(relPath)
                                case None => None
                            }
                    }

                    filePath match {
                        case Some(relPath) =>
                            val locations = List.newBuilder[Int]
                            for (codeLocation <- coverageResult.getLocationsNotCovered) {
                                locations += codeLocation.getLine
                            }
                            val coverageJSON = JSONObject(Map("path" -> relPath, "linesTotalNum" -> coverageResult.getNumLocations,
                                "linesNotCoveredNum" -> coverageResult.getNumLocationsNotCovered,
                                "linesNotCovered" -> JSONArray(locations.result().toList)))
                            // end result looks like so:
                            // {"path" : "src/classes/AccountController.cls", "linesNotCovered" : [1, 2, 3,  4, 15, 16,...]}
                            writer.println(coverageJSON.toString(ResponseWriter.defaultFormatter))
                        case None =>
                    }
                case None =>
            }
        }
        //dump coverage related MessageDetail-s into the response file, making sure that messages are sorted by file name
        val messageDetailsSortedByFileName = coverageRelatedMessages.result().toSeq.sortBy(_._1.toLowerCase).map(_._2).toList
        responseWriter.println(messageDetailsSortedByFileName)

        val coverageMessage = new Message(ResponseWriter.WARN, "Code coverage warnings")
        if (runTestResult.getCodeCoverageWarnings.nonEmpty) {
            responseWriter.println(coverageMessage)
        }
        val reportedNamesSet = reportedNames.result()
        for ( coverageWarning <- runTestResult.getCodeCoverageWarnings) {
            if (null != coverageWarning.getName) {
                if (!reportedNamesSet.contains(coverageWarning.getName)) {
                    responseWriter.println(new MessageDetail(coverageMessage, Map("text" -> (coverageWarning.getName + ": " + coverageWarning.getMessage))))
                }
            } else {
                responseWriter.println(new MessageDetail(coverageMessage, Map("text" -> coverageWarning.getMessage)))
            }

        }

        coverageWriter match {
            case Some(writer) =>
                writer.close()
            case _ =>
        }
        coverageFile
    }
}