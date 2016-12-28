package com.neowit.apex.actions.tooling

import java.io.File
import java.nio.file.{Files, Path}

import com.neowit.apex.Session
import com.neowit.apex.actions.{ActionHelp, ApexActionWithReadOnlySession}
import com.neowit.apex.completion.models.{ApexModelJsonProtocol, ApexType}
import com.neowit.utils.{FileUtils, Logging}
import spray.json._

import scala.util.{Failure, Success, Try}
/**
  * Author: Andrey Gavrikov
  */
class DownloadApexModel extends ApexActionWithReadOnlySession with StandardApexModelJson with ApexModelJsonProtocol with Logging {
    override def getHelp: ActionHelp = new ActionHelp {
        override def getExample: String = ""

        override def getParamDescription(paramName: String): String = {
            paramName match {
                case "outputDirPath" => "--outputDirPath=/path/to/dir where to store extracted json file"
                case _ => ""
            }
        }

        override def getParamNames: List[String] = List("outputDirPath")

        override def getSummary: String = "Download Apex Model definitions to local file(s) in JSON format. This is for internal purposes only, end users does not need to use this command"

        override def getName: String = "DownloadApexModel"
    }

    override protected def act(): Unit = {
        download(session).map { apexJson =>
            getOutputDir.map{outputDirPath =>
                apexJson.publicDeclarations.map{
                    case (namespace, classByName) => convertNamespace(namespace, classByName, outputDirPath)
                }
            }
        }

    }

    private def download(session: Session): Try[ApexJson] = {
        session.getRestContentTooling("/completions", urlParameters = "type=apex", httpHeaders = Map("ACCEPT" -> "application/json")) match {
            case Some(json) =>
                parseJson(json)
            case None => Failure(throw new IllegalStateException("Call to Apex Model resulted in nothing"))
        }
    }

    private def parseJson(json: String): Try[ApexJson] = {
        Try(json.parseJson.convertTo[ApexJson])
    }

    private def getOutputDir: Try[Path] = {
        config.getRequiredProperty("outputDirPath") match {
            case Some(dirPath) =>
                val dir = new File(dirPath)
                if (!dir.exists()) {
                    Try(Files.createDirectory(dir.toPath))
                } else {
                    Success(dir.toPath)
                }
            case None => Failure(new IllegalArgumentException("missing '--outputDirPath' parameter"))
        }

    }

    private def convertNamespace(name: String, classByName: Map[String, ApexStandardNamespaceClass], outputDirPath: Path): Try[File] = {

        val apexTypes =
            classByName.map{
                case (className, apexClass) => (className, convertClassFormat(name, className, apexClass))
            }
        val fileContent = apexTypes.toJson.prettyPrint
        val outFile = new File(outputDirPath.toFile, name + ".json")
        FileUtils.writeFile(fileContent, outFile)
        Success(outFile)

    }

    private def convertClassFormat(namespace: String, name: String, apexClass: ApexStandardNamespaceClass): ApexType = {
        val methodsOpt = if (apexClass.methods.isEmpty) None else Option(apexClass.methods.map(_.toApexMethod))
        val ctorsOpt = if (apexClass.constructors.isEmpty) None else Option(apexClass.constructors.map(_.toApexCtor(name)))

        val apexType =
            new ApexType(
                name = name,
                superType = None,
                enums = None,
                methods = methodsOpt,
                tag = "CLASSDEF",
                ctors = ctorsOpt,
                fqn = namespace + "." + name
            )

        apexType
    }
}
