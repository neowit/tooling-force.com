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

package com.neowit.apex.lsp

import java.io.{File, InputStream, OutputStream}
import java.nio.file.{FileSystems, Path}

import com.neowit.apex.lsp.WorkspaceCommand._
import com.neowit.apex.{ProjectsCache, Session}
import com.neowit.apexscanner.Project
import com.neowit.apexscanner.server.LanguageServerDefault
import com.neowit.apexscanner.server.protocol.messages._
import com.neowit.utils.{BasicConfig, FileUtils}

import scala.concurrent.{ExecutionContext, Future}

/**
  * Created by Andrey Gavrikov 
  */
object ApexLanguageServerBase {
    /**
      * given some path - try to find project dir
      * @param path project path or src folder path or class path, etc
      * @return
      */
    def findProjectPath(path: Path): Option[Path] = {
        FileUtils.findSrcFolder(path.toFile) match {
            case Some(srcFolder) => Option(srcFolder.getParentFile.toPath)
            case None =>
                if (new File(path.toString, "src").isDirectory) {
                    // provided path points to project root
                    Option(path)
                } else {
                    None
                }
        }
    }

    def findProjectName(path: Path): Option[String] = {
        findProjectPath(path).map(_.getFileName.toString)
    }
}
class ApexLanguageServerBase(inputStream: InputStream, outputStream: OutputStream, config: LanguageServerConfig)(implicit val ex: ExecutionContext)
                    extends LanguageServerDefault(inputStream, outputStream) with LSPJsonSupport {

    override protected def initialiseProjectImpl(params: MessageParams.InitializeParams): Either[String, Project] = {
        params.initializationOptions match {
            case Some(json) =>
                json.as[InitializationOptions] match {
                    case Right(options) => createProject(params, options)
                    case Left(err) => throw new IllegalArgumentException("Failed to parse initializationOptions: " + err.message)
                }
            case None =>
                createProject(params, InitializationOptions(authConfigPath = None))
        }
    }

    override def getServerCapabilities: ServerCapabilities = {
        val commands = Seq(
            ToolingVersion,
            ApexDeploy
        )
        val supportedCommands = ExecuteCommandOptions(commands.map(_.name))
        super.getServerCapabilities.copy(executeCommandProvider = supportedCommands)
    }

    private def createProject(params: MessageParams.InitializeParams, options: InitializationOptions): Either[String, Project] = {
        Project.findApexProjectRoot(params.rootUri.path) match {
            case Some(projectPath) =>
                val authConfigurationFilePath = getAuthConfigFilePath(options, projectPath, config)
                authConfigurationFilePath match {
                    case Some(authConfigPath) =>
                        val authConfig = new File(authConfigPath)
                        if (authConfig.canRead) {
                            val basicConfig: BasicConfig = new BasicConfig()
                            basicConfig.setProperty("authConfigPath", authConfigPath)
                            //val config: ConfigWithSfdcProject = new ConfigWithSfdcProject(basicConfig)
                            val session: Session = new Session(basicConfig, isReadOnly = true)
                            Right(ProjectsCache.getProject(projectPath.toFile, session, loadStdLib = true, loadSobjectLib = true))
                        } else {
                            val err = "Failed to create project - authConfigPath is invalid or not readable: " + authConfigPath
                            logger.error(err)
                            Left(err)
                        }
                    case None =>
                        val err = "Failed to create project - auth Configuration Path is required. "
                        logger.error(err)
                        Left(err)
                }
            case None =>
                val err = "Failed to create project - InitializeParams.rootUri.path is missing: "
                logger.error(err)
                Left(err)
        }
    }

    private def getAuthConfigFilePath(options: InitializationOptions, projectPath: Path, config: LanguageServerConfig): Option[String] = {
        val authConfigPath = options.authConfigPath.getOrElse(config.authConfigPath)

         if (null != authConfigPath && authConfigPath.nonEmpty) {
             Option(authConfigPath)
         } else {
             //fall back to config.authConfigDirPath / <projectName>
             ApexLanguageServerBase.findProjectName(projectPath) match {
                 case Some(projectName) =>
                     if (config.authConfigDirPath.nonEmpty && null != projectName && projectName.nonEmpty) {
                         val authConfigurationFilePath = FileSystems.getDefault.getPath(config.authConfigDirPath, projectName)
                         Option(authConfigurationFilePath.toString)
                     } else {
                         None
                     }
                 case None =>
                     None
             }
         }
    }

    override def executeCommand(messageId: Int, params: MessageParams.ExecuteCommandParams, projectOpt: Option[Project]): Future[Either[ResponseError, ResponseMessage]] = {
        logger.info("TODO: execute command: " + params.command + " with arguments: " + params.arguments + " in project: " + projectOpt.map(_.path).getOrElse(""))
        super.executeCommand(messageId, params, projectOpt)
    }
    override def executeCommand(messageId: Int, command: String): Future[Either[ResponseError, ResponseMessage]] = {
        logger.info("TODO: execute command: " + command + " without arguments")
        Future.successful(Right(ResponseMessage(messageId, result = None, error = None)))
    }
}
