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

package com.neowit.response.protocols.vim

import java.io.{File, FileOutputStream, OutputStream, PrintWriter}

import com.neowit.apex.actions.{ActionFailure, ActionResult, ActionSuccess}
import com.neowit.response._
import com.neowit.utils.JsonSupport
import spray.json._

trait VimProtocol[A <: BaseResult] {
    def send(result: A): Unit
}
class ResponseWriterVim(out: OutputStream, autoFlush: Boolean = true, append: Boolean = false) extends ResponseWriter with JsonSupport {
    private val _writer = new PrintWriter(out, autoFlush)

    var needClosing = false

    def this(file: File) {
        this(new FileOutputStream(file), autoFlush = true, append = false)
    }
    def this(file: File, append: Boolean) {
        this(new FileOutputStream(file), autoFlush = true, append)
    }

    override def send(msg: Message): Message = println(msg)

    override def send(msg: String): Unit = println(msg)

    override def send(msg: RESULT): Unit = println(msg)


    private def println(result: RESULT): Unit = {
        result match {
            case SUCCESS => this.println("RESULT=SUCCESS")
            case FAILURE => this.println("RESULT=FAILURE")
        }
    }
    private def println(p1: String): Unit = {
        _writer.println(p1)
        needClosing = true
        logger.debug(p1)
    }
    private def println(msg: Message): Message = {
        msg match {
            case KeyValueMessage(data) =>
                data.foreach{
                    case (key, value) => println(key + "=" + value)
                }
            case _ =>
                println("MESSAGE: " + msg.toJson.compactPrint)
        }
        val details =
            msg match {
                case InfoMessage(_, _, _details, _) if _details.nonEmpty => _details
                case WarnMessage(_, _, _details, _) if _details.nonEmpty => _details
                case ErrorMessage(_, _, _details, _) if _details.nonEmpty => _details
                case DebugMessage(_, _, _details, _) if _details.nonEmpty => _details
                case _ => Nil
            }
        if (details.nonEmpty) {
            println(details)
        }
        msg
    }
    def println(messageDetail: MessageDetailMap): Unit = {
        println("MESSAGE DETAIL: " + messageDetail.toJson.compactPrint)
    }
    def println(messageDetail: MessageDetailText): Unit = {
        println("MESSAGE DETAIL: " + messageDetail.text)
    }
    def println(messageDetails: List[MessageDetail]): Unit = {
        messageDetails.foreach{
            case msg @ MessageDetailMap(_, _) => println(msg)
            case msg @ MessageDetailText(_, _) => println(msg)
        }
    }
    def send(prefix: String, data: Map[String, Any]): Unit = {
        println(prefix + ": " + ResponseWriter.ensureNoNulls(data).toJson.compactPrint)
    }
    def println(data: Map[String, Any]): Unit = {
        send("", data)
    }
    def startSection(sectionName: String): String = {
        println("#SECTION START: " + sectionName)
        sectionName
    }
    def endSection(sectionName: String): Unit = {
        println("#SECTION END: " + sectionName)
    }

    private def println(result: BaseResult): Unit = {
        result match {
            case FindSymbolResult(Some(member)) => println(member.serialise.compactPrint)
            case FindSymbolResult(None) => // do nothing
            case ListCompletionsResult(members) => println(members.map(_.toJson).mkString("\n"))
            case res @ AppVersionResult(_, _, _, _, _) => new AppVersion(this).send(res)
            case res @ BulkRetrieveActionResult(_, _, _) => new BulkRetrieve(this).send(res)
            case res @ CheckSyntaxResult(_, _) => new CheckSyntax(this).send(res)
            case res @ DeployAllDestructiveResult(_, _) => new DeployAllDestructive(this).send(res)
            case res @ DeployAllResult(_) => new DeployAll(this).send(res)
            case res @ DeployDestructiveResult(_) => new DeployDestructive(this).send(res)
            case res @ DeployModifiedDestructiveResult(_) => new DeployModifiedDestructive(this).send(res)
            case res @ DeployModifiedResult(_) => new DeployModified(this).send(res)
            case res @ DiffWithRemoteResult(_) => new DiffWithRemote(this).send(res)
            case res @ ExecuteAnonymousResult(_, _, _, _) => new ExecuteAnonymous(this).send(res)
            case res @ ListConflictingResult(_) => new ListConflicting(this).send(res)
            case res @ ListModifiedResult(_, _) => new ListModified(this).send(res)
            case res @ LoginOauthResult(_, _) => new LoginOauth(this).send(res)
            case res @ RefreshMetadataResult(_, _) => new RefreshMetadata(this).send(res)
            case res @ RunTestsResult(_, _, _, _, _) => new RunTests(this).send(res)
            case res @ SoqlQueryResult(_) => new SoqlQuery(this).send(res)
            case res @ LoadApexCodeCoverageAggregateResult(_) => new RunTests(this).send(res)
            case res @ GuessSetupUrlResult(_) => new GuessSetupUrl(this).send(res)
        }
    }

    def sendResponse(result: ActionResult): Unit = {
        result match {
            case ActionSuccess(messages, None) =>
                println(SUCCESS)
                messages.foreach(println(_))
            case ActionSuccess(messages, Some(_result)) =>
                println(SUCCESS)
                messages.foreach(println(_))
                println(_result)
            case ActionFailure(messages, None) =>
                println(FAILURE)
                messages.foreach(println(_))
            case ActionFailure(messages, Some(_result)) =>
                println(FAILURE)
                messages.foreach(println(_))
                println(_result)
        }
    }

    def close(): Unit = {
        if (needClosing)
            _writer.close()
    }

}
