package com.neowit.auth

import com.neowit.utils.{ConfigWithSfdcProject, Logging}

import scala.sys.process._
import scala.util.matching.Regex

case class OAuthToken(accessToken: String, instanceUrl: String)

object SfCliAuth extends Logging {
    private def isWindows: Boolean = System.getProperty("os.name").toLowerCase.contains("win")

    def buildCommand(cliPath: String, orgAlias: String, windows: Boolean = isWindows): Seq[String] = {
        def quoteForCmd(value: String): String =
            if (value.exists(_.isWhitespace) || value.exists("()&^|<>".contains(_))) "\"" + value + "\""
            else value
        if (windows) {
            val inner = quoteForCmd(cliPath) + " org display --target-org " + quoteForCmd(orgAlias) + " --json"
            Seq("cmd.exe", "/S", "/C", inner)
        } else Seq(cliPath, "org", "display", "--target-org", orgAlias, "--json")
    }

    def buildTokenCommand(cliPath: String, orgAlias: String, windows: Boolean = isWindows): Seq[String] = {
        def quoteForCmd(value: String): String =
            if (value.exists(_.isWhitespace) || value.exists("()&^|<>".contains(_))) "\"" + value + "\""
            else value
        if (windows) {
            val inner = quoteForCmd(cliPath) + " org auth show-access-token --target-org " + quoteForCmd(orgAlias) + " --json --no-prompt"
            Seq("cmd.exe", "/S", "/C", inner)
        } else Seq(cliPath, "org", "auth", "show-access-token", "--target-org", orgAlias, "--json", "--no-prompt")
    }

    private def extractField(json: String, name: String): Option[String] = {
        val field = ("\"" + Regex.quote(name) + "\"\\s*:\\s*\"((?:\\\\.|[^\"\\\\])*)\"").r
        field.findFirstMatchIn(json).map(_.group(1).replace("\\\"", "\"").replace("\\\\", "\\"))
    }

    def parseDisplayJson(json: String): OAuthToken = {
        val instanceUrl = extractField(json, "instanceUrl")
        instanceUrl match {
            case Some(url) => OAuthToken("", url)
            case _ =>
                val message = extractField(json, "message").getOrElse("sf org display returned incomplete JSON")
                val lowerMessage = message.toLowerCase
                if (lowerMessage.contains("authorization") || lowerMessage.contains("not authenticated"))
                    throw new RuntimeException(message + ". Run sf org login web --alias <alias>.")
                if (lowerMessage.contains("not found"))
                    throw new RuntimeException(message)
                throw new RuntimeException("sf org display JSON is missing result.instanceUrl")
        }
    }

    def parseTokenJson(json: String): OAuthToken =
        extractField(json, "accessToken") match {
            case Some(token) => OAuthToken(token, "")
            case None => throw new RuntimeException("sf org auth show-access-token JSON is missing result.accessToken")
        }

    def commandError(cliPath: String, orgAlias: String, exitCode: Int, stdout: String, stderr: String, command: String = "org display"): RuntimeException = {
        val combined = (stdout + " " + stderr).toLowerCase
        if (combined.contains("authorization information") || combined.contains("not authenticated") || combined.contains("not authorized"))
            new RuntimeException("Salesforce org '" + orgAlias + "' is not authenticated. Run sf org login web --alias " + orgAlias + ".")
        else new RuntimeException("sf " + command + " failed with exit code " + exitCode + ": " + stderr)
    }

    def executableError(cliPath: String, cause: java.io.IOException): RuntimeException =
        new RuntimeException("Salesforce CLI executable '" + cliPath + "' was not found or could not be started. Install sf CLI v2 or set sf.cliPath." +
            (if (isWindows) " On Windows, run 'where sf' or 'where sf.cmd' to locate it, or set sf.cliPath to the full path of sf.exe from the official installer (for example, under C:\\Program Files\\Salesforce CLI\\bin). The .cmd npm shim cannot be launched directly." else ""), cause)

    def fetchToken(config: ConfigWithSfdcProject): OAuthToken = {
        def run(command: Seq[String], name: String): String = {
            val stdout = new StringBuilder
            val stderr = new StringBuilder
            val processLogger = ProcessLogger(line => stdout.append(line).append("\n"), line => stderr.append(line).append("\n"))
            val exitCode = try Process(command).!(processLogger)
            catch { case error: java.io.IOException => throw executableError(config.cliPath, error) }
            if (exitCode != 0) throw commandError(config.cliPath, config.orgAlias, exitCode, stdout.toString, stderr.toString, name)
            stdout.toString
        }
        val display = try parseDisplayJson(run(buildCommand(config.cliPath, config.orgAlias), "org display"))
        catch {
            case error: RuntimeException if error.getMessage.contains("--alias <alias>") =>
                throw new RuntimeException(error.getMessage.replace("--alias <alias>", "--alias " + config.orgAlias))
        }
        val token = parseTokenJson(run(buildTokenCommand(config.cliPath, config.orgAlias), "org auth show-access-token"))
        logger.info("Salesforce CLI org alias: " + config.orgAlias)
        logger.info("Salesforce instance URL: " + display.instanceUrl)
        OAuthToken(token.accessToken, display.instanceUrl)
    }
}
