/*
 * Copyright (c) 2014 Andrey Gavrikov.
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

package com.neowit.apex.actions

import com.neowit.utils._
import com.neowit.apex._

import scala.collection.mutable
import scala.concurrent.{ExecutionContext, Future}

class ActionError(msg: String) extends Error(msg: String)
class UnsupportedActionError(msg: String) extends ActionError(msg: String)

object ActionFactory {
    private val REGISTERED_ACTIONS = Map[String, String](
        "version" -> "com.neowit.apex.AppVersion",
        "serverStart" -> "com.neowit.ServerStart",
        "login" -> "LoginOauth",
        "refresh" -> "RefreshMetadata",
        "listModified" -> "ListModified",
        "saveModified" -> "com.neowit.apex.actions.tooling.SaveModified",
        "saveSpecificFiles" -> "com.neowit.apex.actions.tooling.SaveSpecificFiles",
        "runTestsTooling" -> "com.neowit.apex.actions.tooling.RunTests",
        "loadApexCodeCoverageAggregate" -> "com.neowit.apex.actions.tooling.LoadApexCodeCoverageAggregate",
        "deployModified" -> "DeployModified",
        "deployAll" -> "DeployAll",
        "deploySpecificFiles" -> "DeploySpecificFiles",
        "listConflicts" -> "ListConflicting",
        "describeMetadata" -> "DescribeMetadata",
        "bulkRetrieve" -> "BulkRetrieve",
        "diffWithRemote" -> "DiffWithRemote",
        "listMetadata" -> "ListMetadata",
        "executeAnonymous" -> "ExecuteAnonymous",
        "deleteMetadata" -> "DeployDestructive",
        "deployModifiedDestructive" -> "DeployModifiedDestructive",
        "deployAllDestructive" -> "DeployAllDestructive",
        "scanSource" -> "ScanSource",
        "listCompletions" -> "ListCompletions",
        "findSymbol" -> "FindSymbol",
        "soqlQuery" -> "SoqlQuery",
        "changeLogLevels" -> "com.neowit.apex.actions.tooling.ChangeLogLevels",
        "deleteLogs" -> "com.neowit.apex.actions.tooling.DeleteLogs",
        "testSuiteManage" -> "TestSuiteActions",
        "checkSyntax" -> "CheckSyntax",
        "guessSetupUrl" -> "GuessSetupUrl"
    )
    def getActionNames: List[String] = REGISTERED_ACTIONS.keys.toList.sorted

    def getAction(basicConfig: BasicConfig, name: String, skipLoading: Boolean = false): Option[Action] = {

        //convert all keys to lower case
        val lowerCaseMap = REGISTERED_ACTIONS.map{case (key, value) => key.toLowerCase -> value}
        lowerCaseMap.get(name.toLowerCase) match {
          case Some(action) =>
              val fullClassName = if (action.indexOf(".") < 0) "com.neowit.apex.actions." + action else action
              val constructor = Class.forName(fullClassName).getConstructor()
              //Some(constructor.newInstance(basicConfig).asInstanceOf[Action])
              try {
                  val actionInstance = constructor.newInstance().asInstanceOf[Action]
                  if (!skipLoading) {
                      actionInstance.load(basicConfig)
                  }
                  Some(actionInstance)
              } catch {
                  case ex:MissingRequiredConfigParameterException => throw ex
                  case ex:ShowHelpException => throw ex
                  case ex:Throwable => throw ex
              }

          case None => throw new UnsupportedActionError("--action=" + name + " is not supported")
        }
    }

}
trait Action extends Logging {

    def execute()(implicit ec: ExecutionContext): Future[ActionResult] = {
        act().map{result =>
            finalise()
            result
        }
    }

    //this method should implement main logic of the action
    protected def act()(implicit ec: ExecutionContext): Future[ActionResult]

    //implement if need to execute some logic only after main action is complete, e.g. persist data to disk
    protected def finalise(): Unit

    def load[T <:Action](existingSession: Session): T

    /**
     * do NOT call this method if there is an existing Session, use alternative with existingSession parameter instead
     */
    def load[T <:Action](basicConfig: BasicConfig): T

    def getHelp: ActionHelp
}

abstract class AsyncAction extends Action {
    protected var _basicConfig: Option[BasicConfig] = None

    protected def basicConfig: BasicConfig = _basicConfig match {
      case Some(config) => config
      case None => throw new IllegalAccessError("call load(basicConfig) first")
    }

    override def load[T <:Action](basicConfig: BasicConfig): T = {
        _basicConfig = Some(basicConfig)
        this.asInstanceOf[T]
    }
    override def load[T <:Action](existingSession: Session): T = {
        load(existingSession.getConfig.basicConfig)
    }
}

abstract class ApexAction extends AsyncAction {
    private var _config: Option[Config] = None

    override def load[T <:Action](basicConfig: BasicConfig): T = {
        _config = Some(new Config(basicConfig))
        /*
        if (null != responseWriter) {
            // in case if command fails due to not command specific problem (e.g. can not get SFDC connection )
            // give Runner a chance to write into expected `--responseFile`
            basicConfig.setResponseWriter(responseWriter)
        }
        */
        this.asInstanceOf[T]
    }
    //need to def (as opposed to val) to stop from failing when called for help() display without session
    def config:Config = _config match {
        case Some(loadedConfig) => loadedConfig
        case None => throw new IllegalAccessError("call load(basicConfig) first")
    }

    def addToMap(originalMap: Map[String, Set[String]], key: String, value: String): Map[String, Set[String]] = {
        originalMap.get(key)  match {
            case Some(list) =>
                val newList: Set[String] = list + value
                originalMap ++ Map(key -> newList)
            case None => originalMap ++ Map(key -> Set(value))
        }
    }

    protected override def finalise(): Unit = { }
}
abstract class ApexActionWithProject extends ApexAction {
    private var _config: Option[ConfigWithSfdcProject] = None

    //need to def (as opposed to val) to stop from failing when called for help() display without session
    override def config:Config = getProjectConfig

    def getProjectConfig: ConfigWithSfdcProject = _config match {
        case Some(loadedConfig) => loadedConfig
        case None => throw new IllegalAccessError("call load(basicConfig) first")
    }
    override def load[T <:Action](basicConfig: BasicConfig): T = {
        _config = Some(new ConfigWithSfdcProject(basicConfig))
        this.asInstanceOf[T]
    }
}

abstract class ApexActionWithReadOnlySession extends ApexActionWithProject {
    protected var _session: Option[Session] = None
    protected def session: Session = _session match {
        case Some(s) => s //return existing session
        case None => throw new ShowHelpException(getHelp)
    }
    override def load[T <:Action](basicConfig: BasicConfig): T = {
        _session = Some(Session(basicConfig, isSessionReadOnly))
        this.asInstanceOf[T]
    }
    override def load[T <:Action](existingSession: Session): T = {
        _session = Some(existingSession)
        this.asInstanceOf[T]
    }

    //need to def (as opposed to val) to stop from failing when called for help() display without session
    override def config:Config = getSessionConfig

    def getSessionConfig: ConfigWithReadOnlySession = session.getConfig

    override def getProjectConfig: ConfigWithSfdcProject = getSessionConfig

    def isSessionReadOnly: Boolean = true
}

object ApexActionWithWritableSession {
    private val _currentActionByProject = new mutable.HashMap[String, ApexActionWithWritableSession]()

    private def getKey(action: ApexActionWithWritableSession): Option[String] = {
        action.getProjectConfig.projectDirOpt.map(_.getAbsolutePath)
    }

    def lockSession(action: ApexActionWithWritableSession):Unit = {
        getKey(action) match {
            case Some(key) =>
                _currentActionByProject += key -> action
            case None =>
        }
        ()
    }
    def unLockSession(action: ApexActionWithWritableSession):Unit = {
        getKey(action) match {
            case Some(key) =>
                _currentActionByProject -= key
            case None =>
        }
        ()
    }
    def isSessionLocked(action: ApexActionWithWritableSession): Boolean = {
        getKey(action) match {
            case Some(key) =>
                _currentActionByProject.contains(key)
            case None => false // no point to lock session if project path is not provided
        }
    }
}
abstract class ApexActionWithWritableSession extends ApexActionWithReadOnlySession {

    override def execute()(implicit ec: ExecutionContext): Future[ActionResult]  = {
        // make sure we do not have another action (which can modify the session of given project) already in progress
        if (!ApexActionWithWritableSession.isSessionLocked(this)) {
            try {
                ApexActionWithWritableSession.lockSession(this)
                super.execute()(ec)
            } catch {
                case ex: ConfigValueException =>
                    //ERROR(ex.getMessage)
                    Future.successful(ActionFailure(ex.getMessage))
                case ex: Throwable =>
                    //ERROR(ex)
                    //Future.successful(ActionFailure(ex.getMessage))
                    throw ex
            } finally {
                ApexActionWithWritableSession.unLockSession(this)
            }
        } else {
            //responseWriter.println("RESULT=FAILURE")
            //responseWriter.println(new Message(ERROR, "Session is locked. Another action is already in progress. Please wait for that action to complete..."))
            //ERROR("Session is locked. Another action is already in progress. Please wait for that action to complete...")
            Future.successful(ActionFailure("Session is locked. Another action is already in progress. Please wait for that action to complete..."))
        }
    }

    protected override def finalise(): Unit = {
        if (isSessionReadOnly) {
            //make sure that session data is saved to disk
            session.storeSessionData()
        }
    }

    override def isSessionReadOnly: Boolean = false
}

/*
abstract class ApexActionWithoutProject extends ApexAction {
    override def load[T <:Action](basicConfig: BasicConfig): T = {
        // fake "projectPath"
        val tempDir = FileUtils.createTempDir("temp")
        basicConfig.setProperty("projectPath", tempDir.getAbsolutePath)
        try {

            super.load(basicConfig)
        } finally {
            tempDir.delete()
        }
    }
    // make sure session does not get touched
    protected override def finalise(): Unit = {}
}
*/

class ShowHelpException(val help: ActionHelp, val message: String = "") extends IllegalStateException

trait ActionHelp {
    def getName: String
    def getSummary: String
    def getParamNames: List[String]
    def getParamDescription(paramName: String): String
    def getExample: String
}

abstract class AbstractActionHelp(parentActionHelp: ActionHelp) extends ActionHelp {
    def getParentActionHelp: ActionHelp = parentActionHelp
}





