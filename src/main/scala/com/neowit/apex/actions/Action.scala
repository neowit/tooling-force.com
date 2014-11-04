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


class ActionError(msg: String) extends Error(msg: String)
class UnsupportedActionError(msg: String) extends ActionError(msg: String)

object ActionFactory {
    val REGISTERED_ACTIONS = Map[String, String](
        "version" -> "com.neowit.apex.AppVersion",
        "serverStart" -> "com.neowit.ServerStart",
        "refresh" -> "RefreshMetadata",
        "listModified" -> "ListModified",
        "saveModified" -> "com.neowit.apex.actions.tooling.SaveModified",
        "deployModified" -> "DeployModified",
        "deployAll" -> "DeployAll",
        "deploySpecificFiles" -> "DeploySpecificFiles",
        "listConflicts" -> "ListConflicting",
        "describeMetadata" -> "DescribeMetadata",
        "bulkRetrieve" -> "BulkRetrieve",
        "listMetadata" -> "ListMetadata",
        "executeAnonymous" -> "ExecuteAnonymous",
        "deleteMetadata" -> "DeployDestructive",
        "deployModifiedDestructive" -> "DeployModifiedDestructive",
        "scanSource" -> "ScanSource",
        "listCompletions" -> "ListCompletions"
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
    def act(): Unit

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
}

abstract class ApexAction extends AsyncAction {

    protected var _session: Option[Session] = None
    protected def session: Session = _session match {
        case Some(s) => s //return existing session
        case None => throw new ShowHelpException(getHelp)
    }
    override def load[T <:Action](basicConfig: BasicConfig): T = {
        _session = Some(Session(basicConfig))
        this.asInstanceOf[T]
    }

    //need to def (as opposed to val) to stop from failing when called for help() display without session
    def config:Config = session.getConfig
    def responseWriter: ResponseWriter = config.responseWriter

    def addToMap(originalMap: Map[String, Set[String]], key: String, value: String): Map[String, Set[String]] = {
        originalMap.get(key)  match {
            case Some(list) =>
                val newList: Set[String] = list + value
                originalMap ++ Map(key -> newList)
            case None => originalMap ++ Map(key -> Set(value))
        }
    }
}

class ShowHelpException(val help: ActionHelp) extends IllegalStateException

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





