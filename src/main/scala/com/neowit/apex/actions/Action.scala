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
        "refresh" -> "RefreshMetadata",
        "listModified" -> "ListModified",
        "deployModified" -> "DeployModified",
        "deployAll" -> "DeployAll",
        "deploySpecificFiles" -> "DeploySpecificFiles",
        "listConflicts" -> "ListConflicting",
        "describeMetadata" -> "DescribeMetadata",
        "bulkRetrieve" -> "BulkRetrieve",
        "listMetadata" -> "ListMetadata",
        "executeAnonymous" -> "ExecuteAnonymous"
    )
    def getActionNames: List[String] = REGISTERED_ACTIONS.keys.toList.sorted

    def getAction(session:Session, name: String): Option[Action] = {

        //convert all keys to lower case
        val lowerCaseMap = REGISTERED_ACTIONS.map{case (key, value) => key.toLowerCase -> value}
        lowerCaseMap.get(name.toLowerCase) match {
          case Some(action) =>
              val constructor = Class.forName("com.neowit.apex.actions." + action).getConstructor(classOf[Session])
              Some(constructor.newInstance(session).asInstanceOf[Action])
          case None => throw new UnsupportedActionError("--action=" + name + " is not supported")
        }
    }

}
trait Action extends Logging with ActionHelp {
    def act(): Unit
}
trait AsyncAction extends Action {
}

abstract class ApexAction(session: Session) extends AsyncAction {
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

trait ActionHelp {
    def getName: String
    def getSummary: String
    def getParamNames: List[String]
    def getParamDescription(paramName: String): String
    def getExample: String
}





