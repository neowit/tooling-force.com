package com.neowit.response

import com.neowit.apex.actions.ActionResult
import com.neowit.apex.parser.Member
import com.neowit.utils.JsonSupport
import spray.json._

/**
  * Author: Andrey Gavrikov
  * Date: 01/01/2017
  */
sealed abstract class BaseResult {

    def addToExisting(result: BaseResult): BaseResult = {
        // by default do nothing
        this
    }
    def addToExisting(result: ActionResult): BaseResult = {
        // by default do nothing
        this
    }
}

case class FindSymbolResult(memberOpt: Option[Member]) extends BaseResult with JsonSupport {
    def toJson: JsValue = memberOpt match {
        case Some(member) => member.serialise
        case None => Map.empty.toJson
    }
}

case class ListCompletionsResult(members: List[Member]) extends BaseResult

