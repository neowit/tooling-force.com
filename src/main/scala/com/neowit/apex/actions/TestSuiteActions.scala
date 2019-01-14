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

package com.neowit.apex.actions

import java.io.File

import com.neowit.apex.Session
import com.neowit.apex.actions.SoqlQuery.ResultRecord
import com.neowit.utils.FileUtils
import com.sforce.soap.tooling.sobject.{ApexClass, ApexTestSuite, TestSuiteMembership}
import spray.json._

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

/**
  * Author: Andrey Gavrikov (westbrook)
  * Date: 14/04/2016
  */
trait TestSuiteProtocol extends DefaultJsonProtocol {
    case class TestSuite(name: String, id: String, classes: List[ApexClass])

    implicit object ApexClassJsonFormat extends RootJsonFormat[ApexClass] {
        def write(c: ApexClass): JsValue =
            Map("Name" -> c.getName.toJson, "Id" -> c.getId.toJson).toJson

        def read(value: JsValue) = value match {
            case _ => deserializationError("ApexClass deserialisation is NOT supported")
        }
    }
    implicit val TestSuiteJsonFormat = jsonFormat3(TestSuite)
}

class TestSuiteActions extends ApexActionWithReadOnlySession with TestSuiteProtocol{

    override def getHelp: ActionHelp = new ActionHelp {
        override def getExample: String = ""
        override def getParamDescription(paramName: String): String = paramName match {
            case "testSuiteAction" =>
                """--testSuiteAction=action-name
                  |  Manage test suite using suite name provided by 'testSuiteName' parameter
                  |  Supported Actions:
                  |  - create, delete, addClasses, removeClasses, dump, dumpNames
                """.stripMargin
            case "testSuiteName" =>
                """--testSuiteName=test-suite-name
                  |  If 'testSuiteAction' is specified then this parameter is REQUIRED.
                  |  e.g. --testSuiteName=MySuite1
                """.stripMargin
            case "testSuiteClassNames" =>
                """--testSuiteClassNames=class-names-list
                  |  If 'testSuiteAction' is specified then this parameter is REQUIRED.
                  |  e.g. --testSuiteClassNames=MyClass1,MyClass2,MyClass1Test
                """.stripMargin
            case "dumpToFile" =>
                """--dumpToFile=/path/to/file.js
                  |  If 'testSuiteAction' is 'dump' or 'dumpNames' then this parameter is required.
                  |  Dump of current Test Suite structure or just Test Suite Names will bo saved in specified file
                """.stripMargin
        }

        override def getParamNames: List[String] = List("testSuiteAction", "testSuiteName", "testSuiteClassNames", "dumpToFile")

        override def getSummary: String = "Create|Update|Delete test suite by name"

        override def getName: String = "testSuiteManage"
    }

    //this method should implement main logic of the action
    override protected def act()(implicit ec: ExecutionContext): Future[ActionResult] = {
        val actionResult =
        config.getProperty("testSuiteAction") match {
            case Some(action) if Set("create", "addclasses", "removeclasses", "delete").contains(action.toLowerCase) =>
                config.getProperty("testSuiteName") match {
                    case None => Failure(new IllegalArgumentException("Invalid config for 'testSuiteAction': testSuiteName is required"))
                    case Some(testSuiteName) =>
                        action match {
                            case "create" => create(testSuiteName, session)
                            case "delete" => delete(List(testSuiteName), session)
                            case _action if Set("addClasses", "removeClasses").contains(_action) =>
                                getTestSuiteIdByName(testSuiteName, session) match {
                                    case Some(testSuiteId) =>
                                        _action match {
                                            case "addClasses" =>
                                                Try(config.getRequiredPropertyOpt("testSuiteClassNames")
                                                    .map(classNames => addClasses(testSuiteId, classNames.split(",").toList , session)))

                                            case "removeClasses" =>
                                                Try(config.getRequiredPropertyOpt("testSuiteClassNames")
                                                    .map(classNames => removeClasses(testSuiteId, classNames.split(",").toList , session)))
                                        }
                                    case None => Failure(new IllegalArgumentException(s"Invalid config for 'testSuiteAction', Test Suite with name '$testSuiteName' not found"))
                                }
                        }
                }
            case Some("dump") => dump(session)
            case Some("dumpNames") => dump(session, namesOnly = true)
            case _ =>
                Failure(new IllegalArgumentException("Invalid config for 'testSuiteAction'"))

        }

        actionResult match {
            case Success(_res) =>
                //responseWriter.println("RESULT=SUCCESS")
                Future.successful(ActionSuccess())
            case Failure(ex) =>
                //responseWriter.println("RESULT=FAILURE")
                //responseWriter.println(new ResponseWriter.Message(ERROR, ex.getMessage))
                Future.successful(ActionFailure(ex.getMessage))
        }
    }

    private def dump(session: Session, namesOnly: Boolean = false): Try[String] = {
        session.config.getProperty("dumpToFile") match {
            case Some(filePath) =>
                //val testSuites = TestSuiteActions.getTestSuiteList(session)
                //val testSuiteIds = testSuites.map(_.getId)
                val classesBySuite = getTestSuiteClassesByTestSuite(session)
                val text = if (namesOnly){
                    classesBySuite.keys.toList.sorted.toJson.compactPrint
                } else {
                    classesBySuite.toJson.compactPrint
                }
                Try(FileUtils.writeFile(text, new File(filePath), append = false)) match {
                    case Success(_) =>
                        Success(filePath)
                    case Failure(ex) => Failure(ex)
                }
            case None =>
                Failure(new IllegalArgumentException("Invalid config for 'testSuiteAction=dump', missing parameter 'dumpToFile' "))
        }
    }

    /**
      * @return map("testSuiteId" -> List(classId1, classId2, classId3, ...))
      */

    private def getTestSuiteClassesByTestSuite(session: Session): Map[String, TestSuite] = {
        val soql = s"select Id, ApexClass.Id, ApexTestSuite.Id, ApexClass.Name, ApexTestSuite.TestSuiteName from TestSuiteMembership"
        val memberships = SoqlQuery.getQueryIteratorTyped[com.sforce.soap.tooling.sobject.TestSuiteMembership](session, session.queryTooling(soql))
        val mapBuilder = Map.newBuilder[TestSuite, List[ApexClass]]

        val testSuitePerClass =
            memberships.map{membership =>
                val apexClass = membership.getApexClass
                val apexTestSuite = membership.getApexTestSuite

                val testSuite = TestSuite(name = apexTestSuite.getTestSuiteName, id = apexTestSuite.getId, List(apexClass))
                testSuite
            }.toList

        val testSuitesByName = testSuitePerClass.groupBy(_.name)
        // convert:  Map[TestSuite.name, List[TestSuite]]
        // to Map[String, TestSuite] by accumulating all TestSuite.classes by TestSuite.Name
        testSuitesByName.values
            .filter(_.nonEmpty)
            .map{suits =>
                val suite = suits.head
                // now put classes from current suits into suite
                val classes = suits.flatMap(_.classes)
                suite.copy(classes = classes)
            }.map(ts => ts.name -> ts).toMap
    }

//    private def toJson(classesBySuite: Map[ApexTestSuite, List[ApexClass]]): JsValue = {
//        classesBySuite.toJson
//    }
    private def create(testSuiteName: String, session: Session): Try[String] = {
        session.config.getProperty("testSuiteClassNames") match {
            case Some(classNamesStr) =>
                // make sure this Test Suet does not exist
                delete(List(testSuiteName), session)

                val apexTestSuite = new ApexTestSuite()
                apexTestSuite.setTestSuiteName(testSuiteName)

                val saveResults = session.createTooling(Array(apexTestSuite))

                saveResults.filterNot(_.isSuccess).toList match {
                    case errorResults @ head :: tail =>
                        Failure(new Throwable(head.getErrors.mkString("\n")))
                    case Nil =>
                        val testSuiteId = saveResults.head.getId
                        addClasses(testSuiteId, classNamesStr.split(",").toList, session).map(_ => testSuiteId)
                }

            case _ => Failure(new IllegalArgumentException("Missing or invalid 'testSuiteClassNames' parameter") )
        }
    }

    private def addClasses(testSuiteId: String, classNames: List[String], session: Session): Try[Int] = {
        val classIds = getClassIdByName(classNames, session).values
        val testSuiteMembers =
            classIds.map{classId =>
                val testSuiteMember = new TestSuiteMembership()
                testSuiteMember.setApexTestSuiteId(testSuiteId)
                testSuiteMember.setApexClassId(classId)
                testSuiteMember
            }
//        val saveResults = session.upsertTooling("ApexClassId", testSuiteMembers.toArray)
        val saveResults = session.createTooling(testSuiteMembers.toArray)
        saveResults.filterNot(_.isSuccess).toList match {
            case errorResults @ head :: tail =>
                Failure(new Throwable(head.getErrors.mkString("\n")))
            case Nil =>
                Success(saveResults.length)
        }
    }

    private def removeClasses(testSuiteId: String, classNames: List[String], session: Session): Try[Int] = {
        val classIds = getClassIdByName(classNames, session).values.mkString("','")

        if (classIds.nonEmpty) {
            val testSuiteMemberIds =
                SoqlQuery.getQueryIteratorTooling(session, s"select Id, ApexClassId, ApexTestSuiteId from TestSuiteMembership where ApexClassId in ('$classIds') and ApexTestSuiteId = '$testSuiteId'")
                    .map(new ResultRecord(_))
                    .map(r => r.getFieldAsString("Id").get )
            val deleteResult = session.deleteTooling(testSuiteMemberIds.toArray)
            deleteResult.filterNot(_.isSuccess).toList match {
                case errorResults @ head :: tail =>
                    Failure(new Throwable(head.getErrors.mkString("\n")))
                case Nil =>
                    Success(deleteResult.length)
            }
        } else {
            Success(0)
        }

    }
    private def getClassIdByName(classNames: List[String], session: Session): Map[String, String] = {
        val containsStr = classNames.mkString("','")
        SoqlQuery.getQueryIteratorTooling(session, s"select Id, Name from ApexClass where Name in ('$containsStr')")
            .map(new ResultRecord(_))
            .map(r => ( r.getFieldAsString("Name").get, r.getFieldAsString("Id").get) )
            .toMap
    }

    private def delete(testSuiteNames: List[String], session: Session): Try[String] = {
        val idsToDelete = TestSuiteActions.getTestSuiteIdByName(testSuiteNames, session).values.toArray
        session.deleteTooling(idsToDelete).find(!_.getSuccess) match {
            case Some(errorResult) => Failure(new RuntimeException(errorResult.getErrors.map(_.getMessage).mkString("; ")))
            case None => Success(idsToDelete.length.toString)
        }
    }

    private def getTestSuiteIdByName(testSuiteName: String, session: Session): Option[String] = {
        TestSuiteActions.getTestSuiteIdByName(List(testSuiteName), session).get(testSuiteName.toLowerCase)
    }
}

object TestSuiteActions {
    /**
      * @param testSuiteNames e.g. "One,Two,Three"
      * @param session - session to use
      * @return test suite Id by Name
      */
    def getTestSuiteIdByName(testSuiteNames: List[String], session: Session): Map[String, String] = {
        val containsStr = testSuiteNames.mkString("','")
        val queryResult = session.query(s"select Id, TestSuiteName from ApexTestSuite where TestSuiteName in ('$containsStr')")
        val records = queryResult.getRecords
        if (records.nonEmpty) {
            records.map(sobject => (sobject.getField("TestSuiteName").toString.toLowerCase, sobject.getId)).toMap
        } else {
            Map.empty
        }
    }
    def getTestSuiteList(session: Session): List[ApexTestSuite] = {
        val soql = s"select Id, TestSuiteName from ApexTestSuite"
        val iterator = SoqlQuery.getQueryIteratorTyped[com.sforce.soap.tooling.sobject.ApexTestSuite](session, session.queryTooling(soql))
        if (iterator.nonEmpty) {
            //records.map(sobject => sobject.asInstanceOf[ApexTestSuite]).toList
            iterator.toList
        } else {
            List.empty[ApexTestSuite]
        }

    }

}
