package com.neowit.apex.actions

import com.neowit.apex.Session
import com.neowit.apex.actions.SoqlQuery.ResultRecord
import com.neowit.utils.ResponseWriter
import com.sforce.soap.tooling.{ApexTestSuite, TestSuiteMembership}

import scala.util.{Failure, Success, Try}

/**
  * Author: Andrey Gavrikov (westbrook)
  * Date: 14/04/2016
  */
class TestSuiteActions extends ApexAction{

    override def getHelp: ActionHelp = new ActionHelp {
        override def getExample: String = ""
        override def getParamDescription(paramName: String): String = paramName match {
            case "testSuiteManage" =>
                """--testSuiteManage=action-name
                  |  Manage test suite using suite name provided by 'testSuiteName' parameter
                  |  Supported Actions:
                  |  - create, delete, update
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
        }

        override def getParamNames: List[String] = List("testSuiteAction", "testSuiteName", "testSuiteClassNames")

        override def getSummary: String = "Create|Update|Delete test suite by name"

        override def getName: String = "testSuite"
    }

    //this method should implement main logic of the action
    override protected def act(): Unit = {
        val actionResult =
        config.getProperty("testSuiteAction") match {
            case Some(action) if Set("create", "update", "delete").contains(action.toLowerCase) =>
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
                                                Try(config.getRequiredProperty("testSuiteClassNames")
                                                    .map(classNames => addClasses(testSuiteId, classNames.split(",").toList , session)))

                                            case "removeClasses" =>
                                                Try(config.getRequiredProperty("testSuiteClassNames")
                                                    .map(classNames => removeClasses(testSuiteId, classNames.split(",").toList , session)))
                                        }
                                    case None => Failure(new IllegalArgumentException(s"Invalid config for 'testSuiteAction', Test Suite with name '$testSuiteName' not found"))
                                }
                        }
                }
            case _ =>
                Failure(new IllegalArgumentException("Invalid config for 'testSuiteAction'"))

        }

        actionResult match {
            case Success(_res) =>
            case Failure(ex) =>
                responseWriter.println("RESULT=FAILURE")
                responseWriter.println(ResponseWriter.Message(ResponseWriter.ERROR, ex.getMessage))
        }
    }

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

            case _ => Failure(new IllegalArgumentException("Missing or invalid 'testSuiteClassName' parameter") )
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
        val saveResults = session.upsertTooling("ApexClassId", testSuiteMembers.toArray)
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

    private def delete(testSuiteNames: List[String], session: Session): Array[com.sforce.soap.tooling.DeleteResult] = {
        val idsToDelete = TestSuiteActions.getTestSuiteIdByName(testSuiteNames, session).values.toArray
        session.deleteTooling(idsToDelete)
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

}
