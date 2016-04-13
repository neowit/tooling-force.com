package com.neowit.apex

import java.io.File

import com.neowit.utils.{ConfigValueException, FileUtils}
import spray.json.{JsString, JsonParser}

import scala.reflect.ClassTag

/**
  * Author: Andrey Gavrikov (westbrook)
  * Date: 13/04/2016
  */
object LogUtils {
    /**
      *
      * @param levelByCategory Map("category" -> "level", ...)
      * @see loadDebugHeaderConfig
      * @param logInfoCreatorProvider takes care of instantiating appropriate classes
      * @tparam A e.g. com.sforce.soap.metadata.LogInfo
      * @return
      */
    private def getDebugHeaderLogInfos[A](levelByCategory: Map[String, String],
                                          logInfoCreatorProvider: LogInfoCreatorProvider[A]) = {

        val logInfoOpts =
            levelByCategory.keys.map{ category =>
                levelByCategory.get(category) match {
                    case Some(level) =>
                        val logInfo = logInfoCreatorProvider.logInfoCreator(category, level)
                        logInfo

                    case None => None
                }
            }
        logInfoOpts.filter(_.nonEmpty).map(_.get)
    }

    trait LogInfoCreatorProvider[A] {
        def logInfoCreator (category: String, level: String): Option[A]
    }

    class MetadataLogInfoCreatorProvider extends LogInfoCreatorProvider[com.sforce.soap.metadata.LogInfo] {
        import com.sforce.soap.metadata._

        def logInfoCreator(category: String, level: String): Option[com.sforce.soap.metadata.LogInfo] = {

            val logInfo = new LogInfo
            val logCategory = LogCategory.valueOf(category)
            val logLevel = LogCategoryLevel.valueOf(level)
            if (null!= logCategory && null != logLevel) {
                logInfo.setCategory(logCategory)
                logInfo.setLevel(logLevel)
                Option(logInfo)
            } else {
                None
            }
        }
    }

    class ToolingLogInfoCreatorProvider extends LogInfoCreatorProvider[com.sforce.soap.tooling.LogInfo] {
        import com.sforce.soap.tooling._

        def logInfoCreator(category: String, level: String): Option[com.sforce.soap.tooling.LogInfo] = {

            val logInfo = new LogInfo
            val logCategory = LogCategory.valueOf(category)
            val logLevel = LogCategoryLevel.valueOf(level)
            if (null!= logCategory && null != logLevel) {
                logInfo.setCategory(logCategory)
                logInfo.setLevel(logLevel)
                Option(logInfo)
            } else {
                None
            }
        }
    }
    def getDebugHeaderLogInfos[A](configPath: Option[String],

                                  session: Session,
                                  logInfoCreatorProvider: LogInfoCreatorProvider[A]
                                 )(implicit classTag: ClassTag[A]): Array[A] = {
        val levelByCategoryMap = loadDebugHeaderConfig(configPath, session)
        getDebugHeaderLogInfos[A](levelByCategoryMap, logInfoCreatorProvider).toArray[A]
    }

    private def loadDebugHeaderConfig(configPath: Option[String], session: Session): Map[String, String] = {
        val defaultMap = Map("Apex_code" -> "Debug", "Apex_profiling"-> "Error", "Callout"-> "Error", "Db"-> "Error", "System"-> "Error", "Validation"-> "Error", "Visualforce"-> "Error", "Workflow"-> "Error")
        loadDebugConfig(configPath, session, "debuggingHeaderConfig", defaultMap)
    }

    def loadDebugConfig(jsonConfigPath: Option[String], session: Session, sessionKey: String, default: Map[String, String]): Map[String, String] = {
        val traceFlagMap: Map[String, String] = jsonConfigPath match {
            case Some(logConfigPath) =>
                val f = new File(logConfigPath)
                if (f.canRead) {
                    val jsonStr = FileUtils.readFile(f).getLines().mkString("")
                    val jsonAst = JsonParser(jsonStr)
                    val pairs = jsonAst.asJsObject.fields.map {
                        case (key, jsVal:JsString) => key -> jsVal.value
                        case (key, jsVal) =>
                            //this case should never be used, but have it here to make compiler happy
                            key -> jsVal.toString()
                    }
                    pairs
                } else {
                    throw new ConfigValueException(s"$sessionKey file is NOT readable. Path: " + logConfigPath)
                }
            case None =>
                val data = session.getData(sessionKey)
                if (data.nonEmpty) {
                    data.map{case (key, str) => key -> str.toString}
                } else {
                    default
                }
        }
        traceFlagMap
    }

}
