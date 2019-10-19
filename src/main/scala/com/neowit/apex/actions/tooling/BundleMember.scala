/*
 * Copyright (c) 2019 Andrey Gavrikov.
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

package com.neowit.apex.actions.tooling

import java.io.File

import com.neowit.apex.Session
import com.neowit.utils.FileUtils
import com.sforce.soap.metadata.FileProperties
import com.sforce.soap.tooling.sobject.SObject

trait BundleMember {
    def getBundleMemberHelper: BundleMemberHelper
}

trait BundleMemberHelper extends BundleMember {
    def DIR_NAME: String
    def BUNDLE_XML_TYPE: String
    def XML_TYPE: String

    def isSupportedType(file: File): Boolean
    def getBundleDir(file: File): Option[File]
    def getInstanceUpdate(file: File, session: Session): SObject
    def updateDefinitionData(session: Session, bundledFiles: List[File], idsOnly: Boolean): Map[File, FileProperties]

    /**
     * extract value used in <member>FileName</member> in package.xml
     * @param resourcePath - e.g. lwc/helloWorld/helloWorld.html
     * @return "helloWorld" - i.e. bundle name
     */
    def getPackageXmlMemberName(resourcePath: String): Option[String] = {
        val normalisedPath = FileUtils.normalizePath(resourcePath)
        val pathComponents = normalisedPath.split("/")
        val indexOfBundleDir = pathComponents.indexOf(DIR_NAME)
        if (indexOfBundleDir >= 0 && pathComponents.length> 1) {
            Option(pathComponents(indexOfBundleDir+1))
        } else {
            None
        }
    }

    /**
     * current version (v32.0) of metadata API fails to deploy packages if they contain incomplete Aura Bundle
     * i.e. if any single file from aura bundle is included (e.g. <app-name>.css ) then *all* files in this bundle must be
     * part of deployment package, otherwise SFDC returns: UNKNOWN_EXCEPTION: An unexpected error occurred.
     * @param fileInBundle - file which belongs to Aura bundle
     * @return
     */
    def getAllFilesInBundle(fileInBundle: File): Set[File] = {
        getBundleDir(fileInBundle) match {
            case Some(bundleDir) => FileUtils.listFiles(bundleDir, descentIntoFolders = true, includeFolders = false).toSet
            case None => Set()
        }
    }
}

object BundleMember {
    val HELPERS:Seq[BundleMemberHelper] = Seq(AuraMemberHelper.helper, LwcMemberHelper.helper)

    /**
     * try to detect which bundle member helper may cover type of the file with provided path
     * e.g. if file path is lwc/some/some.html
     * then return LwcMemberHelper
     * @param file - e.g. lwc/some/some.html
     * @return if file path is lwc/some/some.html then return LwcMemberHelper
     */
    def getBundleMemberHelper(file: File): Option[BundleMemberHelper] = {
        HELPERS.find(_.isSupportedType(file))
    }
    def getBundleMemberHelper(filePath: String): Option[BundleMemberHelper] = {
        val filePathNormalised = FileUtils.normalizePath(filePath)
        HELPERS.find(h => filePathNormalised.contains(h.DIR_NAME + FileUtils.NORMAL_SLASH))
    }

    def isSupportedType(file: File): Boolean = HELPERS.exists(_.isSupportedType(file))

    def splitFilesByHelper(files: List[File]): Map[BundleMemberHelper, List[File]] = {
        val filesByHelper = HELPERS.map(helper => (helper, files.filter(helper.isSupportedType))).toMap
        // remove all keys where file collection is empty
        filesByHelper.filter(p => p._2.nonEmpty)
    }
}
