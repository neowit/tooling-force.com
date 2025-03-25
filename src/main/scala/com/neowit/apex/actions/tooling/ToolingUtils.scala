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

package com.neowit.apex.actions.tooling

import java.io.File

import com.neowit.apex.Session

object ToolingUtils {

    /**
    * we can use ToolingApi in following cases
    * 1. there are no -meta.xml files
    * 2. there are no new files
    * 3. all files are supported by Tooling API
    * 4. there is NO mix of aura and non aura files in the list of files to deploy
    * 5. changed in 2024 - multiple bundles (e.g. multiple LWC components) used to work before API v61 but no longer
     *      work and attempts to use tooling API to deploy more than 1 LWC bundle throws " An unexpected error occurred. Please include this ErrorId if you contact support"
    *       still works with API v63: saving multiple files if all bundle files (e.g. LWC components) are from the same bundle/component
    */
    def canUseTooling(session: Session, files: List[File]): Boolean = {
        val hasMeta = files.exists(_.getName.endsWith("-meta.xml"))
        if (hasMeta) {
            return false
        }
        //check if there are new files
        val hasNewFile = files.exists(f => {
            val key = session.getKeyByFile(f)
            "" == session.getData(key).getOrElse("Id", "")
        })

        if (hasNewFile) {
            return false
        }
        val hasBundleFiles = files.exists(BundleMember.isSupportedType)
        val hasMultipleBundles = if (hasBundleFiles) {
            // count number of involved bundles
            files.map(f =>
                BundleMember.getBundleMemberHelper(f).
                    map(_.getBundleDir(f).
                        map(_.getName))).
                toSet.size > 1
        } else {
            false
        }

        val hasApexFiles = files.exists(ApexMember.isSupportedType(_, session))
        val hasMixOfApexAndAura = hasBundleFiles && hasApexFiles
        //check if all files supported by tooling api
        val hasUnsupportedType = files.exists(f =>
            !ApexMember.isSupportedType(f, session)
              && !BundleMember.isSupportedType(f)
        )
        val canNotUseTooling = hasUnsupportedType || hasMixOfApexAndAura || hasMultipleBundles

        !canNotUseTooling
    }


}
