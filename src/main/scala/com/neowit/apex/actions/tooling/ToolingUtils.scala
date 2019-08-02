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

    //we can use ToolingApi in following cases
    //1. there are no -meta.xml files
    //2. there are no new files
    //3. all files are supported by Tooling API
    //4. there is NO mix of aura and non aura files in the list of files to deploy
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
        val hasAuraFiles = files.exists(AuraMember.isSupportedType(_))
        val hasLwcFiles = files.exists(LwcMember.isSupportedType(_))
        val hasApexFiles = files.exists(ApexMember.isSupportedType(_, session))
        val hasMixOfApexAndAura = (hasAuraFiles || hasLwcFiles) && hasApexFiles
        //check if all files supported by tooling api
        val hasUnsupportedType = files.exists(f =>
            !ApexMember.isSupportedType(f, session)
              && !AuraMember.isSupportedType(f)
              && !LwcMember.isSupportedType(f)
        )
        val canNotUseTooling = hasUnsupportedType || hasMixOfApexAndAura

        !canNotUseTooling
    }


}
