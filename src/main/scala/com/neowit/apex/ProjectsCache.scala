/*
 *
 *  * Copyright (c) 2017 Andrey Gavrikov.
 *  * this file is part of tooling-force.com application
 *  * https://github.com/neowit/tooling-force.com
 *  *
 *  * This program is free software: you can redistribute it and/or modify
 *  * it under the terms of the GNU Lesser General Public License as published by
 *  * the Free Software Foundation, either version 3 of the License, or
 *  * (at your option) any later version.
 *  *
 *  * This program is distributed in the hope that it will be useful,
 *  * but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  * GNU Lesser General Public License for more details.
 *  *
 *  * You should have received a copy of the GNU Lesser General Public License
 *  * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 */

package com.neowit.apex

import java.io.File

import com.neowit.apex.completion.SObjectLibrary
import com.neowit.apexscanner.Project

import scala.concurrent.ExecutionContext

/**
  * Created by Andrey Gavrikov 
  */
object ProjectsCache {
    private val _projectByPath = new collection.mutable.HashMap[File, Project]()

    /**
      * return previously initialised project or new one if no loaded project for given path exists
      * @param projectDir @see ConfigWithSfdcProject.projectDirOpt
      * @param session session related to current project/action
      * @param ec execution context used to initialise project
      * @return
      */
    def getProject(projectDir: File, session:Session, loadStdLib: Boolean = true, loadSobjectLib: Boolean = true)(implicit ec: ExecutionContext): Option[Project] = {
        _projectByPath.get(projectDir) match {
            case projectOpt @ Some(_) => projectOpt
            case None =>
                val project = new Project(projectDir.toPath)
                // add SObjectLibrary
                val sobjectLib = new SObjectLibrary(session)
                project.addExternalLibrary(sobjectLib)
                // add StdLib (must go after SObject library because some Objects in DB may confict with names in StdLib)
                project.loadStdLib()

                _projectByPath += projectDir -> project
                Option(project)
        }
    }
}
