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

import com.neowit.apex.completion.{DatabaseModel, SObjectLibrary}
import com.neowit.apexscanner.Project


/**
  * Created by Andrey Gavrikov 
  */
object ProjectsCache {
    private val _projectByPath = new collection.mutable.HashMap[File, Project]()

    /**
      * return previously initialised project or new one if no loaded project for given path exists
      * @param projectDir @see ConfigWithSfdcProject.projectDirOpt
      * @param session session related to current project/action
      * @return
      */
    def getProject(projectDir: File, session:Session, loadStdLib: Boolean = true, loadSobjectLib: Boolean = true): Project = {
        _projectByPath.get(projectDir) match {
            case Some(_project) => _project
            case None =>
                Project.findApexProjectRoot(projectDir.toPath) match {
                    case Some(projectRoot) =>
                        val project = new Project(projectRoot)
                        // add SObjectLibrary
                        val sobjectLib = new SObjectLibrary(session)
                        // make sure model is not loaded for this session (e.g. when running tests in parallel)
                        // different test classes can not re-use same DatabaseModel because their projects have different Ast Trees
                        DatabaseModel.removeModelBySession(session)
                        // load SObject library
                        project.addExternalLibrary(sobjectLib)
                        // add StdLib (must go after SObject library because some Objects in DB may conflict with names in StdLib)
                        project.loadStdLib()

                        _projectByPath += projectDir -> project
                        project
                    case None =>
                        throw new IllegalArgumentException("Failed ot detect project root by path: " + projectDir)
                }

        }
    }
}
