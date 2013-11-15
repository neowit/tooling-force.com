/*
 * Copyright (c) 2013 Andrey Gavrikov.
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

package com.neowit.apex.metadata

import java.io.File
import com.neowit.utils.Logging
import com.neowit.apex.session.SfdcSession
import com.sforce.soap.metadata.DescribeMetadataResult

abstract class AsyncTask(session: SfdcSession) extends Logging {

    def run[A](callback: (A) => Unit)
    /**
     * using stored session data figure out what files have changed
     * @return
     */
    type ChangedFiles = Map[SObjectType, List[File]]

    /*
    def isModified(sessionData: SessionData, helper: SObjectType, f: File): Boolean = {
        val key = helper.getKey(f)
        sessionData.getField(key, "LastSyncDateLocal") match {
            case Some(x) =>
                f.lastModified > x.toLong && None != sessionData.getField(key, "Id")
            //case None => throw new IllegalStateException("Workspace is out of date. Please refresh before continuing")
            case _ =>
                //if there is no session data for existing file then this file must be Created
                logger.debug(key + ": missing LastSyncDateLocal. Looks like the project is out of sync")
                throw new IllegalStateException("New files must be saved before Modified ones. Perhaps your project needs 'refresh'")
        }
    }
    def getModifiedFiles(sessionData: SessionData): ChangedFiles = {
        def iter (helpers: List[SObjectType], res: ChangedFiles): ChangedFiles = {
            helpers match {
                case Nil => res
                case helper :: xs =>
                    val files = helper.listFiles(srcDir)
                    iter(xs, res ++ Map(helper -> files.filter(isModified(sessionData, helper, _)).toList))

            }
        }
        iter(SObjectTypes.list, Map())
    }
    */


}

class DeployTask(session: SfdcSession) extends AsyncTask(session) {
    /**
     * deploy zip archive
     */
    def run[A](callback: (A) => Unit){

    }

}
class DescribeTask(session: SfdcSession) extends AsyncTask(session) {
    /**
     * describe metadata
     */
    def run[A](callback: (A) => Unit){
        val describeResult: DescribeMetadataResult = session.getMetadataConnection.describeMetadata
        callback(describeResult.asInstanceOf[A])
    }

}


