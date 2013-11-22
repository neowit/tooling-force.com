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
import com.neowit.utils.{FileUtils, Logging}
import com.neowit.apex.session.SfdcSession
import com.sforce.soap.metadata.{DeployOptions, AsyncResult, AsyncRequestState, MetadataConnection}
import com.neowit.apex.tooling.Response

abstract class AsyncTask(session: SfdcSession) extends Logging with Response{

    def run[A](callback: (A) => Unit)
    /**
     * using stored session data figure out what files have changed
     * @return
     */
    type ChangedFiles = Map[MetadataType, List[File]]

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

    private val ONE_SECOND = 3600
    private val MAX_NUM_POLL_REQUESTS = 50
    def wait(connection: MetadataConnection, asyncResult: AsyncResult): AsyncResult = {
        val waitTimeMilliSecs = ONE_SECOND
        var attempts = 0
        var _asyncResult = asyncResult
        while (!_asyncResult.isDone) {
            Thread.sleep(waitTimeMilliSecs)
            attempts += 1
            if (!asyncResult.isDone && ((attempts +1) > MAX_NUM_POLL_REQUESTS)) {
                throw new Exception("Request timed out.  If this is a large set " +
                    "of metadata components, check that the time allowed " +
                    "by MAX_NUM_POLL_REQUESTS is sufficient.")
            }
            _asyncResult = connection.checkStatus(Array(_asyncResult.getId))(0)
            logger.info("Status is: " + _asyncResult.getState)
        }
        if (AsyncRequestState.Completed != _asyncResult.getState) {
            throw new Exception(_asyncResult.getStatusCode + " msg:" + _asyncResult.getMessage)
        }
        _asyncResult
    }

}

class DeployTask(session: SfdcSession) extends AsyncTask(session) {
    import com.neowit.utils.ZipUtils
    /**
     * deploy zip archive
     */
    def run[A](callback: (A) => Unit){
        //zip provided folder
        //get temp file name
        val destZip = FileUtils.createTempFile("deploy", ".zip")
        try {
            destZip.delete()
            ZipUtils.zipDir(session.getConfig.srcPath, destZip.getAbsolutePath)
            val deployOptions = new DeployOptions()
            deployOptions.setPerformRetrieve(false)
            deployOptions.setAllowMissingFiles(true)
            deployOptions.setRollbackOnError(true)

            val metadataBinding = session.getMetadataConnection.connection


            //val asyncResult = wait(metadataBinding, session.getMetadataConnection.deployZip(destZip, deployOptions))
            val asyncResult = wait(metadataBinding, session.getMetadataConnection.deployZip(ZipUtils.zipDirToBytes(session.getConfig.srcDir), deployOptions))

            val includeDetails = true //TODO make false
            /*
            val deployResult = metadataBinding.checkDeployStatus(asyncResult.getId, includeDetails)
            if (!deployResult.isSuccess) {
                logger.error("Deploy failed. " + deployResult)
                throw new Exception(deployResult.getErrorStatusCode + " msg: " + deployResult.getErrorMessage)
            }
            */

            val deployResultWithDetails = metadataBinding.checkDeployStatus(asyncResult.getId, true)
            callback(deployResultWithDetails.asInstanceOf[A])
        } finally {
            //delete temp zip file
            try {
                new File(destZip.getAbsolutePath).delete()
            } catch {
                case _:Throwable => //ignore
            }
        }
    }
}

class DescribeTask(session: SfdcSession) extends AsyncTask(session) {
    import com.sforce.soap.metadata.DescribeMetadataResult
    /**
     * describe metadata
     */
    def run[A](callback: (A) => Unit){
        val describeResult: DescribeMetadataResult = session.getMetadataConnection.describeMetadata
        callback(describeResult.asInstanceOf[A])
    }

}

class RetrieveTask(session: SfdcSession) extends AsyncTask(session) {
    import com.sforce.soap.metadata.RetrieveRequest
    /**
     * refresh project
     */
    def run[A](callback: (A) => Unit){

        val retrieveRequest = new RetrieveRequest()
        retrieveRequest.setApiVersion(session.apiVersion)
        setUpackaged(retrieveRequest)
        val metadataBinding = session.getMetadataConnection.connection
        val asyncResult = wait(metadataBinding,
                                session.getMetadataConnection.retrieve(retrieveRequest))

        val retrieveResult = metadataBinding.checkRetrieveStatus(asyncResult.getId)
        retrieveResult.getMessages match {
          case messages if null != messages && !messages.isEmpty=>
              for(msg <- messages) {
                  response.warning("Retrieve", "", msg.getFileName, msg.getProblem )
              }
          case _ =>
        }
        //write results to ZIP file
        logger.debug("retrieveResult.getFileProperties=" + retrieveResult.getFileProperties)
        callback(retrieveResult.asInstanceOf[A]) //return resulting zip

    }
    def setUpackaged(retrieveRequest: RetrieveRequest) {
        val metaXml = new MetaXml(session.getConfig)
        val unpackagedManifest = metaXml.getPackageXml
        logger.debug("Manifest file: " + unpackagedManifest.getAbsolutePath)

        retrieveRequest.setUnpackaged(metaXml.getPackage)
    }

}

