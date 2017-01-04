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

package com.neowit.response.protocols.vim

import com.neowit.apex.actions.DeploymentReport
import com.neowit.response.DeployModifiedResult

object DeployModified {
    def sendDeploymentReport(writer: ResponseWriterVim, report: DeploymentReport): Unit = {
        if (report.conflictsReportOpt.exists(_.hasConflicts)) {
            // report conflicts
            ListConflicting.sendConflictDetails(writer, report.conflictsReportOpt.get)
        } else {
            report.failureReportOpt.foreach(DeploymentReportUtils.sendDeploymentFailureReport(writer, _))
            if (report.otherErrors.nonEmpty) {
                DeploymentReportUtils.sendOtherErrors(writer, report.otherErrors)
            }
            DeploymentReportUtils.sendSuccessReport(writer, report)
        }
        DeploymentReportUtils.sendLogFile(writer, report.logFileOpt)
    }

}
class DeployModified(writer: ResponseWriterVim) extends VimProtocol[DeployModifiedResult] {
    def send(result: DeployModifiedResult): Unit = {
        val report = result.deploymentReport
        DeployModified.sendDeploymentReport(writer, report)
    }
}
