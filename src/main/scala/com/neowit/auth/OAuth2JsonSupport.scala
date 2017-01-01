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

package com.neowit.auth

import java.net.URL

import com.neowit.utils.JsonSupport

/**
  * Author: Andrey Gavrikov
  */
trait OAuth2JsonSupport extends JsonSupport {
    implicit val Oauth2TokensFormat = jsonFormat9(Oauth2Tokens)
}

case class Oauth2Tokens(id: Option[String],
                        issued_at: Option[String],
                        scope: Option[String],
                        instance_url: Option[String],
                        token_type: Option[String],
                        refresh_token: Option[String],
                        id_token: Option[String],
                        signature: Option[String],
                        access_token: Option[String]
                       ) {
    /**
      * extract User Id from
      * "id":"https://login.salesforce.com/id/00D50000000IZ3ZEAW/00550000001fg5OAAQ"
      * @return
      */
    def getUserId: Option[String] = {
        id match {
            case Some(url) => Option(url.split("/").takeRight(1).head)
            case None => None
        }
    }
    /**
      * extract Org Id from
      * "id":"https://login.salesforce.com/id/00D50000000IZ3ZEAW/00550000001fg5OAAQ"
      * @return
      */
    def getOrgId: Option[String] = {
        id match {
            case Some(url) => Option(url.split("/").dropRight(1).takeRight(1).head)
            case None => None
        }
    }

    /**
      * extract Login URL from
      * "id":"https://login.salesforce.com/id/00D50000000IZ3ZEAW/00550000001fg5OAAQ"
      * @return
      */
    def getLoginUrl: Option[String] = {
        id match {
            case Some(urlStr) =>
                val url = new URL(urlStr)
                val urlString = url.toExternalForm
                Option(urlString.substring(0, urlString.length - url.getPath.length))
            case None => None
        }

    }
}

