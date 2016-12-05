package com.neowit.oauth

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
}

