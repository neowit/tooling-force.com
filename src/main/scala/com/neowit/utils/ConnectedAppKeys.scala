package com.neowit.utils

import java.net.URL
import java.util.Properties

/**
  * Author: Andrey Gavrikov
  */

object ConnectedAppKeys {
    def load(): Option[ConnectedAppKeys] = {
        val is = getClass.getClassLoader.getResource("connected-app-keys.properties")
        if (null == is) {
            return None
        }
        val props = new Properties()
        props.load(is.openStream())
        val consumerKey = props.getProperty("CONSUMER_KEY")
        val consumerSecret = props.getProperty("CONSUMER_SECRET")
        val callbackUrl = props.getProperty("CALLBACK_URL")
        if (null == consumerSecret || null == consumerKey || null == callbackUrl) {
            None
        } else {
            Option(ConnectedAppKeys(consumerKey, consumerSecret, callbackUrl))
        }
    }
}
case class ConnectedAppKeys(consumerKey: String, consumerSecret: String, callbackUrl: String) {
    def getPort: Int = {
        new URL(callbackUrl).getPort
    }
}

