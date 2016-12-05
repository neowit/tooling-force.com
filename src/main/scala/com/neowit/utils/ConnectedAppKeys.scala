package com.neowit.utils

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
        if (null == consumerSecret || null == consumerKey) {
            None
        } else {
            Option(ConnectedAppKeys(consumerKey, consumerSecret))
        }
    }
}
case class ConnectedAppKeys(consumerKey: String, consumerSecret: String)

