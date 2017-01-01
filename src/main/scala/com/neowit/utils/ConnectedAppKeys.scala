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

