package com.neowit.response

/**
  * Author: Andrey Gavrikov
  */
sealed trait MessageType {
    def getTypeString: String
    override def toString: String = getTypeString
}
case object INFO extends MessageType {
    def getTypeString: String = "INFO"
}
case object WARN extends MessageType {
    def getTypeString: String = "WARN"
}
case object ERROR extends MessageType {
    def getTypeString: String = "ERROR"
}
case object DEBUG extends MessageType {
    def getTypeString: String = "DEBUG"
}
case object SECTION extends MessageType {
    def getTypeString: String = "SECTION"
}
case object KEY_VALUE extends MessageType {
    def getTypeString: String = "KEY_VALUE"
}
case class CustomMessageType(msgType: String) extends MessageType {
    def getTypeString: String = msgType
}
