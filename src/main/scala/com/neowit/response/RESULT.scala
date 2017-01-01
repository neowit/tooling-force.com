package com.neowit.response

/**
  * Author: Andrey Gavrikov
  */
sealed trait RESULT

case object FAILURE extends RESULT
case object SUCCESS extends RESULT
