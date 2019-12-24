package co.blocke.scalajack

import model._
import json._

object ScalaJack {
  def apply()                                      = JsonFlavor()
  def apply[S](kind: JackFlavor[S]): JackFlavor[S] = kind
}

class ScalaJackError(msg: String)                           extends Exception(msg)
class ScalaJackValueError(val value: Any, cause: Throwable) extends Exception(cause.getMessage)
