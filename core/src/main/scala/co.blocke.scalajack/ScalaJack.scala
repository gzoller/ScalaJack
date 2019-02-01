package co.blocke.scalajack

import model._
import json.JsonFlavor

object ScalaJack {
  // TODO:  This may not be thread-safe!  Need to create a *NEW* JsonFlavor on each call to apply()
  def apply[N, WIRE](kind: JackFlavor[N, WIRE] = JsonFlavor()): JackFlavor[N, WIRE] = kind
}

case class ViewException(msg: String) extends Exception(msg)
