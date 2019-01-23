package co.blocke.scalajack

import model._
import json.JsonFlavor

object ScalaJack {
  def apply[N, WIRE](kind: JackFlavor[N, WIRE] = JsonFlavor()): JackFlavor[N, WIRE] = kind
}

case class ViewException(msg: String) extends Exception(msg)
