package co.blocke.scalajack

import model._
import json.JsonFlavor

//import co.blocke.scalajack.BijectiveFunctions._
//import co.blocke.scalajack.json.JsonFlavor
//import co.blocke.scalajack.typeadapter._

object ScalaJack {
  def apply[N, WIRE](kind: JackFlavor[N, WIRE] = JsonFlavor()): JackFlavor[N, WIRE] = kind
}

case class ViewException(msg: String) extends Exception(msg)
