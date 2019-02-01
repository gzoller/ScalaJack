package co.blocke.scalajack

import model._
import json.JsonFlavor

object ScalaJack {

  type MAKER = () => JackFlavor[_, _]

  // TODO:  This may not be thread-safe!  Need to create a *NEW* JsonFlavor on each call to apply()
  //  def apply[N, WIRE](kind: JackFlavor[N, WIRE] = JsonFlavor()): JackFlavor[N, WIRE] = new JsonFlavor().asInstanceOf[JackFlavor[N, WIRE]] //kind

  //  def apply[N, WIRE](maker: () => JackFlavor[N, WIRE] = json.JsonFlavorMaker()): JackFlavor[N, WIRE] = {
  //    println(maker)
  //    (new JsonFlavor()).asInstanceOf[JackFlavor[N, WIRE]]
  //  }

  //  def apply[N, WIRE](maker: () => JackFlavor[N, WIRE] = () => JsonFlavor()): JackFlavor[N, WIRE] = {
  //    println("STARTED")
  //    maker()
  //  }

  def apply[N, WIRE](maker: MAKER = () => JsonFlavor()): JackFlavor[N, WIRE] = {
    println("STARTED")
    maker()
  }

}

case class ViewException(msg: String) extends Exception(msg)
