package co.blocke.scalajack

import model._
import json._

object ScalaJack {
  def apply() = JsonFlavor.make()
  def apply(maker: FlavorMaker) = maker.make()
}

case class ViewException(msg: String) extends Exception(msg)
