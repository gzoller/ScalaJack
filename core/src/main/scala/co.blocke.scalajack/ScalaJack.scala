package co.blocke.scalajack

import model._
import json._

object ScalaJack {
  def apply() = JsonFlavor()
  def apply[S](kind: JackFlavor[S]): JackFlavor[S] = kind
}

case class ViewException(msg: String) extends Exception(msg)
