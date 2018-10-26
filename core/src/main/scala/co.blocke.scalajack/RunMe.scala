package co.blocke.scalajack

import csv._

case class Maybe(s: String, dunno: Option[String], more: Boolean)

object RunMe extends App {

  val sj = ScalaJack(CSVFlavor())
  val inst = Maybe("no", None, false)
  val csv = sj.render(inst)
  println(csv)
  //  val i = sj.read[Maybe](csv)
  //  println("i = "+i)

}

