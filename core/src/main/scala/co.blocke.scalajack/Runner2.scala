package co.blocke.scalajack

import json._
import model._
import util._

object Runner2 extends App {

  val sj = ScalaJack()

  val js = """[{"foo":5},{"bar":6}]"""
  println(js)

  println(sj.read[List[Map[String, Int]]](js))

  //  val raceString = """[12345,54321,-4768,672,-983456,2547]"""
  //  val inst = sj.read[List[Int]](raceString)
  //  println(inst)
  //  println(sj.render(inst))
}
