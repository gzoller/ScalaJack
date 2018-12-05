package co.blocke.scalajack
package parser

import json._
import co.blocke.scalajack.ScalaJack

object Runner extends App {

  implicit val p = new json.JsonIntParser

  val i = IntPath()
  val ps = JsonParserState("12345 67890")

  val bs = JsonParserState("true false")
  val jbp = new json.JsonBooleanParser
  println(BooleanPath().parse(bs)(jbp))
  println(BooleanPath().parse(bs)(jbp))

  val sj = ScalaJack()

  val one = timer(() => {
    for (x <- 1 to 1000000) {
      val ps = JsonParserState("12345")
      i.parse(ps)
    }
  })

  val two = timer(() => {
    for (x <- 1 to 1000000) {
      sj.read[Int]("12345")
    }
  })

  println(" X: " + one)
  println("v5: " + two)

  def timer(fn: () => Unit): Long = {
    val now = System.currentTimeMillis()
    fn()
    val later = System.currentTimeMillis()
    return (later - now)
  }

}
