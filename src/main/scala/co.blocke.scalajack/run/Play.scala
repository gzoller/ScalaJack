package co.blocke.scalajack
package run

import co.blocke.scala_reflection.*

enum Color:
  case Red, Green, Blue

object RunMe extends App:

  /*
  val f = () => parser.expectLong
  val f2 = () => parser.expectList[Long](() => parser.expectLong)
  val r = parser.expectList[List[Long]](f2)

  println("R: " + r)
   */

  given json.JsonConfig = json
    .JsonConfig()
  // .copy(enumsAsIds = '*')

  try
    val x = Blah("foo", (5, true, "wow"))
    val js = ScalaJack.write(x)
    println(js)
    val inst = ScalaJack.read[Blah](js)
    println(inst)

  catch {
    case t: Throwable =>
      println(s"BOOM ($t): " + t.getMessage)
      println(t.printStackTrace)
      println(t.getClass.getName)
  }
