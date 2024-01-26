package co.blocke.scalajack
package json
package run

import co.blocke.scala_reflection.*
import scala.jdk.CollectionConverters.*
import scala.reflect.ClassTag
import json.*
import scala.collection.immutable.Queue

class Shape[T](polygon: T)
class Parallelogram()
class Rectangle() extends Parallelogram

object RunMe extends App:

  val suite: Shape[Parallelogram] = new Shape[Parallelogram](new Parallelogram())

  // UPDATE: I have no idea what these two cases actually test!  They seem to do different things...

  // val s = "\"This is a test\""
  // val now = System.nanoTime()
  // (1 to 1000000).map(_ => exp.readFromString(s))
  // val later = System.nanoTime()
  // println("JsonReader: " + (later - now))

  // println("==============================")

  // val s2 = "This is a test\""
  // val now2 = System.nanoTime()
  // (1 to 1000000).map(_ => parseString(reading.JsonSource(s2)))
  // val later2 = System.nanoTime()
  // println("SJ        : " + (later2 - now2))

  // def parseString(in: reading.JsonSource): CharSequence =
  //   // charWithWS(in, '"')
  //   val sb = new reading.FastStringBuilder(64)
  //   while true do
  //     val c = in.readEscapedString()
  //     if c == END_OF_STRING then return sb.buffer // mutable thing escapes, but cannot be changed
  //     sb.append(c.toChar)
  //   throw JsonParseError("Invalid string value detected", in)

  import ScalaJack.*
  import co.blocke.scalajack.run.Record
  println("\n")
  implicit val blah: ScalaJack[List[Queue[Int]]] = sj[List[Queue[Int]]]
  println(ScalaJack[List[Queue[Int]]].fromJson("[[1,2,3],[4,5,6],[7,8,9]]"))
  // implicit val blah: ScalaJack[Record] = sj[Record]
  // println(ScalaJack[Record].fromJson(co.blocke.scalajack.run.jsData))

  println("done.")
