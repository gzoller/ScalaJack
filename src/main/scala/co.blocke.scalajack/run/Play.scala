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
  //   val sb = new reading.FastStringBuilder(64)x
  //   while true do
  //     val c = in.readEscapedString()
  //     if c == END_OF_STRING then return sb.buffer // mutable thing escapes, but cannot be changed
  //     sb.append(c.toChar)
  //   throw JsonParseError("Invalid string value detected", in)

  import ScalaJack.*
  import co.blocke.scalajack.json.run.Record
  println("\n")

  // implicit val blah: ScalaJack[Foom] = sj[Foom]
  // println(ScalaJack[Foom].fromJson("""{"a": -12, "b":"Greg Z"}"""))

  // implicit val blah: ScalaJack[List[Queue[Int]]] = sj[List[Queue[Int]]]
  // println(ScalaJack[List[Queue[Int]]].fromJson("null")) // "[[1,2,3],[4,5,6],[7,8,9]]"))

  implicit val blah: ScalaJack[Record] = sj[Record]
  println(blah.fromJson(jsData))

  println("done.")

  // def r0(in: JsonSource): Foom = {
  //   val fieldMatrix: StringMatrix = new StringMatrix(Array("a", "b"))
  //   val args: Array[Any] = Array[Any](Foom.$lessinit$greater$default$1, Foom.$lessinit$greater$default$2)(ClassTag.Any)

  //   if !in.expectObjectStart() then null.asInstanceOf[Foom]
  //   else {
  //     if in.here.!=('}') then
  //       while {
  //         in.expectFieldName(fieldMatrix) match {
  //           case 0 =>
  //             args(0) = in.expectInt()
  //           case 1 =>
  //             args(1) = in.expectString().toString()
  //           case -1 =>
  //             in.skipValue()
  //         }
  //         in.nextField()
  //       } do ()
  //     else in.read()
  //     ((fieldValues: Array[Any]) => new Foom(fieldValues(0).asInstanceOf[scala.Int], fieldValues(1).asInstanceOf[String])).apply(args)
  //   }
  // }
