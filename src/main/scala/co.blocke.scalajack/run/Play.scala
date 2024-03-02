package co.blocke.scalajack
package json
package run

import co.blocke.scala_reflection.*
import scala.jdk.CollectionConverters.*
import scala.reflect.ClassTag
import json.*
import scala.collection.immutable.Queue
import co.blocke.scalajack.json.reading.SafeNumbers.double

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

  /*
  var i = 0
  val msg = """This is a test""""

  val cbuf = new Array[Char](4048)
  val ta = System.nanoTime()
  while i < 1000 do
    val ps = reading.ParseString(msg.getBytes)
    val z = ps.parseString(0, msg.length(), cbuf, 0)
    new String(cbuf, 0, z)
    i += 1
  val tb = System.nanoTime()
  println("Time (Jsoniter) : " + (tb - ta)) // 3235208

  println("------")

  i = 0
  val tr = System.nanoTime()
  val msgX = "\"" + msg
  while i < 1000 do
    val jsrc = reading.JsonSource(msgX)
    jsrc.expectString()
    i += 1
  val tq = System.nanoTime()
  println("Time (ExpectNew): " + (tq - tr)) // 4927792 --> about a 52% improvement!!!

  println("------")

  i = 0
  val tr2 = System.nanoTime()
  while i < 1000 do
    val jsrc = reading.JsonSource(msgX)
    jsrc.expectStringOld()
    i += 1
  val tq2 = System.nanoTime()
  println("Time (ExpectOld): " + (tq2 - tr2)) // 4927792 --> about a 52% improvement!!!

  println("------")

  i = 0
  val te2 = System.nanoTime()
  while i < 1000 do
    val jsrc = reading.JsonSource(msg)
    val pc = jsrc.parseString(0)
    jsrc.js.subSequence(0, pc)
    i += 1
  val tf2 = System.nanoTime()
  println("Time (PNew)     : " + (tf2 - te2)) // 4927792 --> about a 52% improvement!!!

  println("------")

  i = 0
  val te = System.nanoTime()
  val msgY = "\"" + msg
  while i < 1000 do
    val jsrc = reading.JsonSource(msgY)
    jsrc.parseStringOld()
    i += 1
  val tf = System.nanoTime()
  println("Time (POld)     : " + (tf - te)) // 4927792 --> about a 52% improvement!!!

  println("------")

  i = 0
  val ty = System.nanoTime()
  while i < 1000 do
    blah.fromJson(jsData)
    i += 1
  val tz = System.nanoTime()
  println("Time (ParseAll) : " + (tz - ty))
  // Orig: 79298041
  // New : 58537667
  // Latest: 79713209
  // Newnew: 34919833

  println("------")

  i = 0
  val ty2 = System.nanoTime()
  while i < 1000 do
    val g = reading.JsonSource("true}")
    g.expectBoolean()
    i += 1
  val tz2 = System.nanoTime()
  println("Time (Boolean Old) : " + (tz2 - ty2))
   */
