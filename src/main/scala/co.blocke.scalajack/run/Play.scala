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

  /*
  val buf = Array.fill(1000)('Z')
  val buf2 = new String(buf)
  val buf3 = buf2.getBytes

  var i = 0
  val t0 = System.nanoTime()
  while i < 1000 do
    buf2.charAt(i)
    i += 1
  val t1 = System.nanoTime()
  println("Time: " + (t1 - t0)) // 1,802,166

  println("------")

  // Wow... so ByteArrayAccess is about 1/2 as fast as not using it. :-O
  i = 0
  val max = 997 // buf2.length
  println("MAX: " + max)
  val t2 = System.nanoTime()
  while i < max do
    co.blocke.scalajack.util.ByteArrayAccess.getInt(buf3, i)
    i += 1
  val t3 = System.nanoTime()
  println("Time: " + (t3 - t2)) // 2,267,500

  def parseInt(s: String): Int = {
    var result = 0
    var sign = 1
    var i = 0

    // Handle optional sign
    if s.charAt(0) == '-' then {
      sign = -1
      i += 1
    } else if s.charAt(0) == '+' then {
      i += 1
    }

    // Parse digits
    while i < s.length do {
      val digit = s.charAt(i) - '0'

      if digit < 0 || digit > 9 then {
        throw new NumberFormatException(s"Invalid character in integer: ${s.charAt(i)}")
      }

      result = (result << 3) + (result << 1) + digit // equivalent to result * 10 + digit

      i += 1
    }

    sign * result
  }

  println("------")
   */

  /*
  var i = 0
  val msg = """This is a test""""
  val cbuf = new Array[Char](4048)
  val ps = reading.ParseString(msg.getBytes)
  val ta = System.nanoTime()
  while i < 1000 do
    val z = ps.parseString(0, msg.length(), cbuf, 0)
    String(cbuf.take(z))
    i += 1
  val tb = System.nanoTime()
  println("Time: " + (tb - ta)) // 3235208

  println("------")

  i = 0
  val src = reading.JsonSource(""""This is a test"""")
  val tc = System.nanoTime()
  while i < 1000 do
    src.expectString()
    src.i = 0
    i += 1
  val td = System.nanoTime()
  println("Time: " + (td - tc)) // 4927792 --> about a 52% improvement!!!
   */

  val msg = """This is a test"Another test""""
  val max = msg.length()
  println("max: " + max)
  val cbufLen = 4048
  val cbuf = new Array[Char](cbufLen)
  val ps = reading.ParseString(msg.getBytes)
  var pos = 0
  val parsedCount = ps.parseString(0, max - pos, cbuf, pos)
  println("Parsed: " + parsedCount)
  println("cbuf: " + new String(cbuf, 0, parsedCount))
  pos = parsedCount + 1

  println("HERE: " + msg.charAt(parsedCount))
  val pc2 = ps.parseString(0, max - pos, cbuf, 15)
  println("2: " + pc2 + " : " + new String(cbuf, 0, pc2))

  // import com.github.plokhotnyuk.jsoniter_scala.core.JsonReader as Wow

  // val jsr = new Wow(msg.getBytes)
  // val a = jsr.parseString(0, cbufLen, cbuf, 0)
  // println("A: " + new String(cbuf, 0, a))

  // val b = jsr.parseString(0, cbufLen, cbuf, a + 1)
  // println("B: " + new String(cbuf, 0, b))
