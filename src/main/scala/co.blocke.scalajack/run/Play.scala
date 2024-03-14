package co.blocke.scalajack
package json
package run

import co.blocke.scala_reflection.*
import scala.jdk.CollectionConverters.*
import scala.reflect.ClassTag
import json.*
import scala.collection.immutable.Queue
import co.blocke.scalajack.json.reading.SafeNumbers.double
import co.blocke.scalajack.schema.*

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

  // println(RType.of[Person].pretty)

  implicit val blah: ScalaJack[Record] = sj[Record] // (JsonConfig.withSuppressedEscapedStrings())
  println(blah.toJson(record))

  // println(RType.of[Schema].pretty)

  val f = new FastStringBuilder()
  val s = """Gregory "William"
Zoller""" + "\u20A0 wow"
  f.appendEscaped(s, 0, s.length)
  println(f.result)

  // implicit val blah: ScalaJack[schema.Schema] = sj[schema.Schema](JsonConfig.withSuppressedTypeHints())
  // println(sj.toJson(schema.JsonSchema.of[Person]))

  // println(RType.of[schema.Schema].pretty)

  println("done.")
