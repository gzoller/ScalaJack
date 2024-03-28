package co.blocke.scalajack
package json
package run

import co.blocke.scala_reflection.*
import scala.jdk.CollectionConverters.*
import scala.reflect.ClassTag
import json.*
import scala.collection.immutable.Queue
import co.blocke.scalajack.json.reading.SafeNumbers.double
import co.blocke.scalajack.json.schema.*

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

  // implicit val blah: ScalaJack[Record] = sj[Record] // (JsonConfig.withSuppressedEscapedStrings())
  // println(blah.toJson(record))

//   val f = new FastStringBuilder()
//   val s = """Gregory "William"
// Zoller""" + "\u20A0 wow"
//   f.appendEscaped(s, 0, s.length)
//   println(f.result)

  // implicit val blah: ScalaJack[schema.Schema] = codecOf[schema.Schema](JsonConfig.withSuppressTypeHints())
  // val oride = Map("co.blocke.scalajack.json.run.Header$Who$$Type" -> schema.EnumSchema(List("staff", "customer", "program")))
  // println(ScalaJack[schema.Schema].toJson(schema.JsonSchema.of[DormancyEvent](oride)))

  // NOT YET!
  // implicit val blah: ScalaJack[schema.Schema] = codecOf[schema.Schema](JsonConfig.withSuppressTypeHints())
  // val s = schema.JsonSchema.of[DormancyEvent]
  // println(ScalaJack[schema.Schema].toJson(s))

  // val jssrc = json.reading.JsonSource(""""Red"""")
  // println("E: " + jssrc.expectEnum())

  // implicit val blah: ScalaJack[Pizza] = codecOf[Pizza]
  // val c: Pizza = ScalaJack[Pizza].fromJson("\"READY\"")
  // println("Pizza: " + c)

  opaque type OnOff = Boolean
  opaque type Counter = Short

  val m: Map[OnOff, Counter] = Map(true.asInstanceOf[OnOff] -> 1.asInstanceOf[Counter])
  val n: Map[Boolean, Short] = m.asInstanceOf[Map[Boolean, Short]]
  println(n)

  println("done.")
