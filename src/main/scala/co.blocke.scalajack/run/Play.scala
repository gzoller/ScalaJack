package co.blocke.scalajack
package run

import co.blocke.scala_reflection.*
import scala.jdk.CollectionConverters.*
import scala.reflect.ClassTag

object RunMe extends App:

  given json.JsonConfig = json
    .JsonConfig()
    .copy(noneAsNull = true)
    .copy(writeNonConstructorFields = true)
  // .copy(enumsAsIds = '*')

  try

    import json.*
    import ScalaJack.*

    // internal.CodePrinter.code {
    //   sj[Record]
    // }

    // val thing = Foo("Greg", 57)

    println(sj[Record].toJson(record))
    println("------")
    println(sj[Record].fromJson(jsData))
    // println(sj[Foo].fromJson("""{"name":"Greg","age":57}"""))

    /*
    val jjs = """{"name":"Greg","age":57}"""
    // println(ScalaJack.read[Foo](jjs))
     */

    // val record = sj[Record].fromJson(jsData) match
    //   case Right(r)           => r
    //   case Left(t: Throwable) => throw t

    // println(record)
    // println("------")
    // println(sj[Record].toJson(record))

    // val r2 = ScalaJack.read[Record](jsData)
    // println(ScalaJack.write(record))

    // implicit val z: json.sj[Record] = ScalaJack.inspect[Record]
    // println(sj[Record].decodeJson(jsData))

  catch {
    case t: Throwable =>
      println(s"BOOM ($t): " + t.getMessage)
      t.printStackTrace
  }
