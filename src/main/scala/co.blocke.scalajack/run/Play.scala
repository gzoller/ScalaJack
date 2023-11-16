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

    // val thing = Foo("Greg", 57)

    val jjs = """{"name":"Greg","age":57}"""
    // println(ScalaJack.read[Foo](jjs))

    val record = ScalaJack.read[Record](jsData) match
      case Right(r) => r
      case Left(t)  => throw t

    println(record)

    val r2 = ScalaJack.read[Record](jsData)
    println(ScalaJack.write(record))

    implicit val z: json.sj[Record] = ScalaJack.inspect[Record]
    println(sj[Record].decodeJson(jsData))

  catch {
    case t: Throwable =>
      println(s"BOOM ($t): " + t.getMessage)
      t.printStackTrace
  }
