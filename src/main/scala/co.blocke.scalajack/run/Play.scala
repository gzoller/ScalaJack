package co.blocke.scalajack
package run

import co.blocke.scala_reflection.*
import scala.jdk.CollectionConverters.*
import scala.reflect.ClassTag
import json.*

object RunMe extends App:

  // import scala.util.Random
  // val random = new Random()

  // def scramble(hash: Int): String =
  //   val last5 = f"$hash%05d".takeRight(5)
  //   val digits = (1 to 5).map(_ => random.nextInt(10))
  //   if digits(0) % 2 == 0 then s"${last5(0)}${digits(0)}${last5(1)}${digits(1)}${last5(2)}-${digits(2)}${last5(3)}${digits(3)}-${last5(4)}${digits(4)}A"
  //   else s"${digits(0)}${last5(0)}${digits(1)}${last5(1)}${digits(2)}-${last5(2)}${digits(3)}${last5(3)}-${digits(4)}${last5(4)}B"

  try

    import json.*
    import ScalaJack.*

    // val inst = Blah("wow", Some(111)) // Some(Some(None))) // Some(Some(3)))
    // val js = sj[Blah].toJson(inst)
    // println(js)

    // co.blocke.scalajack.internal.CodePrinter.code {
    //   sj[Record]
    // }

    val y = Foo("You", Dog("Fido", 4))
    val js2 = sj[Foo]
    println(js2)
    // val v = Foo("Hey", Fish("Bloop", None), None, Color.Blue)
    // val v = Foo("Hey", "Boo")

    // println(ScalaJack[Foo].toJson(v))
    // println(sj[Foo](JsonConfig.withTypeHintLabel("bogus")).toJson(v))

    // println(sj[Record].toJson(record))

    // println("------")

    // println(sj[Record].fromJson(jsData))
  catch {
    case t: Throwable =>
      println(s"BOOM ($t): " + t.getMessage)
      t.printStackTrace
  }

  // val s1 = scramble(15)
  // val s2 = scramble(394857)
  // println(s1)
  // println(s2)
  // println(descrambleTest(s1, 15))
  // println(descrambleTest(s2, 394857))
