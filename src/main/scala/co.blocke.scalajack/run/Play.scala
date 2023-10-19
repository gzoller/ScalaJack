package co.blocke.scalajack
package run

import co.blocke.scala_reflection.*

object RunMe extends App:

  val p = Person("Greg", 57, List(false, true, true), Colors.Blue, "Fred".asInstanceOf[BigName])

  val d = Dog("Fido", 4, 2, Some(Dog("Mindy", 4, 0, None)))

  val t0 = System.currentTimeMillis()
  val cfg = json.JsonConfig()
  // for i <- 0 to 10000 do
  // val x = Codec.write(p)(using cfg)
  // if i % 100 == 0 then println(i)
  // if i == 10000 then println(x)

  println(Codec.write(d)(using cfg))

  val t1 = System.currentTimeMillis()
  println("TIME: " + (t1 - t0))
