package co.blocke.scalajack
package run

import co.blocke.scala_reflection.*

object RunMe extends App:

  val p = Person("Greg", 57, List(false, true, true), Colors.Blue, "Fred".asInstanceOf[BigName])

  val d = Dog("Fido", 4, 2, Some(Dog("Mindy", 4, 0, None)))
  val d2 = Dog("Spot", 4, 3, Some(Dog("Floppy", 3, 1, None)))

  val cfg = json.JsonConfig()
  println(Codec.write(d)(using cfg))
  val t0 = System.currentTimeMillis()
  for i <- 0 to 10000 do
    Codec.write(d)(using cfg)
    if i % 100 == 0 then println(i)
    // if i == 10000 then println(i)

  // println(Codec.write(d)(using cfg))
  // println("")
  // println(Codec.write(d2)(using cfg))
  // println("")
  // println(Codec.write(d)(using cfg))

  val t1 = System.currentTimeMillis()
  println("TIME: " + (t1 - t0))
