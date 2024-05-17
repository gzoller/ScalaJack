package co.blocke.scalajack
package json
package run

import ScalaJack.*

case class Well(repo: scala.collection.mutable.Map[String, Int] = scala.collection.mutable.Map.empty[String, Int])

object RunMe extends App:

  import ScalaJack.*

  given sj: ScalaJack[Pet] = sjCodecOf[Pet]

  val a = sj.toMsgPack(Pet("Mindy","Frenchie",3))
  println("LEN: "+a.length)
  println(a.toList.map(i => f"$i%02X").mkString(" "))

  println("BACK: "+ sj.fromMsgPack(a))
  println("ok")
  