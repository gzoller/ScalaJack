package co.blocke.scalajack

// $COVERAGE-OFF$This file is for debugging only!

import util._

object Runner extends App {

  val sj = ScalaJack()

  val p = Path.Unknown \ "{something" \ "{else}" \ "[blather]" \ "[hmm" \ 3 \ "one.two"
  println(p)
  println(p + "next")
}
// $COVERAGE-ON$

