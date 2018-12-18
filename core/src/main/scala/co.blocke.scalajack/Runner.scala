package co.blocke.scalajack

import json._

object Runner extends App {

  val sj = ScalaJack().forType[List[List[String]]]

  println(sj.fastRead("""[["a","b","c"],["a","b","c"]]"""))

  println(sj.read[Map[String, Int]]("""{"a":1,"b":2}"""))

  //  val t = JsonTokenizer()
  //  val tok = t.tokenize("""{"name":"Greg\"Zoller","age":52}""")
  //  println(tok.toList)
}
