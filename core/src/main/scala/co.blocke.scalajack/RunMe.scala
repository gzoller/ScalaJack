package co.blocke.scalajack

import org.apache.commons.text.StringEscapeUtils.escapeJava

case class Bar(size: Int, ok: String)
case class Foo[T](m: T)
case class TT(t: (String, String))

case class Person(name: String, age: Int)

object RunMe extends App {

  val sj = ScalaJack()

  // What happens when Map key is null? (vs None key)

  val m1 = Map(Some(1) -> true, None -> false)
  val m2 = Map(Some(1) -> true, null.asInstanceOf[Option[Int]] -> false)

  println(sj.render(m1))
  println(sj.render(m2))
}

