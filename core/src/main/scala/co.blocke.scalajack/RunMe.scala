package co.blocke.scalajack

import org.apache.commons.text.StringEscapeUtils.escapeJava

case class Bar(size: Int, ok: String)
case class Foo[T](m: T)
case class TT(t: (String, String))

case class Person(name: String, age: Int)

trait X
case class A() extends X
case class B() extends X
case class C() extends X

object RunMe extends App {

  val sj = ScalaJack()

  // What happens when Map key is null? (vs None key)

  val z: Seq[(Int, X)] = Seq((1, A()), (2, B()), (3, A()), (4, B()))
  val x = z.exists(_._2.isInstanceOf[C])
  println(x)
}

