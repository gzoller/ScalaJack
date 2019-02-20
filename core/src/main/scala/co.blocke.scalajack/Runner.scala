package co.blocke.scalajack

trait Body
case class AnyBody(stuff: Any, count: Int) extends Body
case class DefaultBody(message: String = "Unknown body") extends Body
case class Person(name: String, age: Int)

case class Envelope[T <: Body, U](id: String, body: T, hey: U) {
  type Giraffe = T
  type Bird = U
  type Foo = Int
}

object Runner extends App {

  val sj = ScalaJack().parseOrElse((typeOf[Body] -> typeOf[DefaultBody]))

  val js = "{}"
  println(sj.read[Body](js))

  println(sj.render[Body](AnyBody(true, 3)))

}
