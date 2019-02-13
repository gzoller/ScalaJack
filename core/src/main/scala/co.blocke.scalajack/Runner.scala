package co.blocke.scalajack

case class Person(name: String, age: Int)
trait Pet { val name: String }
case class Dog[A, B, C](name: String, kind: A) extends Pet
case class EitherHolder[L, R](either: Either[L, R])

object Runner extends App {

  val sj = ScalaJack()

  val pet: Pet = Dog("Fido", 13)
  val js = sj.render[Pet](pet)
  println(sj.read[Pet](js))

  val js2 = sj.render[Pet](Dog("Fifi", Person("Greg", 52)))
  println(sj.read[Pet](js2))

}
