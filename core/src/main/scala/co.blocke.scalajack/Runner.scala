package co.blocke.scalajack

trait Pet { val name: String }
case class Dog[A](name: String, kind: A) extends Pet
case class EitherHolder[L, R](either: Either[L, R])

object Runner extends App {

  val sj = ScalaJack() //.forType[List[List[String]]]

  val pet: Pet = Dog("Fido", 13)
  println(sj.render[Pet](pet))

}
