package co.blocke.scalajack

case class Person(name: String, age: Int)
trait Pet
case class Animal(name: String) extends Pet

object Hello extends App {

  val sj = ScalaJack()

  val p = Person("Greg", 53)
  val r = sj.forType[Person]

  val js = r.render(p)
  println(js)
  println(r.read(js))

  println(r.render[Pet](Animal("spot")))
}
