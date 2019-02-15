package co.blocke.scalajack

trait Body
case class AnyBody(stuff: Any, count: Int) extends Body

case class Envelope[T <: Body](id: String, body: T) {
  type Giraffe = T
  type Foo = Int
}

object Runner extends App {

  val sj = ScalaJack()

  val json = """{"Giraffe":"co.blocke.scalajack.AnyBody","id":"ABC","body":{"stuff":{"foo":"bar","blather":false},"count":5}}"""
  try {
    val x = sj.read[Envelope[Body]](json)
    println(x)
  } catch {
    case t: Throwable => println("Boom! " + t.getMessage())
  }

}
