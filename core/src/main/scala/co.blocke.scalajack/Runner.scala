package co.blocke.scalajack

trait Body
case class AnyBody(stuff: Any, count: Int) extends Body

case class Envelope[T <: Body](id: String, body: T) {
  type Giraffe = T
}

object Runner extends App {

  val sj = ScalaJack()

  val json = """{"id":"ABC","body":{"stuff":{"foo":"bar","blather":false},"_hint":"co.blocke.scalajack.AnyBody","count":5}}"""
  //  try {
  val x = sj.read[Envelope[Body]](json)
  println(x)
  //  } catch {
  //    case t: Throwable => println("Boom! " + t.getMessage())
  //  }

}
