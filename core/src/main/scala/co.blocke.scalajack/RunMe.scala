package co.blocke.scalajack

trait Comm
case class Event(happening: Int) extends Comm
trait Command extends Comm { val goDo: String }
case class SimpleCommand(goDo: String, public: Boolean) extends Command
case class CommMessage[T <: Comm](id: Int, payload: T) {
  type kind = T
}

import scala.util.Try

case class ThingsICareAbout(one: String, two: Try[String], three: String) // <-- Note the Try!

object RunMe extends App {

  val sj = ScalaJack()

  //  val js = """{"a":1}"""
  //  val js = "123"
  //  val js = sj.render(CommMessage(2, Event(3)))
  val js = sj.render[CommMessage[Command]](CommMessage(3, SimpleCommand("do", true)))
  //  println(sj.render[CommMessage[Command]](CommMessage(3, SimpleCommand("do", true))))
  //  println(sj.render[CommMessage[SimpleCommand]](CommMessage(3, SimpleCommand("do", true))))
  //  println(sj.render(CommMessage(3, SimpleCommand("do", true))))
  //  println("---")
  println(js)

  val filter1 = sj.filter[Map[String, Int]]()
  val filter2 = sj.filter[Int]()
  val filter3 = sj.filter[CommMessage[Command]]("kind")
  val filter4 = sj.filter[CommMessage[Event]]("kind")

  val js2 = """{"kind":"co.blocke.scalajack.Command","id":3,"payload":{"_hint":"co.blocke.scalajack.Bogus","goDo":"do","public":true}}"""
  sj.parse(js2) match {
    case filter1(x) => println(x)
    case filter2(x) => println("Int: " + x)
    case filter3(x) => println("Cmd: " + x)
    case filter4(x) => println("Evt: " + x)
    case x          => println("Other... " + x)
  }
}

