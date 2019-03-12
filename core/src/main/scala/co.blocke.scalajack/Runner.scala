package co.blocke.scalajack

// $COVERAGE-OFF$This file is for debugging only!

import util._

trait Thing {
  val name: String
}
case class HardThing(name: String, weight: Int) extends Thing
case class SoftThing(name: String, isFluffy: Boolean) extends Thing

case class Message[T <: Thing](id: Int, payload: T) {
  type kind = T
}
case class MessageShell(kind: String, id: Int) extends SJCapture

trait Comm
case class Event(happening: Int) extends Comm
trait Command extends Comm { val goDo: String }
case class SimpleCommand(goDo: String, public: Boolean) extends Command
case class CommMessage[T <: Comm](id: Int, payload: T) {
  type kind = T
}

object Runner extends App {

  val sj = ScalaJack()

  /*
  val mixedMsgs = (1 to 10).map(i => if (i % 2 == 0) sj.render(Message(i, HardThing("foo", 3))) else sj.render(Message(i, SoftThing("floof", true))))
  val msgs = (1 to 1000).map(i => if (i % 2 == 0) sj.render[Thing](HardThing("foo", 3)) else sj.render[Thing](SoftThing("floof", true)))

  mixedMsgs.foreach { m =>
    sj.read[MessageShell](m).kind match {
      case "co.blocke.scalajack.HardThing" =>
        sj.read[Message[HardThing]](m)
      case _ => // ignore
        println("Nope: " + m)
    }
  }
  */

  val m = CommMessage(1, SimpleCommand("doit", true).asInstanceOf[Command])
  println(sj.render[CommMessage[_]](m))

  // Should produce:
  // {"kind":"co.blocke.scalajack.Command","id":1,"payload":{"_hint":"co.blocke.scalajack.Command","goDo":"doit"}}
}
// $COVERAGE-ON$

