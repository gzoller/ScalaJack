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
case class CommWrapper(kind: Type)

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
  val js = sj.render(m)
  println(js)

  //    val inst = sj.read[CommMessage[Command]](js)
  //    println(inst)

  val e = CommMessage(2, Event(99))
  val js2 = sj.render(e)

  val xjs = js2 //"""{"_hint":"co.blocke.scalajack.HardThing","name":"rock","weight":9}"""

  val p = sj.parse(xjs)
  val filterCmd = sj.filter[CommMessage[Command]]("kind")
  val filterEvt = sj.filter[CommMessage[Event]]("kind")

  val found = filterCmd(p).orElse(filterEvt(p))
  println("Filtering... ")
  found.map(_ match {
    case x: CommMessage[_] if (x.payload.isInstanceOf[Command]) => println("Command: " + x)
    case x: CommMessage[_] if (x.payload.isInstanceOf[Event]) => println("Event: " + x)
    case x => println("Nope: " + x)
  })

  //------------------------------------------
  println("--------------------- Filter X")

  val cmdType = typeOf[Command]
  val evtType = typeOf[Event]
  val t = sj.read[CommWrapper](xjs).kind
  t match {
    case _ if t == cmdType => println("Command: " + sj.read[CommMessage[Command]](xjs))
    case _ if t == evtType => println("Event: " + sj.read[CommMessage[Event]](xjs))
    case _                 =>
  }

  //  sj.filterX[CommMessage[Command]]("kind").mapOrElse(
  //    p,
  //    (c: CommMessage[Command]) => println("Command: " + c),
  //    (t: model.Transceiver[String]) => sj.filterX[CommMessage[Event]]("kind").mapIf(
  //      p,
  //      (c: CommMessage[Event]) => println("Event: " + c)
  //    )
  //  )

  //  sj.filterX[CommMessage[Command]]("kind").orElse[model.Transceiver[String],CommMessage[Event]](sj.filter[CommMessage[Event]]("kind"))

  //  val p = sj.parse(js2)
  //  println("YL " + sj.filterY[CommMessage[Command], Command](p, "kind"))

  //  val m2 = Message(2, HardThing("rock", 9))
  //  println(sj.render(m2))

  // Should produce:
  // {"kind":"co.blocke.scalajack.Command","id":1,"payload":{"_hint":"co.blocke.scalajack.Command","goDo":"doit"}}
}
// $COVERAGE-ON$

