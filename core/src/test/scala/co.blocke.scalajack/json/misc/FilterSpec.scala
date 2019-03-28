package co.blocke.scalajack
package json.misc

import org.scalatest.{ FunSpec, Matchers }

trait Comm
case class Event(happening: Int) extends Comm
trait Command extends Comm { val goDo: String }
case class SimpleCommand(goDo: String, public: Boolean) extends Command
case class CommMessage[T <: Comm](id: Int, payload: T) {
  type kind = T
}

class FilterSpec extends FunSpec with Matchers {

  val sj = ScalaJack()

  val jsCmd = sj.render(CommMessage(1, SimpleCommand("doit", true).asInstanceOf[Command])) // asInstanceOf is important here!

  // {"goDo":"simple","public":false}
  val jsSimple = sj.render(SimpleCommand("simple", false))

  // {"kind":"co.blocke.scalajack.json.misc.SimpleCommand","id":1,"payload":{"goDo":"doit","public":true}}
  val jsCmd2 = sj.render(CommMessage(1, SimpleCommand("doit", true)))

  val jsEvt = sj.render(CommMessage(2, Event(99)))

  describe("------------------\n:  Filter Tests  :\n------------------") {
    it("Must filter simple classes") {
      val p = sj.parse(jsSimple)
      val x = sj.filter[SimpleCommand]().apply(p) // Note -> No hint label here!
      x should be(Some(SimpleCommand("simple", false)))
    }
    it("Must filter concrete wrapped classes") {
      val p = sj.parse(jsCmd2)
      val x = sj.filter[CommMessage[SimpleCommand]]("kind").apply(p)
      x should be(Some(CommMessage(1, SimpleCommand("doit", true))))
    }
    it("Must filter trait wrapped classes") {
      val p = sj.parse(jsCmd)
      val x = sj.filter[CommMessage[Command]]("kind").apply(p)
      x should be(Some(CommMessage(1, SimpleCommand("doit", true).asInstanceOf[Command])))
    }
    it("Must fall through filter") {
      val p = sj.parse(jsCmd)
      val filterCmd = sj.filter[CommMessage[Command]]("kind")
      val filterEvt = sj.filter[CommMessage[Event]]("kind")
      val x = filterEvt(p).orElse(filterCmd(p))
      x should be(Some(CommMessage(1, SimpleCommand("doit", true).asInstanceOf[Command])))
    }
    it("Must fall through filter 2") {
      val p = sj.parse("5")
      val filterCmd = sj.filter[CommMessage[Command]]("kind")
      val filterEvt = sj.filter[CommMessage[Event]]("kind")
      val filterInt = sj.filter[Int]()
      val x = filterEvt(p).orElse(filterCmd(p)).orElse(filterInt(p))
      x should be(Some(5))
    }
    it("Must gracefully fail if input not parsable") {
      val p = sj.parse(jsCmd)
      val filterEvt = sj.filter[CommMessage[Event]]("kind")
      val filterInt = sj.filter[Int]()
      val x = filterEvt(p).orElse(filterInt(p))
      x should be(None)
    }
    it("Flag extra Json") {
      val p = sj.parse(jsCmd + "}")
      val msg =
        """[$]: Extra input after read.
          |...misc.SimpleCommand","goDo":"doit","public":true}}}
          |----------------------------------------------------^""".stripMargin
      the[model.ReadInvalidError] thrownBy sj.filter[CommMessage[Command]]("kind").apply(p) should have message msg
    }
    it("Filter with type modifier (successful mod)") {
      val sjx = ScalaJack().withTypeValueModifier(model.ClassNameHintModifier((hint: String) => "co.blocke.scalajack.json.misc." + hint, (cname: String) => cname.split('.').last))
      val p = sjx.parse("""{"kind":"Command","id":1,"payload":{"_hint":"co.blocke.scalajack.json.misc.SimpleCommand","goDo":"doit","public":true}}""")
      val x = sjx.filter[CommMessage[Command]]("kind").apply(p)
      x should be(Some(CommMessage(1, SimpleCommand("doit", true).asInstanceOf[Command])))
    }
    it("Filter with type modifier (failed mod)") {
      val sjx = ScalaJack().withTypeValueModifier(model.ClassNameHintModifier((hint: String) => "co.blocke.scalajack.json.misc." + hint, (cname: String) => cname.split('.').last))
      val p = sjx.parse("""{"kind":"Nothing","id":1,"payload":{"_hint":"co.blocke.scalajack.json.misc.SimpleCommand","goDo":"doit","public":true}}""")
      val x = sjx.filter[CommMessage[Command]]("kind").apply(p)
      x should be(None)
    }
    it("Filter when hit exists but we can't successfully marshal the type from it") {
      val p = sj.parse("""{"kind":"co.blocke.scalajack.json.misc.Bogus","id":1,"payload":{"goDo":"doit","public":true}}""")
      val x = sj.filter[CommMessage[SimpleCommand]]("kind").apply(p)
      x should be(None)
    }
  }
}
