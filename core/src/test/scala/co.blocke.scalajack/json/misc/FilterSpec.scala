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
      val filter = sj.filter[SimpleCommand]() // Note -> No hint label here!
      val z = p match {
        case filter(x) => x
        case _         => false
      }
      z should be(SimpleCommand("simple", false))
    }
    it("Must filter concrete wrapped classes") {
      val p = sj.parse(jsCmd2)
      val filter = sj.filter[CommMessage[SimpleCommand]]("kind")
      val z = p match {
        case filter(x) => x
        case _         => false
      }
      z should be(CommMessage(1, SimpleCommand("doit", true)))
    }
    it("Must fall through if type member hint is unknown") {
      val js = """{"kind":"co.blocke.scalajack.vEvent","id":1,"payload":{"happening":5}}"""
      val filter = sj.filter[CommMessage[Event]]("kind")
      val z = sj.parse(js) match {
        case filter(x) => 1
        case _         => 2
      }
      z should be(2)
    }
    it("Must filter trait wrapped classes") {
      val p = sj.parse(jsCmd)
      val filter = sj.filter[CommMessage[Command]]("kind")
      val z = p match {
        case filter(x) => x
        case _         => false
      }
      z should be(CommMessage(1, SimpleCommand("doit", true)))
    }
    it("Must fall through filter") {
      val p = sj.parse(jsCmd)
      val filterCmd = sj.filter[CommMessage[Command]]("kind")
      val filterEvt = sj.filter[CommMessage[Event]]("kind")
      val z = p match {
        case filterCmd(x) => x
        case filterEvt(x) => x
        case _            => false
      }
      z should be(CommMessage(1, SimpleCommand("doit", true)))
    }
    it("Must fall through filter 2") {
      val p = sj.parse("5")
      val filterCmd = sj.filter[CommMessage[Command]]("kind")
      val filterEvt = sj.filter[CommMessage[Event]]("kind")
      val filterInt = sj.filter[Int]()
      val z = p match {
        case filterCmd(x) => x
        case filterEvt(x) => x
        case filterInt(x) => x
        case _            => false
      }
      z should be(5)
    }
    it("Must gracefully fail if input not parsable") {
      val p = sj.parse(jsCmd)
      val filterEvt = sj.filter[CommMessage[Event]]("kind")
      val filterInt = sj.filter[Int]()
      p match {
        case filterEvt(x) => x
        case filterInt(x) => x
        case _            =>
      }
      // Successful do-nothing completion means test passed, i.e. fall-thru was graceful
    }
    /*
    it("Flag extra Json") {
      val p = sj.parse(jsCmd + "}")
      val msg =
        """[$]: Extra input after read.
          |...misc.SimpleCommand","goDo":"doit","public":true}}}
          |----------------------------------------------------^""".stripMargin
      val filter = sj.filter[CommMessage[Command]]("kind")
      the[model.ReadInvalidError] thrownBy {
        p match {
          case filter(x) => x
          case _         => false
        }
      } should have message msg
    }
     */
    it("Filter with type modifier (successful mod)") {
      val sjx = ScalaJack().withTypeValueModifier(model.ClassNameHintModifier((hint: String) => "co.blocke.scalajack.json.misc." + hint, (cname: String) => cname.split('.').last))
      val p = sjx.parse("""{"kind":"Command","id":1,"payload":{"_hint":"co.blocke.scalajack.json.misc.SimpleCommand","goDo":"doit","public":true}}""")
      val filter = sjx.filter[CommMessage[Command]]("kind")
      val z = p match {
        case filter(x) => x
        case _         => false
      }
      z should be(CommMessage(1, SimpleCommand("doit", true)))
    }
    it("Filter with type modifier (failed mod)") {
      val sjx = ScalaJack().withTypeValueModifier(model.ClassNameHintModifier((hint: String) => "co.blocke.scalajack.json.misc." + hint, (cname: String) => cname.split('.').last))
      val p = sjx.parse("""{"kind":"Nothing","id":1,"payload":{"_hint":"co.blocke.scalajack.json.misc.SimpleCommand","goDo":"doit","public":true}}""")
      val filter = sjx.filter[CommMessage[Command]]("kind")
      val z = p match {
        case filter(x) => x
        case _         => false
      }
      z should equal(false)
    }
    it("Filter when hit exists but we can't successfully marshal the type from it") {
      val p = sj.parse("""{"kind":"co.blocke.scalajack.json.misc.Bogus","id":1,"payload":{"goDo":"doit","public":true}}""")
      val filter = sj.filter[CommMessage[SimpleCommand]]("kind")
      val z = p match {
        case filter(x) => x
        case _         => false
      }
      z should equal(false)
    }
  }
}
