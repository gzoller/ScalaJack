package co.blocke.scalajack
package json.test.misc

import org.scalatest.{ FunSpec, Matchers }
import scala.reflect.runtime.universe.typeOf

trait Foo { val a: Int }
trait Bar { val b: Int; val c: Foo }
case class One(a: Int) extends Foo
case class Two(b: Int, c: Foo) extends Bar

class Greg() extends FunSpec with Matchers {

  // val sj = ScalaJack()

  describe("---------------------------\n:  Try and Capture Tests  :\n---------------------------") {
    it("Map key - Option") {

      val strMatchHintMod = StringMatchHintModifier(Map("Ace" -> typeOf[One], "Big" -> typeOf[Two]))
      val sj = ScalaJack()
        .withHintModifiers((typeOf[Foo] -> strMatchHintMod), (typeOf[Bar] -> strMatchHintMod))
        .withHints((typeOf[Foo] -> "mark"), (typeOf[Bar] -> "size"))
      val inst: Bar = Two(3, One(2))
      println(sj.render(inst))

      // val c = Car(4)
      // println(sj.render[Vehicle[Land.type]](c))
    }
  }
}
