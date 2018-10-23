package co.blocke.scalajack
package json.test.misc

import org.scalatest.{ FunSpec, Matchers }

//---------- Standard Scala Enums
object Size extends Enumeration {
  val Small, Medium, Large = Value
}
case class SampleEnum(e1: Size.Value, e2: Size.Value)
import Size._

//---------- Case Objects
sealed trait Weekday

case object Monday extends Weekday
case object Tuesday extends Weekday
case object Wednesday extends Weekday
case object Thursday extends Weekday
case object Friday extends Weekday
case object Saturday extends Weekday
case object Sunday extends Weekday

case class SampleEnum2(e1: Weekday, e2: Weekday)

//---------- Custom
trait Enum[A] {
  trait Value { self: A =>
    _values :+= this
  }
  private var _values = List.empty[A]
  def values = _values
}

object Currency extends Enum[Currency]
sealed trait Currency extends Currency.Value
case object EUR extends Currency
case object GBP extends Currency

case class SampleEnum3(e1: Currency, e2: Currency)

class EnumSpec extends FunSpec with Matchers {

  val sj = ScalaJack()

  describe("-----------------------\n:  Enumeration Tests  :\n-----------------------") {
    describe("+++ Positive Tests +++") {
      it("Standard Scala Enum works") {
        val inst = SampleEnum(Small, null)
        val js = sj.render(inst)
        assertResult { """{"e1":"Small","e2":null}""" }(js)
        assertResult {
          inst
        }(sj.read[SampleEnum](js))

        // Map Key
        val m = Map[Size.Value, Int](Small -> 1, Large -> 2)
        val js2 = sj.render(m)
        assertResult { """{"Small":1,"Large":2}""" }(js2)
        assertResult {
          m
        }(sj.read[Map[Size.Value, Int]](js2))
      }
      it("Case object enums works") {
        val inst = SampleEnum2(Tuesday, null)
        val js = sj.render(inst)
        assertResult { """{"e1":"Tuesday","e2":null}""" }(js)
        assertResult {
          inst
        }(sj.read[SampleEnum2](js))

        // Map Key
        val m = Map[Weekday, Int](Tuesday -> 1, Wednesday -> 2)
        val js2 = sj.render(m)
        assertResult { """{"Tuesday":1,"Wednesday":2}""" }(js2)
        assertResult {
          m
        }(sj.read[Map[Weekday, Int]](js2))
      }
      it("Custom enums works") {
        val inst = SampleEnum3(GBP, null)
        val js = sj.render(inst)
        assertResult { """{"e1":"GBP","e2":null}""" }(js)
        assertResult {
          inst
        }(sj.read[SampleEnum3](js))

        // Map Key
        val m = Map[Currency, Int](GBP -> 1, EUR -> 2)
        val js2 = sj.render(m)
        assertResult { """{"GBP":1,"EUR":2}""" }(js2)
        assertResult {
          m
        }(sj.read[Map[Currency, Int]](js2))
      }
    }
    describe("--- Negative Tests ---") {
      it("Value not known in Scala Enum") {
        val js = """{"e1":"Bogus","e2":null}"""
        val msg = """DeserializationException(1 error):
                    |  [$.e1] Enumeration co.blocke.scalajack.json.test.misc.Size$ does not contain a value named Bogus (reported by: co.blocke.scalajack.typeadapter.EnumerationValueDeserializer)""".stripMargin
        the[DeserializationException] thrownBy sj.read[SampleEnum](js) should have message msg
      }
      it("Value not known in case objects for sealed trait") {
        val js = """{"e1":"Bogus","e2":null}"""
        val msg = """DeserializationException(1 error):
                    |  [$.e1] Expected a valid subclass of co.blocke.scalajack.json.test.misc.Weekday (reported by: co.blocke.scalajack.typeadapter.CaseObjectDeserializer)""".stripMargin
        the[DeserializationException] thrownBy sj.read[SampleEnum2](js) should have message msg
      }
      it("Value not known in case objects for custom enum") {
        val js = """{"e1":"Bogus","e2":null}"""
        val msg = """DeserializationException(1 error):
                    |  [$.e1] Expected a valid subclass of co.blocke.scalajack.json.test.misc.Currency (reported by: co.blocke.scalajack.typeadapter.CaseObjectDeserializer)""".stripMargin
        the[DeserializationException] thrownBy sj.read[SampleEnum3](js) should have message msg
      }
    }
  }
}
