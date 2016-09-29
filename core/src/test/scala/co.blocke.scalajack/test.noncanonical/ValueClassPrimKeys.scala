package co.blocke.scalajack
package test
package noncanonical

import org.scalatest.{ FunSpec, Matchers }
import java.util.UUID
import scala.reflect.runtime.universe.typeOf

class ValueClassPrimKeys() extends FunSpec with Matchers {

  val sj = ScalaJack()

  describe("-----------------------------------\n:  ValueClass Noncanonical Tests  :\n-----------------------------------") {
    describe("+++ Positive Primitive Tests +++") {
      it("Value class of BigDecimal") {
        (pending)
      }
      it("Value class of BigInt") {
        (pending)
      }
      it("Value class of Byte") {
        (pending)
      }
      it("Value class of Boolean") {
        (pending)
      }
      it("Value class of Char") {
        (pending)
      }
      it("Value class of Double") {
        (pending)
      }
      it("Value class of Float") {
        (pending)
      }
      it("Value class of Int") {
        (pending)
      }
      it("Value class of Long") {
        (pending)
      }
      it("Value class of Short") {
        (pending)
      }
      it("Value class of String") {
        (pending)
      }
      it("Value class of UUID") {
        (pending)
      }
    }
    describe("+++ Positive Collection Tests +++") {
      it("Value class of List") {
        (pending)
      }
      it("Value class of Map") {
        (pending)
      }
      it("Value class of Tupple") {
        (pending)
      }
    }
    describe("+++ Positive Complex Tests +++") {
      it("Value class of Case Class") {
        (pending)
      }
      it("Value class of Trait") {
        (pending)
      }
      it("Value class of Parameterized Case Class") {
        (pending)
      }
      it("Value class of Parameterized Trait") {
        (pending)
      }
      it("Value class of Option") {
        (pending)
      }
    }
    describe("--- Negative Tests ---") {
    }
  }
}