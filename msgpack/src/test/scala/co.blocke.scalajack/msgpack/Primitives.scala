package co.blocke.scalajack
package msgpack
package test

import scala.reflect.runtime.universe.typeOf
import org.scalatest.{ FunSpec, GivenWhenThen, BeforeAndAfterAll }
import org.scalatest.Matchers._
import scala.util.Try
import java.util.UUID

trait Bar { val size: Int }
case class Foo(size: Int) extends Bar
case class Person(name: String, age: Bar)

class Primitives extends FunSpec with GivenWhenThen with BeforeAndAfterAll {

  val sj = ScalaJack(MsgPackFlavor())

  describe("---------------------------\n:  Primitive Value Tests  :\n---------------------------") {
    describe("Standard Serialization:") {
      it("Basic class with primitive fields") {
        val inst: Person = Person("Greg", Foo(2))
        val item = sj.render(inst)
        println(item.map("%02X" format _).mkString(" "))
      }
    }
  }
}
