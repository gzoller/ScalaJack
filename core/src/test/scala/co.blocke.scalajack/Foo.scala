package co.blocke.scalajack
package test

import org.scalatest.{ FunSpec, Matchers }
import java.util.UUID
import scala.reflect.runtime.universe.typeOf

case class Hey(name: String) extends SJCapture

class Foo() extends FunSpec with Matchers {

  val sj = ScalaJack()

  describe("-----------------------------------\n:  ValueClass Noncanonical Tests  :\n-----------------------------------") {
    describe("+++ Positive Primitive Tests +++") {
      it("Capture & Regurgitate") {
        val js = """{"name":"Greg", "foo":"bar"}"""
        println(sj.read[Hey](js))
        // val h = Hey("Greg")
        // h.capture("What's up doc?")
        // println(h.regurgitate())
      }
    }
  }
}