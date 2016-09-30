package co.blocke.scalajack
package test

import org.scalatest.{ FunSpec, Matchers }
import java.util.UUID
import scala.reflect.runtime.universe.typeOf

case class Hey(name: String) extends SJCapture
case class You(name: String)

class Foo() extends FunSpec with Matchers {

  val sj = ScalaJack()

  describe("-----------------------------------\n:  ValueClass Noncanonical Tests  :\n-----------------------------------") {
    describe("+++ Positive Primitive Tests +++") {
      it("Capture & Regurgitate") {
        // val js = """{"name":"Greg", "foo": {"age":5}}"""
        // val js = """{"name":"Greg", "foo": {"age":5}}"""
        // val js = """{"name":"Greg", "foo": {"age":5}}"""
        val js = """{"name":"Greg", "foo": [1,2,"t"], "zing" :  {"dot":{"age":25,"food":"Pizza"}}, "blather":"wow", "boo": -29384.34, "maybe": false }"""
        val h = sj.read[Hey](js)
        println(h)
        println("---------")
        println(sj.render(h))
        // val h = Hey("Greg")
        // h.capture("What's up doc?")
        // println(h.regurgitate())
      }
    }
  }
}