package co.blocke.scalajack
package json.test.misc

import org.scalatest.{ FunSpec, GivenWhenThen, BeforeAndAfterAll }
import org.scalatest.Matchers._
import scala.util.Try
import java.util.UUID

class ComplexRelationships extends FunSpec with GivenWhenThen with BeforeAndAfterAll {

  val sj = ScalaJack()

  describe("---------------------------------\n:  Complex Relationships Tests  :\n---------------------------------") {
    it("Child class declares a type parameter not provided by the parent (trait)") {
      val inst: Parent[Int, Boolean] = Child(1, true, "here")
      val js = sj.render(inst)
      assertResult("""{"_hint":"co.blocke.scalajack.json.test.misc.Child","a":1,"b":true,"c":"here"}""") { js }
      assertResult(inst) {
        sj.read[Parent[Int, Boolean]](js)
      }
    }
    it("Parameterized type implements non-parameterized trait") {
      val inst: Pet = Dog("Fido", Dog("Larry", true))
      val js = sj.render(inst)
      assertResult("""{"_hint":"co.blocke.scalajack.json.test.misc.Dog","name":"Fido","kind":{"_hint":"co.blocke.scalajack.json.test.misc.Dog","name":"Larry","kind":true}}""") { js }
      assertResult(classOf[java.lang.Boolean]) {
        inst.asInstanceOf[Dog[_]].kind.asInstanceOf[Dog[_]].kind.getClass
      }
      assertResult(classOf[java.lang.Boolean]) {
        val found = sj.read[Pet](js)
        found.asInstanceOf[Dog[_]].kind.asInstanceOf[Dog[_]].kind.getClass
      }
    }
    it("Parameterized type implements non-parameterized trait (using Long for parameter type") {
      val inst: Pet = Dog("Fido", Dog("Larry", 15L))
      val js = sj.render(inst)
      assertResult("""{"_hint":"co.blocke.scalajack.json.test.misc.Dog","name":"Fido","kind":{"_hint":"co.blocke.scalajack.json.test.misc.Dog","name":"Larry","kind":15}}""") { js }
      assertResult(classOf[java.lang.Long]) {
        inst.asInstanceOf[Dog[_]].kind.asInstanceOf[Dog[_]].kind.getClass
      }
      // Loss of fidelity on read: Long -> Byte
      assertResult(classOf[java.lang.Byte]) {
        val found = sj.read[Pet](js)
        found.asInstanceOf[Dog[_]].kind.asInstanceOf[Dog[_]].kind.getClass
      }
    }
    it("Parameterized type implements non-parameterized trait (using enum for parameter type)") {
      val inst: Pet = Dog("Fido", Kind.Pug)
      val js = sj.render(inst)
      assertResult("""{"_hint":"co.blocke.scalajack.json.test.misc.Dog","name":"Fido","kind":"Pug"}""") { js }
      assertResult(Kind.Pug.getClass) {
        inst.asInstanceOf[Dog[_]].kind.getClass
      }
      // Loss of fidelity on read:  Kind.Pug (Enumeration) -> String
      assertResult(classOf[String]) {
        val found = sj.read[Pet](js)
        found.asInstanceOf[Dog[_]].kind.getClass
      }
    }
    it("Parameterized type implements non-parameterized trait (with Map)") {
      val inst: Pet = Dog("Fido", Map("Larry" -> true))
      val js = sj.render(inst)
      assertResult("""{"_hint":"co.blocke.scalajack.json.test.misc.Dog","name":"Fido","kind":{"Larry":true}}""") { js }
      assertResult((classOf[String], classOf[java.lang.Boolean])) {
        val found = sj.read[Pet](js)
        val c: (Any, Any) = found.asInstanceOf[Dog[_]].kind.asInstanceOf[Map[_, _]].head
        (c._1.getClass, c._2.getClass)
      }
    }
  }
}
