package co.blocke.scalajack
package json
package classes

import ScalaJack.*
import co.blocke.scala_reflection.*
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers.*
import org.scalatest.*
import scala.util.*
import TestUtil.*

import java.util.UUID

class ClassSpec() extends AnyFunSpec with JsonMatchers:
  opaque type phone = String

  describe(colorString("-------------------------------\n:         Class Tests         :\n-------------------------------", Console.YELLOW)) {
    describe(colorString("+++ Positive Tests +++")) {
      it("Simple case class must work") {
        val inst = Person("Bob", 34)
        val js = sj[Person].toJson(inst)
        js should matchJson("""{"name":"Bob","duration":34}""")
      }
      it("Inherited class must work") {
        val inst = Child("Bob", 34, 3)
        val js = sj[Child].toJson(inst)
        js should matchJson("""{"name":"Bob","age":34,"phase":3}""")
      }
      it("Non-constructor fields of class must work") {
        val inst = Parent(99)
        inst.hidden_=(true)
        inst.nope_=(false)
        val js = sj[Parent].toJson(inst)
        js should matchJson("""{"phase":99,"foo":"ok","hidden":true}""")
      }
      it("Block non-constructor fields of class must work") {
        val inst = Parent(99)
        inst.hidden_=(true)
        inst.nope_=(false)
        val js = sj[Parent](JsonConfig.withWriteNonConstructorFields(false)).toJson(inst)
        js should matchJson("""{"phase":99}""")
      }
      it("Parameterized class must work") {
        val num: phone = "123-456-7890"
        val inst = Params(List(Person("Bob", 34), Person("Sarah", 28)), Some(num))
        val js = sj[Params[Person, phone]].toJson(inst)
        js should matchJson("""{"a":[{"name":"Bob","duration":34},{"name":"Sarah","duration":28}],"b":"123-456-7890"}""")
      }

      it("Sealed trait with case objects and case classes must work") {
        val inst = TraitHolder(Start, Fish("Beta", false), Miami(101.1))
        val js = sj[TraitHolder].toJson(inst)
        js should matchJson("""{"a":"Start","b":{"_hint":"Fish","species":"Beta","freshwater":false},"c":{"_hint":"Miami","temp":101.1}}""")
      }
      it("Sealed trait with modified type hint label must work") {
        val inst = TraitHolder(Start, Fish("Beta", false), Miami(101.1))
        val js = sj[TraitHolder](JsonConfig.withTypeHintLabel("ref")).toJson(inst)
        js should matchJson("""{"a":"Start","b":{"ref":"Fish","species":"Beta","freshwater":false},"c":{"ref":"Miami","temp":101.1}}""")
      }
      it("Sealed trait with type hint policy SCRAMBLE_CLASSNAME label must work") {
        val inst = TraitHolder(Start, Fish("Beta", false), Miami(101.1))
        val js = sj[TraitHolder](JsonConfig.withTypeHintPolicy(TypeHintPolicy.SCRAMBLE_CLASSNAME)).toJson(inst)
        val diff = parseJValue(js).diff(parseJValue("""{"a":"Start","b":{"_hint":"82949-049-49A","species":"Beta","freshwater":false},"c":{"_hint":"53150-867-73B","temp":101.1}}"""))
        val diffMap = diff.changed.values.asInstanceOf[Map[String, Map[String, _]]]
        assert(diffMap("b").contains("_hint") && diffMap("c").contains("_hint") == true)
      }
      it("Sealed trait with type hint policy USE_ANNOTATION label must work") {
        val inst = TraitHolder(Start, Fish("Beta", false), Miami(101.1))
        val js = sj[TraitHolder](JsonConfig.withTypeHintPolicy(TypeHintPolicy.USE_ANNOTATION)).toJson(inst)
        js should matchJson("""{"a":"Start","b":{"_hint":"flipper","species":"Beta","freshwater":false},"c":{"_hint":"vice","temp":101.1}}""")
      }

      it("Sealed abstract class with case objects and case classes must work") {
        val inst = AbstractClassHolder(Start2, Fish2("Beta", false), Miami2(101.1))
        val js = sj[AbstractClassHolder].toJson(inst)
        js should matchJson("""{"a":"Start2","b":{"_hint":"Fish2","species":"Beta","freshwater":false},"c":{"_hint":"Miami2","temp":101.1}}""")
      }
      it("Sealed abstract class with modified type hint label must work") {
        val inst = AbstractClassHolder(Start2, Fish2("Beta", false), Miami2(101.1))
        val js = sj[AbstractClassHolder](JsonConfig.withTypeHintLabel("ref")).toJson(inst)
        js should matchJson("""{"a":"Start2","b":{"ref":"Fish2","species":"Beta","freshwater":false},"c":{"ref":"Miami2","temp":101.1}}""")
      }
      it("Sealed abstract class with type hint policy SCRAMBLE_CLASSNAME label must work") {
        val inst = AbstractClassHolder(Start2, Fish2("Beta", false), Miami2(101.1))
        val js = sj[AbstractClassHolder](JsonConfig.withTypeHintPolicy(TypeHintPolicy.SCRAMBLE_CLASSNAME)).toJson(inst)
        val diff = parseJValue(js).diff(parseJValue("""{"a":"Start2","b":{"_hint":"82949-049-49A","species":"Beta","freshwater":false},"c":{"_hint":"53150-867-73B","temp":101.1}}"""))
        val diffMap = diff.changed.values.asInstanceOf[Map[String, Map[String, _]]]
        assert(diffMap("b").contains("_hint") && diffMap("c").contains("_hint") == true)
      }
      it("Sealed abstract class with type hint policy USE_ANNOTATION label must work") {
        val inst = AbstractClassHolder(Start2, Fish2("Beta", false), Miami2(101.1))
        val js = sj[AbstractClassHolder](JsonConfig.withTypeHintPolicy(TypeHintPolicy.USE_ANNOTATION)).toJson(inst)
        js should matchJson("""{"a":"Start2","b":{"_hint":"flipper","species":"Beta","freshwater":false},"c":{"_hint":"vice","temp":101.1}}""")
      }

      it("Self-referencing class must work (bonus: parameterized self-referencing class)") {
        val inst = Empl("abc123", 5, Empl("xyz11", -1, null, Nil), List(Empl("tru777", 0, null, Nil), Empl("pop9", 9, null, Nil)))
        val js = sj[Empl[Int]].toJson(inst)
        js should matchJson("""{"id":"abc123","data":5,"boss":{"id":"xyz11","data":-1,"boss":null,"coworkers":[]},"coworkers":[{"id":"tru777","data":0,"boss":null,"coworkers":[]},{"id":"pop9","data":9,"boss":null,"coworkers":[]}]}""")
      }

      it("Java classes must work") {
        val inst = new SampleClass("John Doe", 45, "123 Main St")
        val js = sj[SampleClass].toJson(inst)
        js should matchJson("""{"address":"123 Main St","name":"John Doe"}""")
      }
    }
  }
