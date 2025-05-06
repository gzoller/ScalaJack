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
    it("Simple case class must work (with field renaming)") {
      val inst = Person("Bob", 34)
      val sj = sjCodecOf[Person]
      val js = sj.toJson(inst)
      js should matchJson("""{"name":"Bob","duration":34}""")
      sj.fromJson(js) shouldEqual (inst)
    }
    it("Inherited class must work") {
      val inst = Child("Bob", 34, 3)
      val sj = sjCodecOf[Child]
      val js = sj.toJson(inst)
      js should matchJson("""{"name":"Bob","age":34,"phase":3}""")
      sj.fromJson(js) shouldEqual (inst)
    }
    it("Non-constructor fields of class must work") {
      val inst = Parent(99, List("x", "y"))
      inst.hidden_=(true)
      inst.nope_=(false)
      inst.foo = "we'll see"
      val sj = sjCodecOf[Parent](SJConfig.writeNonConstructorFields)
      val js = sj.toJson(inst)
      js should matchJson("""{"phase":99,"stuff":["x","y"],"foo":"we'll see","hidden":true}""")
      val scrambled = """{"hidden":true,"phase":99,"foo":"we'll see","stuff":["x","y"]}"""
      val re = sj.fromJson(js)
      re.phase shouldEqual (inst.phase)
      re.stuff shouldEqual (inst.stuff)
      re.foo shouldEqual (inst.foo)
      re.hidden shouldEqual (inst.hidden)
    }
    it("Block non-constructor fields of class must work") {
      val inst = Parent(99, List("x", "y"))
      inst.hidden_=(true)
      inst.nope_=(false)
      val sj = sjCodecOf[Parent]
      val js = sj.toJson(inst)
      js should matchJson("""{"phase":99,"stuff":["x","y"]}""")
      val re = sj.fromJson(js)
      re.phase shouldEqual (inst.phase)
      re.stuff shouldEqual (inst.stuff)
      re.foo shouldEqual ("ok")
      re.hidden shouldEqual (false)
    }
    it("Parameterized class must work") {
      val num: phone = "123-456-7890"
      val inst = Params(List(Person("Bob", 34), Person("Sarah", 28)), Some(num))
      val sj = sjCodecOf[Params[Person, phone]]
      val js = sj.toJson(inst)
      js should matchJson("""{"a":[{"name":"Bob","duration":34},{"name":"Sarah","duration":28}],"b":"123-456-7890"}""")
      sj.fromJson(js) shouldEqual (inst)
    }
    it("Sealed abstract class with case objects and case classes must work") {
      val inst = AbstractClassHolder(Start2, Fish2("Beta", false), Miami2(101.1))
      val sj = sjCodecOf[AbstractClassHolder](SJConfig.preferTypeHints) // TODO: Kill the prefer hints!
      val js = sj.toJson(inst)
      println("JS: " + js)
      js should matchJson("""{"a":"Start2","b":{"_hint":"Fish2","species":"Beta","freshwater":false},"c":{"_hint":"Miami2","temp":101.1}}""")
//      js should matchJson("""{"a":"Start2","b":{"species":"Beta","freshwater":false},"c":{"temp":101.1}}""")
      val re = sj.fromJson(js)
      re.a shouldEqual (inst.a)
      re.b shouldEqual (inst.b)
      (re.c.asInstanceOf[Miami2].temp == inst.c.asInstanceOf[Miami2].temp) shouldEqual (true)
    }
    /*
    it("Sealed abstract class with modified type hint label must work") {
      val inst = AbstractClassHolder(Start2, Fish2("Beta", false), Miami2(101.1))
      val sj = sjCodecOf[AbstractClassHolder](SJConfig.preferTypeHints.withTypeHintLabel("ref"))
      val js = sj.toJson(inst)
      js should matchJson("""{"a":"Start2","b":{"ref":"Fish2","species":"Beta","freshwater":false},"c":{"ref":"Miami2","temp":101.1}}""")
      val re = sj.fromJson(js)
      re.a shouldEqual (inst.a)
      re.b shouldEqual (inst.b)
      (re.c.asInstanceOf[Miami2].temp == inst.c.asInstanceOf[Miami2].temp) shouldEqual (true)
    }
    it("Sealed abstract class with type hint policy SCRAMBLE_CLASSNAME label must work") {
      val inst = AbstractClassHolder(Start2, Fish2("Beta", false), Miami2(101.1))
      val sj = sjCodecOf[AbstractClassHolder](SJConfig.preferTypeHints.withTypeHintPolicy(TypeHintPolicy.SCRAMBLE_CLASSNAME))
      val js = sj.toJson(inst)
      val diff = parseJValue(js).diff(parseJValue("""{"a":"Start2","b":{"_hint":"82949-049-49A","species":"Beta","freshwater":false},"c":{"_hint":"53150-867-73B","temp":101.1}}"""))
      val diffMap = diff.changed.values.asInstanceOf[Map[String, Map[String, ?]]]
      assert(diffMap("b").contains("_hint") && diffMap("c").contains("_hint")) // ie only the scrambled _hint values are different
      val re = sj.fromJson(js)
      re.a shouldEqual (inst.a)
      re.b shouldEqual (inst.b)
      (re.c.asInstanceOf[Miami2].temp == inst.c.asInstanceOf[Miami2].temp) shouldEqual (true)
    }
    it("Sealed abstract class with type hint policy USE_ANNOTATION label must work") {
      val inst = AbstractClassHolder(Start2, Fish2("Beta", false), Miami2(101.1))
      val sj = sjCodecOf[AbstractClassHolder](SJConfig.preferTypeHints.withTypeHintPolicy(TypeHintPolicy.USE_ANNOTATION))
      val js = sj.toJson(inst)
      js should matchJson("""{"a":"Start2","b":{"_hint":"flipper","species":"Beta","freshwater":false},"c":{"_hint":"vice","temp":101.1}}""")
      val re = sj.fromJson(js)
      re.a shouldEqual (inst.a)
      re.b shouldEqual (inst.b)
      (re.c.asInstanceOf[Miami2].temp == inst.c.asInstanceOf[Miami2].temp) shouldEqual (true)
    }
    it("Parameterized sealed abstract class must work") {
      val inst = AbstractClassHolder2(Thing2(15L, "wow"))
      val sj = sjCodecOf[AbstractClassHolder2[Long]]
      val js = sj.toJson(inst)
      js should matchJson("""{"a":{"t":15,"s":"wow"}}""")
      val re = sj.fromJson(js)
      re.a.asInstanceOf[Thing2[Long]].t shouldEqual (15L)
      re.a.asInstanceOf[Thing2[Long]].s shouldEqual ("wow")
    }
    it("Top-level abstract class must work") {
      val inst: AThing[Long] = Thing2(99L, "ok")
      val sj = sjCodecOf[AThing[Long]]
      val js = sj.toJson(inst)
      js should matchJson("""{"t":99,"s":"ok"}""")
      val re = sj.fromJson(js)
      re.asInstanceOf[Thing2[Long]].t shouldEqual (99L)
      re.asInstanceOf[Thing2[Long]].s shouldEqual ("ok")
    }
     */
    it("Self-referencing class must work (bonus: parameterized self-referencing class)") {
      val inst = Empl("abc123", 5, Empl("xyz11", -1, null, Nil), List(Empl("tru777", 0, null, Nil), Empl("pop9", 9, null, Nil)))
      val sj = sjCodecOf[Empl[Int]]
      val js = sj.toJson(inst)
      js should matchJson("""{"id":"abc123","data":5,"boss":{"id":"xyz11","data":-1,"boss":null,"coworkers":[]},"coworkers":[{"id":"tru777","data":0,"boss":null,"coworkers":[]},{"id":"pop9","data":9,"boss":null,"coworkers":[]}]}""")
      sj.fromJson(js) shouldEqual (inst)
    }
    /*
    it("Java classes must work") {
      val inst = new SampleClass()
      inst.setName("John Doe")
      inst.setAge(45)
      inst.setAddress("123 Main St")
      val sj = sjCodecOf[SampleClass]
      val js = sj.toJson(inst)
      js should matchJson("""{"address":"123 Main St","name":"John Doe"}""")
      val re = sj.fromJson(js)
      re.getName shouldEqual ("John Doe")
      re.getAge shouldEqual (0)
      re.getAddress shouldEqual ("123 Main St")
    }
    it("Java class value is null") {
      val inst: SampleClass = null
      val sj = sjCodecOf[SampleClass]
      val js = sj.toJson(inst)
      js shouldEqual ("""null""")
      sj.fromJson(js) shouldEqual (null)
    }
    it("Abstract class value is null") {
      val inst = AbstractClassHolder(null, null, null)
      val sj = sjCodecOf[AbstractClassHolder]
      val js = sj.toJson(inst)
      js should matchJson("""{"a":null,"b":null,"c":null}""")
      val re = sj.fromJson(js)
      re.a shouldEqual (null)
      re.b shouldEqual (null)
      re.c shouldEqual (null)
    }
    it("Scala case class value is null") {
      val inst: Person = null
      val sj = sjCodecOf[Person]
      val js = sj.toJson(inst)
      js shouldEqual ("""null""")
      sj.fromJson(js) shouldEqual (inst)
    }
     */
  }
