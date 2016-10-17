package co.blocke.scalajack
package json.test.collections.plain

import org.scalatest.{ FunSpec, Matchers }
import scala.reflect.runtime.universe.typeOf
import co.blocke.test.PlayerJava

class Options() extends FunSpec with Matchers {

  val sj = ScalaJack()

  describe("--------------------------\n:  Option Tests (Plain)  :\n--------------------------") {
    describe("Scala non-case classes:") {
      it("Option of primitive (in class)") {
        val inst = new PlayerOptionMix()
        inst.name = "Fred"
        inst.age = Some(15)
        val js = sj.render(inst)
        assertResult("""{"age":15,"name":"Fred"}""") { js }
        assertResult("Fred") {
          sj.read[PlayerOptionMix](js).name
        }
      }
      it("Option of Class") {
        val pm = new PlayerMix()
        pm.name = "Mike"
        pm.age = 2
        val inst: Option[PlayerMix] = Some(pm)
        val js = sj.render(inst)
        assertResult("""{"age":2,"name":"Mike"}""") { js }
        assertResult(true) {
          val pmm = sj.read[Option[PlayerMix]](js)
          pmm.get.name == pm.name && pmm.get.age == pm.age
        }

        val inst2: Option[PlayerMix] = None
        val js2 = sj.render(inst2)
        assertResult("") { js2 }
        // Can't read nothing into something

        val inst3: Map[Option[PlayerMix], Int] = Map(None -> 2, Some(pm) -> 1)
        val js3 = sj.render(inst3)
        assertResult("""{"":2,"{\"age\":2,\"name\":\"Mike\"}":1}""") { js3 }
        assertResult(true) {
          val mm = sj.read[Map[Option[PlayerMix], Int]](js3)
          val keys = mm.keySet.toList
          keys(0) == None && keys(1).isInstanceOf[Some[PlayerMix]]
        }
      }
      it("Option of Parameterized Class") {
        val p = new PlayerParamMix[Boolean]()
        p.name = "Larry"
        p.age = Some(true)
        val inst: Option[PlayerParamMix[Boolean]] = Some(p)
        val js = sj.render(inst)
        assertResult("""{"age":true,"name":"Larry"}""") { js }
        assertResult(true) {
          val mm = sj.read[Option[PlayerParamMix[Boolean]]](js)
          mm.get.name == "Larry" && mm.get.age == Some(true)
        }

        val inst2: Option[PlayerParamMix[Boolean]] = None
        val js2 = sj.render(inst2)
        assertResult("") { js2 }
      }
      it("Option is None (in class)") {
        val inst = new PlayerOptionMix()
        inst.name = "Fred"
        val js = sj.render(inst)
        assertResult("""{"name":"Fred"}""") { js }
        assertResult("Fred") {
          sj.read[PlayerOptionMix](js).name
        }
      }
    }
    describe("Java classes:") {
      it("Option of Class") {
        val pm = new PlayerJava()
        pm.setName("Mike")
        pm.setAge(2)
        val inst: Option[PlayerJava] = Some(pm)
        val js = sj.render(inst)
        assertResult("""{"age":2,"name":"Mike"}""") { js }
        assertResult(true) {
          val pmm = sj.read[Option[PlayerJava]](js)
          pmm.get.getName == pm.getName && pmm.get.getAge == pm.getAge
        }

        val inst2: Option[PlayerJava] = None
        val js2 = sj.render(inst2)
        assertResult("") { js2 }
        // Can't read nothing into something

        val inst3: Map[Option[PlayerJava], Int] = Map(None -> 2, Some(pm) -> 1)
        val js3 = sj.render(inst3)
        assertResult("""{"":2,"{\"age\":2,\"name\":\"Mike\"}":1}""") { js3 }
        assertResult(true) {
          val mm = sj.read[Map[Option[PlayerJava], Int]](js3)
          val keys = mm.keySet.toList
          keys(0) == None && keys(1).isInstanceOf[Some[PlayerJava]]
        }
      }
    }
  }
}
