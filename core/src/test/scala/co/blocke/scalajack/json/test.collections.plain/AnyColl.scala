package co.blocke.scalajack
package json.test.collections.plain

import org.scalatest.{ FunSpec, Matchers }
import co.blocke.test.PlayerJava

class AnyColl() extends FunSpec with Matchers {

  val sj = ScalaJack()

  describe("----------------------------------\n:  Any Collection Tests (Plain)  :\n----------------------------------") {
    describe("Classes with val-specified constructor arguments:") {
      it("List works (Class)") {
        val inst: Any = List(new PlayerVal("Mike", 34), new PlayerVal("Sarah", 29))
        val js = sj.render(inst)
        assertResult("""[{"_hint":"co.blocke.scalajack.json.test.collections.plain.PlayerVal","name":"Mike","age":34},{"_hint":"co.blocke.scalajack.json.test.collections.plain.PlayerVal","name":"Sarah","age":29}]""") { js }
        assertResult(true) {
          val r = sj.read[List[Any]](js).asInstanceOf[List[PlayerVal]]
          val orig = inst.asInstanceOf[List[PlayerVal]]
          (r.length == orig.length && r(0).name == orig(0).name && r(0).age == orig(0).age
            && r(1).name == orig(1).name && r(1).age == orig(1).age)
        }
      }
      it("Map works (Class,Int)") {
        val inst: Any = Map(new PlayerVal("Mike", 34) -> 1, new PlayerVal("Sarah", 29) -> 2)
        val js = sj.render(inst)
        assertResult("""{"{\"_hint\":\"co.blocke.scalajack.json.test.collections.plain.PlayerVal\",\"name\":\"Mike\",\"age\":34}":1,"{\"_hint\":\"co.blocke.scalajack.json.test.collections.plain.PlayerVal\",\"name\":\"Sarah\",\"age\":29}":2}""") { js }
        assertResult(true) {
          val r = sj.read[Any](js).asInstanceOf[Map[PlayerVal, Int]].keySet.toList
          val orig = inst.asInstanceOf[Map[PlayerVal, Int]].keySet.toList
          (r(0).name == orig(0).name
            && r(0).age == orig(0).age
            && r(1).name == orig(1).name
            && r(1).age == orig(1).age)
        }
      }
    }
    describe("Classes with var members + getter/setters:") {
      it("List works (Class)") {
        val p1 = new PlayerMix()
        p1.name = "Mike"
        p1.age = 34
        val p2 = new PlayerMix()
        p2.name = "Sarah"
        p2.age = 29
        val inst: Any = List(p1, p2)
        val js = sj.render(inst)
        assertResult("""[{"_hint":"co.blocke.scalajack.json.test.collections.plain.PlayerMix","age":34,"name":"Mike"},{"_hint":"co.blocke.scalajack.json.test.collections.plain.PlayerMix","age":29,"name":"Sarah"}]""") { js }
        assertResult(true) {
          val r = sj.read[List[Any]](js).asInstanceOf[List[PlayerMix]]
          val orig = inst.asInstanceOf[List[PlayerMix]]
          (r.length == orig.length && r(0).name == orig(0).name && r(0).age == orig(0).age
            && r(1).name == orig(1).name && r(1).age == orig(1).age)
        }
      }
      it("Map works (Class,Int)") {
        val p1 = new PlayerMix()
        p1.name = "Mike"
        p1.age = 34
        val p2 = new PlayerMix()
        p2.name = "Sarah"
        p2.age = 29
        val inst: Any = Map(p1 -> p2)
        val js = sj.render(inst)
        assertResult("""{"{\"_hint\":\"co.blocke.scalajack.json.test.collections.plain.PlayerMix\",\"age\":34,\"name\":\"Mike\"}":{"_hint":"co.blocke.scalajack.json.test.collections.plain.PlayerMix","age":29,"name":"Sarah"}}""") { js }
        assertResult(true) {
          val marshal = sj.read[Any](js).asInstanceOf[Map[PlayerMix, PlayerMix]]
          val rK = marshal.keySet.head
          val rV = marshal(rK)
          val origK = inst.asInstanceOf[Map[PlayerMix, PlayerMix]].keySet.head
          val origV = inst.asInstanceOf[Map[PlayerMix, PlayerMix]](origK)
          (rK.name == origK.name
            && rK.age == origK.age
            && rV.name == origV.name
            && rV.age == origV.age)
        }
      }
    }
    describe("Classes with var members + getter/setters (Java):") {
      it("List works (Class)") {
        val p1 = new PlayerJava()
        p1.setName("Mike")
        p1.setAge(34)
        val p2 = new PlayerJava()
        p2.setName("Sarah")
        p2.setAge(29)
        val inst: Any = List(p1, p2)
        val js = sj.render(inst)
        assertResult("""[{"_hint":"co.blocke.test.PlayerJava","age":34,"name":"Mike"},{"_hint":"co.blocke.test.PlayerJava","age":29,"name":"Sarah"}]""") { js }
        assertResult(true) {
          val r = sj.read[List[Any]](js).asInstanceOf[List[PlayerJava]]
          val orig = inst.asInstanceOf[List[PlayerJava]]
          (r.length == orig.length && r(0).getName == orig(0).getName && r(0).getAge == orig(0).getAge
            && r(1).getName == orig(1).getName && r(1).getAge == orig(1).getAge)
        }
      }
      it("Map works (Class,Int)") {
        val p1 = new PlayerJava()
        p1.setName("Mike")
        p1.setAge(34)
        val p2 = new PlayerJava()
        p2.setName("Sarah")
        p2.setAge(29)
        val inst: Any = Map(p1 -> p2)
        val js = sj.render(inst)
        assertResult("""{"{\"_hint\":\"co.blocke.test.PlayerJava\",\"age\":34,\"name\":\"Mike\"}":{"_hint":"co.blocke.test.PlayerJava","age":29,"name":"Sarah"}}""") { js }
        assertResult(true) {
          val marshal = sj.read[Any](js).asInstanceOf[Map[PlayerJava, PlayerJava]]
          val rK = marshal.keySet.head
          val rV = marshal(rK)
          val origK = inst.asInstanceOf[Map[PlayerJava, PlayerJava]].keySet.head
          val origV = inst.asInstanceOf[Map[PlayerJava, PlayerJava]](origK)
          (rK.getName == origK.getName
            && rK.getAge == origK.getAge
            && rV.getName == origV.getName
            && rV.getAge == origV.getAge)
        }
      }
    }
  }
}
