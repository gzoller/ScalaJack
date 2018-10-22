package co.blocke.scalajack
package json.test.collections

import org.scalatest.{ FunSpec, Matchers }
import java.util._
import co.blocke.test._

class JavaColl() extends FunSpec with Matchers {

  val sj = ScalaJack()

  describe("---------------------------\n:  Java Collection Tests  :\n---------------------------") {
    it("Can ser/deser Java Map kinds") {
      val jmap = new HashMap[String, Integer]()
      jmap.put("a", 1)
      jmap.put("b", 2)
      jmap.put("c", 3)

      val jweak = new WeakHashMap[String, Integer]()
      jweak.put("one", 7)
      jweak.put("two", 8)
      jweak.put("three", 9)

      val inst = new JavaMapColl()
      inst.setName("Fred")
      //      inst.setTree(jtree)
      inst.setItems(jmap)
      inst.setAge(45)
      inst.weakling = jweak

      val linked = new LinkedHashMap[String, Integer]
      linked.put("bogus", -3)

      val js = sj.render(inst)
      val x = sj.read[JavaMapColl](js)
      println(js)

      assertResult("""{"fine":false,"items":{"a":1,"b":2,"c":3},"name":"Fred","tree":null}""") { js }

      x.getItems.toString shouldBe "{a=1, b=2, c=3}"
      x.getName shouldBe "Fred"
      x.getTree shouldBe null
    }
    it("Can ser/deser Java Array kinds") {
      val jarray = new ArrayList[String]()
      jarray.add("one")
      jarray.add("two")
      jarray.add("three")

      val jll = new LinkedList[Integer]()
      jll.add(1)
      jll.add(2)
      jll.add(3)

      val inst = new JavaListColl()
      inst.setName("Fred")
      inst.setArr(jarray)
      inst.setNums(jll)

      val js = sj.render(inst)
      val x = sj.read[JavaListColl](js)

      assertResult("""{"arr":["one","two","three"],"fine":false,"name":"Fred","nums":[1,2,3],"things":null}""") { js }

      x.getName shouldBe "Fred"
      x.getArr.toString shouldBe "[one, two, three]"
      x.getNums.toString shouldBe "[1, 2, 3]"
      x.getThings shouldBe null
    }
    it("Can ser/deser Java Set kinds") {
      val jset = new HashSet[String]()
      jset.add("one")
      jset.add("two")
      jset.add("three")

      val jtreeset = new TreeSet[Integer]()
      jtreeset.add(1)
      jtreeset.add(2)
      jtreeset.add(3)

      val inst = new JavaSetColl()
      inst.setName("Fred")
      inst.setArr(jset)
      inst.setNums(jtreeset)

      val js = sj.render(inst)
      val x = sj.read[JavaSetColl](js)

      assertResult("""{"arr":["one","two","three"],"fine":false,"name":"Fred","nums":[1,2,3]}""") { js }

      x.getName shouldBe "Fred"
      x.getArr.toString shouldBe "[one, two, three]"
      x.getNums.toString shouldBe "[1, 2, 3]"
    }
  }
}