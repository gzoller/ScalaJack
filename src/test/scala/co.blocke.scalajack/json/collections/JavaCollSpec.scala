package co.blocke.scalajack
package json
package collections

import ScalaJack.*
import co.blocke.scala_reflection.*
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers.*
import org.scalatest.*
import TestUtil.*

import java.util.{ArrayList, Arrays, HashSet}

class JavaCollSpec() extends AnyFunSpec with JsonMatchers:

  describe(colorString("-------------------------------\n:    Java Collection Tests    :\n-------------------------------", Console.YELLOW)) {
    describe(colorString("+++ Basic functions (Set) +++")) {
      it("Set is null must work") {
        val inst = JSetHolder[Int](null)
        val sj = sjCodecOf[JSetHolder[Int]]
        val js = sj.toJson(inst)
        js should matchJson("""{"a":null}""")
        sj.fromJson(js) shouldEqual (inst)
      }
      it("Set of numeric must work") {
        val inst = JSetHolder[Int](HashSet(Arrays.asList(1, 2, 3)))
        val sj = sjCodecOf[JSetHolder[Int]]
        val js = sj.toJson(inst)
        js should matchJson("""{"a":[1,2,3]}""")
        sj.fromJson(js) shouldEqual (inst)
      }
      it("Set of string must work") {
        val inst = JSetHolder[String](HashSet(Arrays.asList("a", "b", "c")))
        val sj = sjCodecOf[JSetHolder[String]]
        val js = sj.toJson(inst)
        js should matchJson("""{"a":["a","b","c"]}""")
        sj.fromJson(js) shouldEqual (inst)
      }
      it("Set of boolean must work") {
        val inst = JSetHolder[Boolean](HashSet(Arrays.asList(true, false, true)))
        val sj = sjCodecOf[JSetHolder[Boolean]]
        val js = sj.toJson(inst)
        js should matchJson("""{"a":[false,true]}""")
        sj.fromJson(js) shouldEqual (inst)
      }
      it("Set of Set (nested) must work") {
        import java.util.TreeSet as JTreeSet
        import java.util.Comparator
        val setOfSets: java.util.Set[java.util.Set[Int]] = new JTreeSet[java.util.Set[Int]](new Comparator[java.util.Set[Int]] {
          override def compare(o1: java.util.Set[Int], o2: java.util.Set[Int]): Int = -1
        })
        val a: java.util.Set[Int] = new JTreeSet()
        a.add(1)
        a.add(2)
        val b: java.util.Set[Int] = new JTreeSet()
        b.add(3)
        b.add(4)
        setOfSets.add(b)
        setOfSets.add(a)
        val inst = JSetHolder[java.util.Set[Int]](setOfSets)
        val sj = sjCodecOf[JSetHolder[java.util.Set[Int]]]
        val js = sj.toJson(inst)
        js should (matchJson("""{"a":[[1,2],[3,4]]}""") or matchJson("""{"a":[[3,4],[1,2]]}"""))
        sj.fromJson(js) shouldEqual (inst)
      }
      it("Set of either must work") {
        val inst = JSetHolder[Either[Int, Boolean]](HashSet(Arrays.asList(Right(true), Left(15), Right(false))))
        val sj = sjCodecOf[JSetHolder[Either[Int, Boolean]]](SJConfig.withEitherLeftHandling(EitherLeftPolicy.AS_VALUE))
        val js = sj.toJson(inst)
        js should matchJson("""{"a":[true,15,false]}""")
        sj.fromJson(js) shouldEqual (inst)
      }
      it("Set of union must work") {
        val inst = JSetHolder[Int | Boolean](HashSet(Arrays.asList(true, 15, false)))
        val sj = sjCodecOf[JSetHolder[Int | Boolean]]
        val js = sj.toJson(inst)
        js should matchJson("""{"a":[false,true,15]}""")
        sj.fromJson(js) shouldEqual (inst)
      }
      it("Set of option must work") {
        val inst = JSetHolder[Option[Int]](HashSet(Arrays.asList(Some(1), None, Some(3))))
        val sj = sjCodecOf[JSetHolder[Option[Int]]]
        val js = sj.toJson(inst)
        js should matchJson("""{"a":[1,3]}""")
        sj.fromJson(js) shouldEqual (JSetHolder[Option[Int]](HashSet(Arrays.asList(Some(1), Some(3)))))
      }
      it("Set of map must work") {
        val inst = JSetHolder[Map[String, Int]](HashSet(Arrays.asList(Map("a" -> 1, "b" -> 2), Map("c" -> 3, "d" -> 4))))
        val sj = sjCodecOf[JSetHolder[Map[String, Int]]]
        val js = sj.toJson(inst)
        js should matchJson("""{"a":[{"a":1,"b":2},{"c":3,"d":4}]}""")
        sj.fromJson(js) shouldEqual (inst)
      }
      it("Set of class must work") {
        val inst = JSetHolder[Person](HashSet(Arrays.asList(Person("Bob", 35), Person("Sally", 54))))
        val sj = sjCodecOf[JSetHolder[Person]]
        val js = sj.toJson(inst)
        js should matchJson("""{"a":[{"name":"Bob","age",35},{"name":"Sally","age",54}]}""")
        sj.fromJson(js) shouldEqual (inst)
      }
    }

    describe(colorString("+++ Basic functions (ArrayList) +++")) {
      it("ArrayList is null must work") {
        val inst = ArrayListHolder[Int](null)
        val sj = sjCodecOf[ArrayListHolder[Int]]
        val js = sj.toJson(inst)
        js should matchJson("""{"a":null}""")
        sj.fromJson(js) shouldEqual (inst)
      }
      it("ArrayList of numeric must work") {
        val inst = ArrayListHolder[Int](ArrayList[Int](Arrays.asList(1, 2, 3)))
        val sj = sjCodecOf[ArrayListHolder[Int]]
        val js = sj.toJson(inst)
        js should matchJson("""{"a":[1,2,3]}""")
        sj.fromJson(js) shouldEqual (inst)
      }
      it("ArrayList of string must work") {
        val inst = ArrayListHolder[String](ArrayList[String](Arrays.asList("a", "b", "c")))
        val sj = sjCodecOf[ArrayListHolder[String]]
        val js = sj.toJson(inst)
        js should matchJson("""{"a":["a","b","c"]}""")
        sj.fromJson(js) shouldEqual (inst)
      }
      it("ArrayList of boolean must work") {
        val inst = ArrayListHolder[Boolean](ArrayList[Boolean](Arrays.asList(true, false, true)))
        val sj = sjCodecOf[ArrayListHolder[Boolean]]
        val js = sj.toJson(inst)
        js should matchJson("""{"a":[true,false,true]}""")
        sj.fromJson(js) shouldEqual (inst)
      }
      it("ArrayList of ArrayList (nested) must work") {
        val inst = ArrayListHolder[ArrayList[Int]](ArrayList[ArrayList[Int]](Arrays.asList(ArrayList(Arrays.asList(1, 2)), ArrayList(Arrays.asList(3, 4)))))
        val sj = sjCodecOf[ArrayListHolder[ArrayList[Int]]]
        val js = sj.toJson(inst)
        js should matchJson("""{"a":[[1,2],[3,4]]}""")
        sj.fromJson(js) shouldEqual (inst)
      }
      it("ArrayList of either must work") {
        val inst = ArrayListHolder[Either[Int, Boolean]](ArrayList[Either[Int, Boolean]](Arrays.asList(Right(true), Left(15), Right(false))))
        val sj = sjCodecOf[ArrayListHolder[Either[Int, Boolean]]](SJConfig.withEitherLeftHandling(EitherLeftPolicy.AS_VALUE))
        val js = sj.toJson(inst)
        js should matchJson("""{"a":[true,15,false]}""")
        sj.fromJson(js) shouldEqual (inst)
      }
      it("ArrayList of union must work") {
        val inst = ArrayListHolder[Int | Boolean](ArrayList[Int | Boolean](Arrays.asList(true, 15, false)))
        val sj = sjCodecOf[ArrayListHolder[Int | Boolean]]
        val js = sj.toJson(inst)
        js should matchJson("""{"a":[true,15,false]}""")
        sj.fromJson(js) shouldEqual (inst)
      }
      it("ArrayList of option must work") {
        val inst = ArrayListHolder[Option[Int]](ArrayList[Option[Int]](Arrays.asList(Some(1), None, Some(3))))
        val sj = sjCodecOf[ArrayListHolder[Option[Int]]]
        val js = sj.toJson(inst)
        js should matchJson("""{"a":[1,3]}""")
        sj.fromJson(js) shouldEqual (ArrayListHolder[Option[Int]](ArrayList[Option[Int]](Arrays.asList(Some(1), Some(3)))))
      }
      it("ArrayList of map must work") {
        val inst = ArrayListHolder[Map[String, Int]](ArrayList[Map[String, Int]](Arrays.asList(Map("a" -> 1, "b" -> 2), Map("c" -> 3, "d" -> 4))))
        val sj = sjCodecOf[ArrayListHolder[Map[String, Int]]]
        val js = sj.toJson(inst)
        js should matchJson("""{"a":[{"a":1,"b":2},{"c":3,"d":4}]}""")
        sj.fromJson(js) shouldEqual (inst)
      }
      it("ArrayList of class must work") {
        val inst = ArrayListHolder[Person](ArrayList[Person](Arrays.asList(Person("Bob", 35), Person("Sally", 54))))
        val sj = sjCodecOf[ArrayListHolder[Person]]
        val js = sj.toJson(inst)
        js should matchJson("""{"a":[{"name":"Bob","age",35},{"name":"Sally","age",54}]}""")
        sj.fromJson(js) shouldEqual (inst)
      }
    }

    describe(colorString("+++ Coersions (special cases, traits, etc) +++")) {
      it("ArrayBlockingQueue") {
        val q = new java.util.concurrent.ArrayBlockingQueue[Int](2, true, Arrays.asList(1, 2))
        val inst = Holder[java.util.concurrent.ArrayBlockingQueue[Int]](q)
        val sj = sjCodecOf[Holder[java.util.concurrent.ArrayBlockingQueue[Int]]]
        val js = sj.toJson(inst)
        js should matchJson("""{"a":[1,2]}""")
        val x = sj.fromJson(js)
        x.a.getClass.getName shouldEqual (inst.a.getClass.getName)
        x.a should contain(1)
        x.a should contain(2)
        x.a.size shouldEqual (2)
      }
      it("TreeSet") {
        val ts = new java.util.TreeSet[Int](Arrays.asList(1, 2, 3))
        val inst = Holder(ts)
        val sj = sjCodecOf[Holder[java.util.TreeSet[Int]]]
        val js = sj.toJson(inst)
        js should matchJson("""{"a":[1,2,3]}""")
        sj.fromJson(js) shouldEqual (inst)
      }
      it("Stack") {
        val ts = new java.util.Stack[String]()
        ts.push("x")
        ts.push("y")
        ts.push("z")
        val inst = Holder(ts)
        val sj = sjCodecOf[Holder[java.util.Stack[String]]]
        val js = sj.toJson(inst)
        js should matchJson("""{"a":["x","y","z"]}""")
        sj.fromJson(js) shouldEqual (inst)
      }
      it("List") {
        val ts: java.util.List[Int] = new java.util.ArrayList[Int](Arrays.asList(1, 2, 3))
        val inst = Holder(ts)
        val sj = sjCodecOf[Holder[java.util.List[Int]]]
        val js = sj.toJson(inst)
        js should matchJson("""{"a":[1,2,3]}""")
        sj.fromJson(js) shouldEqual (inst)
      }
      it("Iterable") {
        val ts: java.lang.Iterable[Int] = new java.util.ArrayList[Int](Arrays.asList(1, 2, 3))
        val inst = Holder(ts)
        val sj = sjCodecOf[Holder[java.lang.Iterable[Int]]]
        val js = sj.toJson(inst)
        js should matchJson("""{"a":[1,2,3]}""")
        sj.fromJson(js) shouldEqual (inst)
      }
      it("Queue") {
        val ts: java.util.Queue[Int] = new java.util.LinkedList[Int](Arrays.asList(1, 2, 3))
        val inst = Holder(ts)
        val sj = sjCodecOf[Holder[java.util.Queue[Int]]]
        val js = sj.toJson(inst)
        js should matchJson("""{"a":[1,2,3]}""")
        sj.fromJson(js) shouldEqual (inst)
      }
      it("Deque") {
        val ts: java.util.Deque[Int] = new java.util.LinkedList[Int](Arrays.asList(1, 2, 3))
        val inst = Holder(ts)
        val sj = sjCodecOf[Holder[java.util.Deque[Int]]]
        val js = sj.toJson(inst)
        js should matchJson("""{"a":[1,2,3]}""")
        sj.fromJson(js) shouldEqual (inst)
      }
      it("BlockingQueue") {
        val ts: java.util.concurrent.BlockingQueue[Int] = new java.util.concurrent.LinkedBlockingQueue[Int](Arrays.asList(1, 2, 3))
        val inst = Holder(ts)
        val sj = sjCodecOf[Holder[java.util.concurrent.BlockingQueue[Int]]]
        val js = sj.toJson(inst)
        js should matchJson("""{"a":[1,2,3]}""")
        val x = sj.fromJson(js)
        x.a.getClass.getName shouldEqual (inst.a.getClass.getName)
        x.a should contain(1)
        x.a should contain(2)
        x.a should contain(3)
        x.a.size shouldEqual (3)
      }
      it("TransferQueue") {
        val ts: java.util.concurrent.TransferQueue[Int] = new java.util.concurrent.LinkedTransferQueue[Int](Arrays.asList(1, 2, 3))
        val inst = Holder(ts)
        val sj = sjCodecOf[Holder[java.util.concurrent.TransferQueue[Int]]]
        val js = sj.toJson(inst)
        js should matchJson("""{"a":[1,2,3]}""")
        val x = sj.fromJson(js)
        x.a.getClass.getName shouldEqual (inst.a.getClass.getName)
        x.a should contain(1)
        x.a should contain(2)
        x.a should contain(3)
        x.a.size shouldEqual (3)
      }
      it("Vector (generic Collection example)") {
        val ts = new java.util.Vector[Int](Arrays.asList(1, 2, 3))
        val inst = Holder(ts)
        val sj = sjCodecOf[Holder[java.util.Vector[Int]]]
        val js = sj.toJson(inst)
        js should matchJson("""{"a":[1,2,3]}""")
        sj.fromJson(js) shouldEqual (inst)
      }
    }
  }
