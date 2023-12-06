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
    describe(colorString("+++ Positive Tests +++")) {
      it("Set is null must work") {
        val inst = JSetHolder[Int](null)
        val js = sj[JSetHolder[Int]].toJson(inst)
        js should matchJson("""{"a":null}""")
      }
      it("Set of numeric must work") {
        val inst = JSetHolder[Int](HashSet(Arrays.asList(1,2,3)))
        val js = sj[JSetHolder[Int]].toJson(inst)
        js should matchJson("""{"a":[1,2,3]}""")
      }
      it("Set of string must work") {
        val inst = JSetHolder[String](HashSet(Arrays.asList("a","b","c")))
        val js = sj[JSetHolder[String]].toJson(inst)
        js should matchJson("""{"a":["a","b","c"]}""")
      }
      it("Set of boolean must work") {
        val inst = JSetHolder[Boolean](HashSet(Arrays.asList(true,false,true)))
        val js = sj[JSetHolder[Boolean]].toJson(inst)
        js should matchJson("""{"a":[false,true]}""")
      }
      it("Set of Set (nested) must work") {
        val inst = JSetHolder[List[Int]](HashSet(Arrays.asList(List(1,2),List(3,4))))
        val js = sj[JSetHolder[List[Int]]].toJson(inst)
        js should matchJson("""{"a":[[3,4],[1,2]]}""")
      }
      it("Set of either must work") {
        val inst = JSetHolder[Either[Int,Boolean]](HashSet(Arrays.asList(Right(true),Left(15),Right(false))))
        val js = sj[JSetHolder[Either[Int,Boolean]]](JsonConfig.withEitherLeftHandling(EitherLeftPolicy.AS_VALUE)).toJson(inst)
        js should matchJson("""{"a":[true,15,false]}""")
      }
      it("Set of union must work") {
        val inst = JSetHolder[Int|Boolean](HashSet(Arrays.asList(true,15,false)))
        val js = sj[JSetHolder[Int|Boolean]].toJson(inst)
        js should matchJson("""{"a":[false,true,15]}""")
      }
      it("Set of option must work") {
        val inst = JSetHolder[Option[Int]](HashSet(Arrays.asList(Some(1),None,Some(3))))
        val js = sj[JSetHolder[Option[Int]]].toJson(inst)
        js should matchJson("""{"a":[1,3]}""")
      }
      it("Set of map must work") {
        val inst = JSetHolder[Map[String,Int]](HashSet(Arrays.asList(Map("a"->1,"b"->2),Map("c"->3,"d"->4))))
        val js = sj[JSetHolder[Map[String,Int]]].toJson(inst)
        js should matchJson("""{"a":[{"a":1,"b":2},{"c":3,"d":4}]}""")
      }
      it("Set of class must work") {
        val inst = JSetHolder[Person](HashSet(Arrays.asList(Person("Bob",35),Person("Sally",54))))
        val js = sj[JSetHolder[Person]].toJson(inst)
        js should matchJson("""{"a":[{"name":"Bob","age",35},{"name":"Sally","age",54}]}""")
      }

      it("ArrayList is null must work") {
        val inst = ArrayListHolder[Int](null)
        val js = sj[ArrayListHolder[Int]].toJson(inst)
        js should matchJson("""{"a":null}""")
      }
      it("ArrayList of numeric must work") {
        val inst = ArrayListHolder[Int](ArrayList[Int](Arrays.asList(1,2,3)))
        val js = sj[ArrayListHolder[Int]].toJson(inst)
        js should matchJson("""{"a":[1,2,3]}""")
      }
      it("ArrayList of string must work") {
        val inst = ArrayListHolder[String](ArrayList[String](Arrays.asList("a","b","c")))
        val js = sj[ArrayListHolder[String]].toJson(inst)
        js should matchJson("""{"a":["a","b","c"]}""")
      }
      it("ArrayList of boolean must work") {
        val inst = ArrayListHolder[Boolean](ArrayList[Boolean](Arrays.asList(true,false,true)))
        val js = sj[ArrayListHolder[Boolean]].toJson(inst)
        js should matchJson("""{"a":[true,false,true]}""")
      }
      it("ArrayList of ArrayList (nested) must work") {
        val inst = ArrayListHolder[ArrayList[Int]](ArrayList[ArrayList[Int]](Arrays.asList(ArrayList(Arrays.asList(1,2)),ArrayList(Arrays.asList(3,4)))))
        val js = sj[ArrayListHolder[ArrayList[Int]]].toJson(inst)
        js should matchJson("""{"a":[[1,2],[3,4]]}""")
      }
      it("ArrayList of either must work") {
        val inst = ArrayListHolder[Either[Int,Boolean]](ArrayList[Either[Int,Boolean]](Arrays.asList(Right(true),Left(15),Right(false))))
        val js = sj[ArrayListHolder[Either[Int,Boolean]]](JsonConfig.withEitherLeftHandling(EitherLeftPolicy.AS_VALUE)).toJson(inst)
        js should matchJson("""{"a":[true,15,false]}""")
      }
      it("ArrayList of union must work") {
        val inst = ArrayListHolder[Int|Boolean](ArrayList[Int|Boolean](Arrays.asList(true,15,false)))
        val js = sj[ArrayListHolder[Int|Boolean]].toJson(inst)
        js should matchJson("""{"a":[true,15,false]}""")
      }
      it("ArrayList of option must work") {
        val inst = ArrayListHolder[Option[Int]](ArrayList[Option[Int]](Arrays.asList(Some(1),None,Some(3))))
        val js = sj[ArrayListHolder[Option[Int]]].toJson(inst)
        js should matchJson("""{"a":[1,3]}""")
      }
      it("ArrayList of map must work") {
        val inst = ArrayListHolder[Map[String,Int]](ArrayList[Map[String,Int]](Arrays.asList(Map("a"->1,"b"->2),Map("c"->3,"d"->4))))
        val js = sj[ArrayListHolder[Map[String,Int]]].toJson(inst)
        js should matchJson("""{"a":[{"a":1,"b":2},{"c":3,"d":4}]}""")
      }
      it("ArrayList of class must work") {
        val inst = ArrayListHolder[Person](ArrayList[Person](Arrays.asList(Person("Bob",35),Person("Sally",54))))
        val js = sj[ArrayListHolder[Person]].toJson(inst)
        js should matchJson("""{"a":[{"name":"Bob","age",35},{"name":"Sally","age",54}]}""")
      }
    }
  }