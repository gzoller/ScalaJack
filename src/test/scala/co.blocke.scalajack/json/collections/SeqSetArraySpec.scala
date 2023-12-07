package co.blocke.scalajack
package json
package collections

import ScalaJack.*
import co.blocke.scala_reflection.*
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers.*
import org.scalatest.*
import TestUtil.*

import scala.collection.immutable.HashSet

class SeqSetArraySpec() extends AnyFunSpec with JsonMatchers:

  describe(colorString("-------------------------------\n:  Seq, Set, and Array Tests  :\n-------------------------------", Console.YELLOW)) {
    describe(colorString("+++ Positive Tests +++")) {
      it("Seq is null must work") {
        val inst = SeqHolder[Int](null)
        val js = sj[SeqHolder[Int]].toJson(inst)
        js should matchJson("""{"a":null}""")
      }
      it("Seq of numeric must work") {
        val inst = SeqHolder[Int](List(1, 2, 3))
        val js = sj[SeqHolder[Int]].toJson(inst)
        js should matchJson("""{"a":[1,2,3]}""")
      }
      it("Seq of string must work") {
        val inst = SeqHolder[String](List("a", "b", "c"))
        val js = sj[SeqHolder[String]].toJson(inst)
        js should matchJson("""{"a":["a","b","c"]}""")
      }
      it("Seq of boolean must work") {
        val inst = SeqHolder[Boolean](List(true, false, true))
        val js = sj[SeqHolder[Boolean]].toJson(inst)
        js should matchJson("""{"a":[true,false,true]}""")
      }
      it("Seq of seq (nested) must work") {
        val inst = SeqHolder[List[Int]](List(List(1, 2), List(3, 4)))
        val js = sj[SeqHolder[List[Int]]].toJson(inst)
        js should matchJson("""{"a":[[1,2],[3,4]]}""")
      }
      it("Seq of either must work") {
        val inst = SeqHolder[Either[Int, Boolean]](List(Right(true), Left(15), Right(false)))
        val js = sj[SeqHolder[Either[Int, Boolean]]](JsonConfig.withEitherLeftHandling(EitherLeftPolicy.AS_VALUE)).toJson(inst)
        js should matchJson("""{"a":[true,15,false]}""")
      }
      it("Seq of union must work") {
        val inst = SeqHolder[Int | Boolean](List(true, 15, false))
        val js = sj[SeqHolder[Int | Boolean]].toJson(inst)
        js should matchJson("""{"a":[true,15,false]}""")
      }
      it("Seq of option must work") {
        val inst = SeqHolder[Option[Int]](List(Some(1), None, Some(3)))
        val js = sj[SeqHolder[Option[Int]]].toJson(inst)
        js should matchJson("""{"a":[1,3]}""")
      }
      it("Seq of map must work") {
        val inst = SeqHolder[Map[String, Int]](List(Map("a" -> 1, "b" -> 2), Map("c" -> 3, "d" -> 4)))
        val js = sj[SeqHolder[Map[String, Int]]].toJson(inst)
        js should matchJson("""{"a":[{"a":1,"b":2},{"c":3,"d":4}]}""")
      }
      it("Seq of class must work") {
        val inst = SeqHolder[Person](List(Person("Bob", 35), Person("Sally", 54)))
        val js = sj[SeqHolder[Person]].toJson(inst)
        js should matchJson("""{"a":[{"name":"Bob","age",35},{"name":"Sally","age",54}]}""")
      }

      it("Set is null must work") {
        val inst = SetHolder[Int](null)
        val js = sj[SetHolder[Int]].toJson(inst)
        js should matchJson("""{"a":null}""")
      }
      it("Set of numeric must work") {
        val inst = SetHolder[Int](HashSet(1, 2, 3))
        val js = sj[SetHolder[Int]].toJson(inst)
        js should matchJson("""{"a":[1,2,3]}""")
      }
      it("Set of string must work") {
        val inst = SetHolder[String](HashSet("a", "b", "c"))
        val js = sj[SetHolder[String]].toJson(inst)
        js should matchJson("""{"a":["a","b","c"]}""")
      }
      it("Set of boolean must work") {
        val inst = SetHolder[Boolean](HashSet(true, false, true))
        val js = sj[SetHolder[Boolean]].toJson(inst)
        js should matchJson("""{"a":[false,true]}""")
      }
      it("Set of Set (nested) must work") {
        val inst = SetHolder[HashSet[Int]](HashSet(HashSet(1, 2), HashSet(3, 4)))
        val js = sj[SetHolder[HashSet[Int]]].toJson(inst)
        js should matchJson("""{"a":[[3,4],[1,2]]}""")
      }
      it("Set of either must work") {
        val inst = SetHolder[Either[Int, Boolean]](HashSet(Right(true), Left(15), Right(false)))
        val js = sj[SetHolder[Either[Int, Boolean]]](JsonConfig.withEitherLeftHandling(EitherLeftPolicy.AS_VALUE)).toJson(inst)
        js should matchJson("""{"a":[15,true,false]}""")
      }
      it("Set of union must work") {
        val inst = SetHolder[Int | Boolean](HashSet(true, 15, false))
        val js = sj[SetHolder[Int | Boolean]].toJson(inst)
        js should matchJson("""{"a":[false,true,15]}""")
      }
      it("Set of option must work") {
        val inst = SetHolder[Option[Int]](HashSet(Some(1), None, Some(3)))
        val js = sj[SetHolder[Option[Int]]].toJson(inst)
        js should matchJson("""{"a":[3,1]}""")
      }
      it("Set of map must work") {
        val inst = SetHolder[Map[String, Int]](HashSet(Map("a" -> 1, "b" -> 2), Map("c" -> 3, "d" -> 4)))
        val js = sj[SetHolder[Map[String, Int]]].toJson(inst)
        js should matchJson("""{"a":[{"c":3,"d":4},{"a":1,"b":2}]}""")
      }
      it("Set of class must work") {
        val inst = SetHolder[Person](HashSet(Person("Bob", 35), Person("Sally", 54)))
        val js = sj[SetHolder[Person]].toJson(inst)
        js should matchJson("""{"a":[{"name":"Bob","age",35},{"name":"Sally","age",54}]}""")
      }

      it("Array is null must work") {
        val inst = ArrayHolder[Int](null)
        val js = sj[ArrayHolder[Int]].toJson(inst)
        js should matchJson("""{"a":null}""")
      }
      it("Array of numeric must work") {
        val inst = ArrayHolder[Int](Array(1, 2, 3))
        val js = sj[ArrayHolder[Int]].toJson(inst)
        js should matchJson("""{"a":[1,2,3]}""")
      }
      it("Array of string must work") {
        val inst = ArrayHolder[String](Array("a", "b", "c"))
        val js = sj[ArrayHolder[String]].toJson(inst)
        js should matchJson("""{"a":["a","b","c"]}""")
      }
      it("Array of boolean must work") {
        val inst = ArrayHolder[Boolean](Array(true, false, true))
        val js = sj[ArrayHolder[Boolean]].toJson(inst)
        js should matchJson("""{"a":[true,false,true]}""")
      }
      it("Array of Array (nested) must work") {
        val inst = ArrayHolder[Array[Int]](Array(Array(1, 2), Array(3, 4)))
        val js = sj[ArrayHolder[Array[Int]]].toJson(inst)
        js should matchJson("""{"a":[[1,2],[3,4]]}""")
      }
      it("Array of either must work") {
        val inst = ArrayHolder[Either[Int, Boolean]](Array(Right(true), Left(15), Right(false)))
        val js = sj[ArrayHolder[Either[Int, Boolean]]](JsonConfig.withEitherLeftHandling(EitherLeftPolicy.AS_VALUE)).toJson(inst)
        js should matchJson("""{"a":[true,15,false]}""")
      }
      it("Array of union must work") {
        val inst = ArrayHolder[Int | Boolean](Array(true, 15, false))
        val js = sj[ArrayHolder[Int | Boolean]].toJson(inst)
        js should matchJson("""{"a":[true,15,false]}""")
      }
      it("Array of option must work") {
        val inst = ArrayHolder[Option[Int]](Array(Some(1), None, Some(3)))
        val js = sj[ArrayHolder[Option[Int]]].toJson(inst)
        js should matchJson("""{"a":[1,3]}""")
      }
      it("Array of map must work") {
        val inst = ArrayHolder[Map[String, Int]](Array(Map("a" -> 1, "b" -> 2), Map("c" -> 3, "d" -> 4)))
        val js = sj[ArrayHolder[Map[String, Int]]].toJson(inst)
        js should matchJson("""{"a":[{"a":1,"b":2},{"c":3,"d":4}]}""")
      }
      it("Array of class must work") {
        val inst = ArrayHolder[Person](Array(Person("Bob", 35), Person("Sally", 54)))
        val js = sj[ArrayHolder[Person]].toJson(inst)
        js should matchJson("""{"a":[{"name":"Bob","age",35},{"name":"Sally","age",54}]}""")
      }
    }
  }
