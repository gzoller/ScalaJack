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
        val sj = sjCodecOf[SeqHolder[Int]]
        val js = sj.toJson(inst)
        js should matchJson("""{"a":null}""")
        sj.fromJson(js) shouldEqual (inst)
      }
      it("Seq of numeric must work") {
        val inst = SeqHolder[Int](List(1, 2, 3))
        val sj = sjCodecOf[SeqHolder[Int]]
        val js = sj.toJson(inst)
        js should matchJson("""{"a":[1,2,3]}""")
        sj.fromJson(js) shouldEqual (inst)
      }
      it("Seq of string must work") {
        val inst = SeqHolder[String](List("a", "b", "c"))
        val sj = sjCodecOf[SeqHolder[String]]
        val js = sj.toJson(inst)
        js should matchJson("""{"a":["a","b","c"]}""")
        sj.fromJson(js) shouldEqual (inst)
      }
      it("Mutable Seq of string must work") {
        val inst = MSeqHolder[String](scala.collection.mutable.ListBuffer("a", "b", "c"))
        val sj = sjCodecOf[MSeqHolder[String]]
        val js = sj.toJson(inst)
        js should matchJson("""{"a":["a","b","c"]}""")
        sj.fromJson(js) shouldEqual (inst)
      }
      it("Seq of boolean must work") {
        val inst = SeqHolder[Boolean](List(true, false, true))
        val sj = sjCodecOf[SeqHolder[Boolean]]
        val js = sj.toJson(inst)
        js should matchJson("""{"a":[true,false,true]}""")
        sj.fromJson(js) shouldEqual (inst)
      }
      it("Seq of seq (nested) must work") {
        val inst = SeqHolder[List[Int]](List(List(1, 2), List(3, 4)))
        val sj = sjCodecOf[SeqHolder[List[Int]]]
        val js = sj.toJson(inst)
        js should matchJson("""{"a":[[1,2],[3,4]]}""")
        sj.fromJson(js) shouldEqual (inst)
      }
      it("Seq of either must work") {
        val inst = SeqHolder[Either[Int, Boolean]](List(Right(true), Left(15), Right(false)))
        val sj = sjCodecOf[SeqHolder[Either[Int, Boolean]]](SJConfig.withEitherLeftHandling(EitherLeftPolicy.AS_VALUE))
        val js = sj.toJson(inst)
        js should matchJson("""{"a":[true,15,false]}""")
        sj.fromJson(js) shouldEqual (inst)
      }
      it("Seq of union must work") {
        val inst = SeqHolder[Int | Boolean](List(true, 15, false))
        val sj = sjCodecOf[SeqHolder[Int | Boolean]]
        val js = sj.toJson(inst)
        js should matchJson("""{"a":[true,15,false]}""")
        sj.fromJson(js) shouldEqual (inst)
      }
      it("Seq of option must work") {
        val inst = SeqHolder[Option[Int]](List(Some(1), None, Some(3)))
        val sj = sjCodecOf[SeqHolder[Option[Int]]]
        val js = sj.toJson(inst)
        js should matchJson("""{"a":[1,3]}""")
        sj.fromJson(js) shouldEqual (SeqHolder[Option[Int]](List(Some(1), Some(3))))
      }
      it("Seq of map must work") {
        val inst = SeqHolder[Map[String, Int]](List(Map("a" -> 1, "b" -> 2), Map("c" -> 3, "d" -> 4)))
        val sj = sjCodecOf[SeqHolder[Map[String, Int]]]
        val js = sj.toJson(inst)
        js should matchJson("""{"a":[{"a":1,"b":2},{"c":3,"d":4}]}""")
        sj.fromJson(js) shouldEqual (inst)
      }
      it("Seq of class must work") {
        val inst = SeqHolder[Person](List(Person("Bob", 35), Person("Sally", 54)))
        val sj = sjCodecOf[SeqHolder[Person]]
        val js = sj.toJson(inst)
        js should matchJson("""{"a":[{"name":"Bob","age",35},{"name":"Sally","age",54}]}""")
        sj.fromJson(js) shouldEqual (inst)
      }

      it("Set is null must work") {
        val inst = SetHolder[Int](null)
        val sj = sjCodecOf[SetHolder[Int]]
        val js = sj.toJson(inst)
        js should matchJson("""{"a":null}""")
        sj.fromJson(js) shouldEqual (inst)
      }
      it("Set of numeric must work") {
        val inst = SetHolder[Int](HashSet(1, 2, 3))
        val sj = sjCodecOf[SetHolder[Int]]
        val js = sj.toJson(inst)
        js should matchJson("""{"a":[1,2,3]}""")
        sj.fromJson(js) shouldEqual (inst)
      }
      it("Set of string must work") {
        val inst = SetHolder[String](HashSet("a", "b", "c"))
        val sj = sjCodecOf[SetHolder[String]]
        val js = sj.toJson(inst)
        js should matchJson("""{"a":["a","b","c"]}""")
        sj.fromJson(js) shouldEqual (inst)
      }
      it("Mutable Set of string must work") {
        val inst = MSetHolder[String](scala.collection.mutable.HashSet("a", "b", "c"))
        val sj = sjCodecOf[MSetHolder[String]]
        val js = sj.toJson(inst)
        js should matchJson("""{"a":["a","b","c"]}""")
        sj.fromJson(js) shouldEqual (inst)
      }
      it("Set of boolean must work") {
        val inst = SetHolder[Boolean](HashSet(true, false, true))
        val sj = sjCodecOf[SetHolder[Boolean]]
        val js = sj.toJson(inst)
        js should matchJson("""{"a":[false,true]}""")
        sj.fromJson(js) shouldEqual (inst)
      }
      it("Set of Set (nested) must work") {
        val inst = SetHolder[HashSet[Int]](HashSet(HashSet(1, 2), HashSet(3, 4)))
        val sj = sjCodecOf[SetHolder[HashSet[Int]]]
        val js = sj.toJson(inst)
        js should matchJson("""{"a":[[3,4],[1,2]]}""")
        sj.fromJson(js) shouldEqual (inst)
      }
      it("Set of either must work") {
        val inst = SetHolder[Either[Int, Boolean]](HashSet(Right(true), Left(15), Right(false)))
        val sj = sjCodecOf[SetHolder[Either[Int, Boolean]]](SJConfig.withEitherLeftHandling(EitherLeftPolicy.AS_VALUE))
        val js = sj.toJson(inst)
        js should matchJson("""{"a":[15,true,false]}""")
        sj.fromJson(js) shouldEqual (inst)
      }
      it("Set of union must work") {
        val inst = SetHolder[Int | Boolean](HashSet(true, 15, false))
        val sj = sjCodecOf[SetHolder[Int | Boolean]]
        val js = sj.toJson(inst)
        js should matchJson("""{"a":[false,true,15]}""")
        sj.fromJson(js) shouldEqual (inst)
      }
      it("Set of option must work") {
        val inst = SetHolder[Option[Int]](HashSet(Some(1), None, Some(3)))
        val sj = sjCodecOf[SetHolder[Option[Int]]]
        val js = sj.toJson(inst)
        js should matchJson("""{"a":[3,1]}""")
        sj.fromJson(js) shouldEqual (SetHolder[Option[Int]](HashSet(Some(1), Some(3))))
      }
      it("Set of map must work") {
        val inst = SetHolder[Map[String, Int]](HashSet(Map("a" -> 1, "b" -> 2), Map("c" -> 3, "d" -> 4)))
        val sj = sjCodecOf[SetHolder[Map[String, Int]]]
        val js = sj.toJson(inst)
        js should matchJson("""{"a":[{"c":3,"d":4},{"a":1,"b":2}]}""")
        sj.fromJson(js) shouldEqual (inst)
      }
      it("Set of class must work") {
        val inst = SetHolder[Person](HashSet(Person("Bob", 35), Person("Sally", 54)))
        val sj = sjCodecOf[SetHolder[Person]]
        val js = sj.toJson(inst)
        js should matchJson("""{"a":[{"name":"Bob","age",35},{"name":"Sally","age",54}]}""")
        sj.fromJson(js) shouldEqual (inst)
      }

      it("Array is null must work") {
        val inst = ArrayHolder[Int](null)
        val sj = sjCodecOf[ArrayHolder[Int]]
        val js = sj.toJson(inst)
        js should matchJson("""{"a":null}""")
        sj.fromJson(js) shouldEqual (inst)
      }
      it("Array of numeric must work") {
        val inst = ArrayHolder[Int](Array(1, 2, 3))
        val sj = sjCodecOf[ArrayHolder[Int]]
        val js = sj.toJson(inst)
        js should matchJson("""{"a":[1,2,3]}""")
        sj.fromJson(js).a.toList shouldEqual (inst.a.toList)
      }
      it("Array of string must work") {
        val inst = ArrayHolder[String](Array("a", "b", "c"))
        val sj = sjCodecOf[ArrayHolder[String]]
        val js = sj.toJson(inst)
        js should matchJson("""{"a":["a","b","c"]}""")
        sj.fromJson(js).a.toList shouldEqual (inst.a.toList)
      }
      it("Array of boolean must work") {
        val inst = ArrayHolder[Boolean](Array(true, false, true))
        val sj = sjCodecOf[ArrayHolder[Boolean]]
        val js = sj.toJson(inst)
        js should matchJson("""{"a":[true,false,true]}""")
        sj.fromJson(js).a.toList shouldEqual (inst.a.toList)
      }
      it("Array of Array (nested) must work") {
        val inst = ArrayHolder[Array[Int]](Array(Array(1, 2), Array(3, 4)))
        val sj = sjCodecOf[ArrayHolder[Array[Int]]]
        val js = sj.toJson(inst)
        js should matchJson("""{"a":[[1,2],[3,4]]}""")
        sj.fromJson(js).a.map(_.toList).toList shouldEqual (inst.a.map(_.toList).toList)
      }
      it("Array of either must work") {
        val inst = ArrayHolder[Either[Int, Boolean]](Array(Right(true), Left(15), Right(false)))
        val sj = sjCodecOf[ArrayHolder[Either[Int, Boolean]]](SJConfig.withEitherLeftHandling(EitherLeftPolicy.AS_VALUE))
        val js = sj.toJson(inst)
        js should matchJson("""{"a":[true,15,false]}""")
        sj.fromJson(js).a.toList shouldEqual (inst.a.toList)
      }
      it("Array of union must work") {
        val inst = ArrayHolder[Int | Boolean](Array(true, 15, false))
        val sj = sjCodecOf[ArrayHolder[Int | Boolean]]
        val js = sj.toJson(inst)
        js should matchJson("""{"a":[true,15,false]}""")
        sj.fromJson(js).a.toList shouldEqual (inst.a.toList)
      }
      it("Array of option must work") {
        val inst = ArrayHolder[Option[Int]](Array(Some(1), None, Some(3)))
        val sj = sjCodecOf[ArrayHolder[Option[Int]]]
        val js = sj.toJson(inst)
        js should matchJson("""{"a":[1,3]}""")
        sj.fromJson(js).a.toList shouldEqual (ArrayHolder[Option[Int]](Array(Some(1), Some(3))).a.toList)
      }
      it("Array of map must work") {
        val inst = ArrayHolder[Map[String, Int]](Array(Map("a" -> 1, "b" -> 2), Map("c" -> 3, "d" -> 4)))
        val sj = sjCodecOf[ArrayHolder[Map[String, Int]]]
        val js = sj.toJson(inst)
        js should matchJson("""{"a":[{"a":1,"b":2},{"c":3,"d":4}]}""")
        sj.fromJson(js).a.toList shouldEqual (inst.a.toList)
      }
      it("Array of class must work") {
        val inst = ArrayHolder[Person](Array(Person("Bob", 35), Person("Sally", 54)))
        val sj = sjCodecOf[ArrayHolder[Person]]
        val js = sj.toJson(inst)
        js should matchJson("""{"a":[{"name":"Bob","age",35},{"name":"Sally","age",54}]}""")
        sj.fromJson(js).a.toList shouldEqual (inst.a.toList)
      }

      it("Vector of class must work") {
        val inst = VectorHolder[Person](Vector(Person("Bob", 35), Person("Sally", 54)))
        val sj = sjCodecOf[VectorHolder[Person]]
        val js = sj.toJson(inst)
        js should matchJson("""{"a":[{"name":"Bob","age",35},{"name":"Sally","age",54}]}""")
        sj.fromJson(js).a.toList shouldEqual (inst.a.toList)
      }
      it("IndexedSeq of class must work") {
        val inst = IndexedSeqHolder[Person](IndexedSeq(Person("Bob", 35), Person("Sally", 54)))
        val sj = sjCodecOf[IndexedSeqHolder[Person]]
        val js = sj.toJson(inst)
        js should matchJson("""{"a":[{"name":"Bob","age",35},{"name":"Sally","age",54}]}""")
        sj.fromJson(js).a.toList shouldEqual (inst.a.toList)
      }
      it("Iterable of class must work") {
        val inst = IterableHolder[Person](Seq(Person("Bob", 35), Person("Sally", 54)))
        val sj = sjCodecOf[IterableHolder[Person]]
        val js = sj.toJson(inst)
        js should matchJson("""{"a":[{"name":"Bob","age",35},{"name":"Sally","age",54}]}""")
        sj.fromJson(js).a.toList shouldEqual (inst.a.toList)
      }

      it("List[T] must serialize and deserialize using the auto-generated list codec") {
        val people = List(Person("Bob", 35), Person("Sally", 54))
        val sj = sjCodecOf[Person]
        val js = sj.toJsonList(people)
        js should matchJson(
          """[
            |  {"name":"Bob","age":35},
            |  {"name":"Sally","age":54}
            |]""".stripMargin
        )
        val roundTrip = sj.fromJsonList(js)
        roundTrip shouldEqual people
      }

    }
  }
