package co.blocke.scalajack
package json
package misc

import ScalaJack.*
import co.blocke.scala_reflection.*
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers.*
import org.scalatest.*
import scala.util.*
import java.util.Optional
import TestUtil.*

import java.util.UUID

class OptionSpec() extends AnyFunSpec with JsonMatchers:

  describe(colorString("-------------------------------\n:         Option Tests        :\n-------------------------------", Console.YELLOW)) {
    describe(colorString("+++ Scala Option +++")) {
      it("Non-empty Options must work") {
        val inst = OptionHolder[Int](
          Some(5), // straight Option
          (Some(1), "ok"), // tuple w/Option
          List(Some(1), None, Some(3)), // Seq of Option
          Map(1 -> Some(2), 3 -> Some(4)), // Map of Option
          Some(99), // Union of Option (R)
          Some(100), // Union of Option (L)
          Some(Some(0)), // Nested Option
          Some(Person("BoB", 34)), // Option of class
          Right(Some(15)), // Either of Option (R)
          Left(Some(-3)) // Either of Option (L)
        )
        val sj = sjCodecOf[OptionHolder[Int]]
        val js = sj.toJson(inst)
        js should matchJson("""{"a":5,"b":[1,"ok"],"c":[1,3],"d":{"1":2,"3":4},"e":99,"f":100,"g":0,"h":{"name":"BoB","age":34},"i":15,"j":-3}""")
        // Some fields get changed/"promoted" when read back in per policy, as Either reads "best-fit" starting with Right value first
        sj.fromJson(js) shouldEqual (inst.copy(c = List(Some(1), Some(3))).copy(e = 99).copy(j = Right(-3)))
      }
      it("Empty Options must work (default)") {
        val inst = OptionHolder[Int](
          None, // straight Option
          (None, "ok"), // tuple w/Option
          List(None, None, None), // Seq of Option
          Map(1 -> None, 3 -> None), // Map of Option
          None, // Union of Option (R)
          None, // Union of Option (L)
          Some(None), // Nested Option
          None, // Option of class
          Right(None), // Either of Option (R)
          Left(None) // Either of Option (L)
        )
        val sj = sjCodecOf[OptionHolder[Int]]
        val js = sj.toJson(inst)
        js should matchJson("""{"b":[null,"ok"],"c":[],"d":{}}""")
        sj.fromJson(js) shouldEqual (inst.copy(c = List(), d = Map(), g = None))
      }
      it("Empty Options must work (config noneAsNull = true)") {
        val inst = OptionHolder[Int](
          None, // straight Option
          (None, "ok"), // tuple w/Option
          List(None, None, None), // Seq of Option
          Map(1 -> None, 3 -> None), // Map of Option
          None, // Union of Option (R)
          None, // Union of Option (L)
          Some(None), // Nested Option
          None, // Option of class
          Right(None), // Either of Option (R)
          Left(None) // Either of Option (L)
        )
        val sj = sjCodecOf[OptionHolder[Int]](
          JsonConfig.withNoneAsNull()
        )
        val js = sj.toJson(inst)
        js should matchJson("""{"a":null,"b":[null,"ok"],"c":[null,null,null],"d":{"1":null,"3":null},"e":null,"f":null,"g":null,"h":null,"i":null,"j":null}""")
        sj.fromJson(js) shouldEqual (inst.copy(g = None, i = null, j = null))
      }
      it("Either recipe should work (non-None)") {
        val inst = EitherRecipe[Int](Right(Left(Some(5))))
        val sj = sjCodecOf[EitherRecipe[Int]]
        val js = sj.toJson(inst)
        js should matchJson("""{"a":5}""")
        sj.fromJson(js) shouldEqual (inst)
      }
      it("Either recipe should work (None)") {
        val inst = EitherRecipe[Int](Right(Left(None)))
        val sj = sjCodecOf[EitherRecipe[Int]]
        val js = sj.toJson(inst)
        js should matchJson("""{}""")
        sj.fromJson(js) shouldEqual (inst)
      }
      it("Either recipe should work (None as null)") {
        val inst = EitherRecipe[Int](Right(Left(None)))
        val sj = sjCodecOf[EitherRecipe[Int]](JsonConfig.withNoneAsNull())
        val js = sj.toJson(inst)
        js should matchJson("""{"a":null}""")
        sj.fromJson(js) shouldEqual (EitherRecipe[Int](null))
      }
    }

    describe(colorString("+++ Java Optional +++")) {
      it("Non-empty Options must work") {
        val inst = OptionalHolder[Int](
          Optional.of(5), // straight Optional
          (Optional.of(1), "ok"), // tuple w/Optional
          List(Optional.of(1), Optional.empty, Optional.of(3)), // Seq of Optional
          Map(1 -> Optional.of(2), 3 -> Optional.of(4)), // Map of Optional
          Optional.of(99), // Union of Optional (R)
          Optional.of(100), // Union of Optional (L)
          Optional.of(Optional.of(0)), // Nested Optional
          Optional.of(Person("BoB", 34)), // Optional of class
          Right(Optional.of(15)), // Either of Optional (R)
          Left(Optional.of(-3)) // Either of Optional (L)
        )
        val sj = sjCodecOf[OptionalHolder[Int]]
        val js = sj.toJson(inst)
        js should matchJson("""{"a":5,"b":[1,"ok"],"c":[1,3],"d":{"1":2,"3":4},"e":99,"f":100,"g":0,"h":{"name":"BoB","age":34},"i":15,"j":-3}""")
        // Some fields get changed/"promoted" when read back in per policy, as Either reads "best-fit" starting with Right value first
        sj.fromJson(js) shouldEqual (inst.copy(c = List(Optional.of(1), Optional.of(3))).copy(e = 99).copy(j = Right(-3)))
      }
      it("Empty Options must work (default)") {
        val inst = OptionalHolder[Int](
          Optional.empty, // straight Option
          (Optional.empty, "ok"), // tuple w/Option
          List(Optional.empty, Optional.empty, Optional.empty), // Seq of Option
          Map(1 -> Optional.empty, 3 -> Optional.empty), // Map of Option
          Optional.empty, // Union of Option (R)
          Optional.empty, // Union of Option (L)
          Optional.of(Optional.empty), // Nested Option
          Optional.empty, // Option of class
          Right(Optional.empty), // Either of Option (R)
          Left(Optional.empty) // Either of Option (L)
        )
        val sj = sjCodecOf[OptionalHolder[Int]]
        val js = sj.toJson(inst)
        js should matchJson("""{"b":[null,"ok"],"c":[],"d":{}}""")
        sj.fromJson(js) shouldEqual (inst.copy(c = List(), d = Map(), g = Optional.empty))
      }
      it("Empty Options must work (config noneAsNull = true)") {
        val inst = OptionalHolder[Int](
          Optional.empty, // straight Option
          (Optional.empty, "ok"), // tuple w/Option
          List(Optional.empty, Optional.empty, Optional.empty), // Seq of Option
          Map(1 -> Optional.empty, 3 -> Optional.empty), // Map of Option
          Optional.empty, // Union of Option (R)
          Optional.empty, // Union of Option (L)
          Optional.of(Optional.empty), // Nested Option
          Optional.empty, // Option of class
          Right(Optional.empty), // Either of Option (R)
          Left(Optional.empty) // Either of Option (L)
        )
        val sj = sjCodecOf[OptionalHolder[Int]](
          JsonConfig.withNoneAsNull()
        )
        val js = sj.toJson(inst)
        js should matchJson("""{"a":null,"b":[null,"ok"],"c":[null,null,null],"d":{"1":null,"3":null},"e":null,"f":null,"g":null,"h":null,"i":null,"j":null}""")
        sj.fromJson(js) shouldEqual (inst.copy(g = Optional.empty, i = null, j = null))
      }
      it("Either recipe should work (non-None)") {
        val inst = EitherRecipeJ[Int](Right(Left(Optional.of(5))))
        val sj = sjCodecOf[EitherRecipeJ[Int]]
        val js = sj.toJson(inst)
        js should matchJson("""{"a":5}""")
        sj.fromJson(js) shouldEqual (inst)
      }
      it("Either recipe should work (None)") {
        val inst = EitherRecipeJ[Int](Right(Left(Optional.empty)))
        val sj = sjCodecOf[EitherRecipeJ[Int]]
        val js = sj.toJson(inst)
        js should matchJson("""{}""")
        sj.fromJson(js) shouldEqual (inst)
      }
      it("Either recipe should work (None as null)") {
        val inst = EitherRecipeJ[Int](Right(Left(Optional.empty)))
        val sj = sjCodecOf[EitherRecipeJ[Int]](JsonConfig.withNoneAsNull())
        val js = sj.toJson(inst)
        js should matchJson("""{"a":null}""")
        sj.fromJson(js) shouldEqual (EitherRecipeJ[Int](null))
      }
    }
  }
