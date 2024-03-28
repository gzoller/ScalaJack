package co.blocke.scalajack
package json
package misc

import ScalaJack.*
import co.blocke.scala_reflection.*
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers.*
import org.scalatest.*
import scala.util.*
import TestUtil.*

import java.util.UUID

class OptionTrySpec() extends AnyFunSpec with JsonMatchers:

  describe(colorString("-------------------------------\n:         Option Tests        :\n-------------------------------", Console.YELLOW)) {
    /*
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
      val js = sjCodecOf[OptionHolder[Int]].toJson(inst)
      js should matchJson("""{"a":5,"b":[1,"ok"],"c":[1,3],"d":{"1":2,"3":4},"e":99,"f":100,"g":0,"h":{"name":"BoB","age":34},"i":15}""")
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
      val js = sjCodecOf[OptionHolder[Int]].toJson(inst)
      js should matchJson("""{"b":[null,"ok"],"c":[],"d":{}}""")
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
      val js = sj[OptionHolder[Int]](
        JsonConfig.withNoneAsNull().withEitherLeftHandling(EitherLeftPolicy.AS_VALUE)
      ).toJson(inst)
      js should matchJson("""{"a":null,"b":[null,"ok"],"c":[null,null,null],"d":{"1":null,"3":null},"e":null,"f":null,"g":null,"h":null,"i":null,"j":null}""")
    }
    it("Java Optional must work") {
      ???
    }
      */
  }
