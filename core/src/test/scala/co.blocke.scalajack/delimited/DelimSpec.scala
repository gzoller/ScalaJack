package co.blocke.scalajack
package delimited

import org.scalatest.PrivateMethodTester
import org.scalatest.matchers.should.Matchers
import org.scalatest.funspec.AnyFunSpec
import scala.reflect.runtime.universe._

class DelimSpec extends AnyFunSpec with Matchers with PrivateMethodTester {

  val sj = ScalaJack(DelimitedFlavor())

  describe(
    "---------------------\n:  Delimited Tests  :\n---------------------"
  ) {
      describe("DelimSpec") {
        it("Basic DelimSpec") {
          val all = AllPrim(
            5,
            25L,
            123.45,
            12.3F,
            'x',
            "Hey",
            b = true,
            BigInt(12345678),
            BigDecimal(0.123458867)
          )
          val delim = sj.render(all)
          delim should equal(
            """5,25,123.45,12.3,x,Hey,true,12345678,0.123458867"""
          )
          val inst = sj.read[AllPrim](delim)
          inst should equal(all)
        }
        it("SalaJack configurations (DelimitedFlavor)") {
          an[ScalaJackError] should be thrownBy ScalaJack(DelimitedFlavor())
            .withAdapters()
          an[ScalaJackError] should be thrownBy ScalaJack(DelimitedFlavor())
            .withDefaultHint(null)
          an[ScalaJackError] should be thrownBy ScalaJack(DelimitedFlavor())
            .withHintModifiers()
          an[ScalaJackError] should be thrownBy ScalaJack(DelimitedFlavor())
            .withHints()
          an[ScalaJackError] should be thrownBy ScalaJack(DelimitedFlavor())
            .withTypeValueModifier(null)
          an[ScalaJackError] should be thrownBy ScalaJack(DelimitedFlavor())
            .allowPermissivePrimitives()
          val sjx = ScalaJack(DelimitedFlavor())
            .parseOrElse(typeOf[ThreeInts] -> typeOf[DefaultThree])
          sjx.read[ThreeInts]("a;sdlfj") should be(DefaultThree())
        }
        it("Enum support") {
          val sjy = ScalaJack(DelimitedFlavor()).enumsAsInts()
          val i = Shirt(1, Size.Small)
          val d = sjy.render(i)
          d should equal("1,0")
          sjy.read[Shirt](d) should be(i)

          val sjz = ScalaJack(DelimitedFlavor())
          val d2 = sjz.render(i)
          d2 should equal("1,Small")
          sjz.read[Shirt](d2) should be(i)

          val d3 = "1,\"Small\""
          sjz.read[Shirt](d3) should be(i)

          val d4 = "1,0"
          sjz.read[Shirt](d4) should be(i)

          val d6 = "1,Huge"
          val msg =
            """No value found in enumeration co.blocke.scalajack.delimited.Size$ for Huge
            |1,Huge
            |--^""".stripMargin
          the[ScalaJackError] thrownBy sjz.read[Shirt](d6) should have message msg
        }
        it("Empty input") {
          sj.render("") should equal("")
          sj.read[Shirt]("") should be(null)
        }
        it("Unnecessary quotes on fields work") { // "a",b,"c"
          val delim = "\"a\",b,\"c\""
          val inst = sj.read[ThreeStrings](delim)
          inst should be(ThreeStrings("a", "b", "c"))
        }
        it("Escaped quotes in string work") { //  a,b""c,d
          val delim = "a,\"b\"\"x\",c"
          val inst = sj.read[ThreeStrings](delim)
          inst should be(ThreeStrings("a", "b\"x", "c"))
        }
        it("Basic fails") {
          val delim = """5,25,123.45,12.3,x,Hey,true,bogus12345678,0.123458867"""
          val msg =
            """Expected a Number here
            |5,25,123.45,12.3,x,Hey,true,bogus12345678,0.123458867
            |----------------------------^""".stripMargin
          the[ScalaJackError] thrownBy sj.read[AllPrim](delim) should have message msg
        }
      }
      describe("Classes") {
        it("Nested case class") {
          val n = Nested("item", Inside(1, "One"), Inside(2, "Two"))
          val delim = sj.render(n)
          delim should equal("""item,"1,One","2,Two"""")
          val inst = sj.read[Nested](delim)
          inst should equal(n)
        }
        it("Null class field value (nullable) w/no default") {
          val delim = """item,,"2,Two""""
          val inst = Nested("item", null, Inside(2, "Two"))
          sj.read[Nested](delim) should be(inst)
          sj.render(inst) should be(delim)
        }
        it("Empty (String) and null primitive class field") {
          val delim = ""","1,One","2,Two""""
          val inst = Nested("", Inside(1, "One"), Inside(2, "Two"))
          sj.read[Nested](delim) should be(inst)
          sj.render(inst) should be(delim)
        }
        it("Null class field value with default") {
          val delim = """item,"1,One","""
          sj.read[Nested](delim) should be(
            Nested("item", Inside(1, "One"), Inside(99, "dunno"))
          )
        }
      }
      describe("Lists") {
        it("Simple list works") {
          val s = WithList(5, List("a", "b", "c"))
          val delim = sj.render(s)
          delim should equal("""5,"a,b,c"""")
          sj.read[WithList[String]](delim) should be(s)
        }
        it("Nullables") {
          val s = WithList(5, List(BigInt(5), null, BigInt(7)))
          val delim = sj.render(s)
          delim should equal("""5,"5,,7"""")
          sj.read[WithList[BigInt]](delim) should be(s)
        }
        it("Null list") {
          val s = WithList[Int](5, null)
          val delim = sj.render[WithList[Int]](s)
          delim should equal("5,")
        }
        it("Empty List") {
          val inst = sj.read[WithList[Int]]("5,")
          inst should be(WithList(5, null))
          sj.render(inst) should be("5,")
        }
        it("Non-nullables") {
          val delim = """5,"1,,3""""
          the[NumberFormatException] thrownBy sj.read[WithList[Int]](delim) should have message "null"
        }
        it("Nested lists") {
          val s = WithList(3, List(List("a,b", "c"), List("x", "y")))
          val delim = sj.render(s)
          delim should equal("3,\"\"\"\"\"\"\"a,b\"\"\"\",c\"\",\"\"x,y\"\"\"")
          sj.read[WithList[List[String]]](delim) should be(s)
        }
      }
      describe("Options") {
        it("Optional case class fields") {
          val s = HasOption(None, Some(7))
          val delim = sj.render(s)
          delim should equal(",7")
          sj.read[HasOption](delim) should be(s)
        }
        it("Missing optional case class field with default") {
          val s = HasOption(Some(7), None)
          val delim = sj.render(s)
          delim should equal("7,")
          sj.read[HasOption](delim) should be(HasOption(Some(7), Some(5)))
        }
        it("Optional list members") {
          val s = WithList(3, List(Some(1), None, Some(2)))
          val delim = sj.render(s)
          delim should equal("3,\"1,2\"")
          sj.read[WithList[Option[Int]]](delim) should be(
            WithList(3, List(Some(1), Some(2)))
          )
          sj.read[WithList[Option[Int]]]("3,\"1,,2\"") should be(s)
        }
      }
      describe("Tuples") {
        it("Tuple case class fields") {
          val s = HasTuples(("a", 3), (false, 9))
          val delim = sj.render(s)
          delim should equal("\"a,3\",\"false,9\"")
          sj.read[HasTuples](delim) should be(s)
        }
        it("Tuple with class value") {
          val delim = "\"thing,\"\"1,foo\"\"\""
          sj.read[HasTuples2](delim) should be(
            HasTuples2(("thing", Inside(1, "foo")))
          )
        }
        it("Tuple with null value") {
          val delim = "\"thing,\""
          sj.read[HasTuples2](delim) should be(HasTuples2(("thing", null)))
        }
        it("Tuple with escaped quote in value") {
          val s = HasTuples(("a\"b", 3), (false, 9))
          val delim = sj.render(s)
          delim should equal("\"\"\"a\"\"\"\"b\"\",3\",\"false,9\"")
          sj.read[HasTuples](delim) should be(s)
        }
        it("Missing optional case class field with default") {
          val delim = "\"a,3\","
          sj.read[HasTuples](delim) should be(HasTuples(("a", 3), (true, 1)))
        }
        it("Tuple with Classish elements") {
          val inst = HasTuples3((true, Inside(4, "foom")))
          val delim = sj.render(inst)
          delim should be("\"true,\"\"4,foom\"\"\"")
          sj.read[HasTuples3](delim) should be(inst)
        }
      }
      describe("Either") {
        it("Supports Either parsing") {
          val s1 = HasEither(1, Left(3))
          val delim1 = sj.render(s1)
          delim1 should equal("1,3")
          sj.read[HasEither](delim1) should be(s1)

          val s2 = HasEither(2, Right(Inside(99, "foo")))
          val delim2 = sj.render(s2)
          delim2 should equal("2,\"99,foo\"")
          sj.read[HasEither](delim2) should be(s2)

          val s3 = HasEither(2, Right(null))
          sj.render(s3) should equal("2,")
        }
        it("Null object") {
          sj.render[HasEither](null) should equal("")
        }
        it("Either missing a value (no default)") {
          val delim = ",\"99,foo\""
          the[java.lang.NumberFormatException] thrownBy sj.read[HasEither](delim) should have message "null"
        }
        it("Supports Either field value with default specified") {
          val delim = "15,"
          val i = sj.read[HasEither2](delim)
          i should be(HasEither2(15, Right(Inside(1, "ok"))))
        }
        it("Can't parse either side of Either") {
          val msg =
            """Failed to read either side of Either
            |3,true
            |^""".stripMargin
          the[ScalaJackError] thrownBy sj.read[Shirt2]("3,true") should have message msg
        }
        it("Either with embedded quote in string value") {
          val s = HasEither3(1, Left("a\"b"))
          val delim = sj.render(s)
          delim should equal("1,\"a\"\"b\"")
          sj.read[HasEither3](delim) should be(s)
        }
        it("Write a null Either value") {
          val delim = "5,"
          val inst = HasEither(5, null)
          sj.render(inst) should be(delim)
          sj.read[HasEither](delim) should be(inst)
        }
        it("Write a Left Classish value") {
          val delim = "5,15"
          val inst = HasEitherRev(5, Right(15))
          sj.render(inst) should be(delim)
          sj.read[HasEitherRev](delim) should be(inst)
        }
        it("Write a Right non-Classish value") {
          val delim = "5,\"15,Mike\""
          val inst = HasEitherRev(5, Left(Inside(15, "Mike")))
          sj.render(inst) should be(delim)
          sj.read[HasEitherRev](delim) should be(inst)
        }
      }
      describe("forType") {
        it("Stock forType behavior") {
          val z = sj.forType[HasEither3]
          val s = HasEither3(1, Left("a\"b"))
          val delim = z.render(s)
          delim should equal("1,\"a\"\"b\"")
          z.read(delim) should be(s)
        }
        it("Can still read non-forType types") {
          val z = sj.forType[HasEither3]
          val delim = "2,\"99,foo\""
          val inst = HasEither(2, Right(Inside(99, "foo")))
          z.read[HasEither](delim) should be(inst)
          z.render(inst) should be(delim)
          val y = z.forType[Inside]
          y.read("1,foom") should be(Inside(1, "foom"))
        }
      }
      describe("Plug coverage holes") {
        it("Non-case classes fail") {
          val delim = "John,35"
          val msg =
            """Only case classes with non-empty constructors are supported for delimited data.
                    |John,35
                    |^""".stripMargin
          the[ScalaJackError] thrownBy sj.read[Busted](delim) should have message msg
        }
        it("No Map support") {
          val delim = "John,35"
          val msg = """No Map support for delimited data.
                    |John,35
                    |-----^""".stripMargin
          the[ScalaJackError] thrownBy sj.read[Busted2](delim) should have message msg
          val msg2 = "Map-typed data is not supported for delimited output"
          the[ScalaJackError] thrownBy sj.render(Busted2("foo", Map("a" -> "b"))) should have message msg2
        }
        it("Write null string") {
          sj.render(Inside(5, null)) should be("5,")
        }
        it("Invalid Boolean value") {
          val delim = "John,35"
          val msg = """Expected a Boolean here
                    |John,35
                    |-----^""".stripMargin
          the[ScalaJackError] thrownBy sj.read[Busted3](delim) should have message msg
        }
        it("Null List value") {
          val delim = "John,"
          sj.read[Busted4](delim) should be(Busted4("John", null))
        }
        it("Parser") {
          sj.parse("John,")
        }
      }
    }
}
