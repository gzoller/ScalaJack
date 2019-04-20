package co.blocke.scalajack
package delimited

import model._

import org.scalatest.{ FunSpec, Matchers, PrivateMethodTester }

class DelimSpec extends FunSpec with Matchers with PrivateMethodTester {

  val sj = ScalaJack(DelimitedFlavor())

  describe("---------------------\n:  Delimited Tests  :\n---------------------") {
    describe("DelimSpec") {
      it("Basic DelimSpec") {
        val all = AllPrim(5, 25L, 123.45, 12.3F, 'x', "Hey", true, BigInt(12345678), BigDecimal(0.123458867))
        val delim = sj.render(all)
        delim should equal("""5,25,123.45,12.3,x,Hey,true,12345678,0.123458867""")
        val inst = sj.read[AllPrim](delim)
        inst should equal(all)
      }
      it("Loose Change") {
        the[SJError] thrownBy sj.render(Map("a" -> 1)) should have message "Map-typed data is not supported for delimited output"
        val bi = BigInt(123)
        sj.render(bi) should be("123")

        val reader = sj.parse("abc")
        reader.scanForHint("foo") should be(None)
        reader.scanForType(util.Path.Root, "foo", None) should be(None)

        val t = reader.head
        reader.back
        reader.head should equal(t)
        reader.skipObject(util.Path.Root)
        reader.head should equal(t)

        the[UnsupportedOperationException] thrownBy sj.read[Map[String, Int]]("foo") should have message "Map serialization not available for Delimited encoding"

        val sjx = ScalaJack(DelimitedFlavor('|'))
        val i = Inside(5, "foo")
        val d = sjx.render(i)
        d should equal("5|foo")
        sjx.read[Inside](d) should be(i)

        val all = AllPrim(5, 25L, 123.45, 12.3F, 'x', "Hey", true, BigInt(12345678), BigDecimal(0.123458867))
        val delim = sj.render(all)
        val r = sj.parse(delim)
        val rPos = PrivateMethod[Int]('pos)
        r.next
        r.next
        r invokePrivate rPos() should be(2)
        r.hasNext should be(true)
        r.reset()
        r invokePrivate rPos() should be(0)
      }
      it("SalaJack configurations (DelimitedFlavor)") {
        an[UnsupportedOperationException] should be thrownBy ScalaJack(DelimitedFlavor()).withAdapters()
        an[UnsupportedOperationException] should be thrownBy ScalaJack(DelimitedFlavor()).withDefaultHint(null)
        an[UnsupportedOperationException] should be thrownBy ScalaJack(DelimitedFlavor()).withHintModifiers()
        an[UnsupportedOperationException] should be thrownBy ScalaJack(DelimitedFlavor()).withHints()
        an[UnsupportedOperationException] should be thrownBy ScalaJack(DelimitedFlavor()).withTypeValueModifier(null)
        an[UnsupportedOperationException] should be thrownBy ScalaJack(DelimitedFlavor()).allowPermissivePrimitives()
        val sjx = ScalaJack(DelimitedFlavor()).parseOrElse(typeOf[ThreeStrings] -> typeOf[DefaultThree])
        sjx.read[ThreeStrings]("a;sdlfj") should be(DefaultThree())
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

        val d5 = "1,\"0\""
        sjz.read[Shirt](d5) should be(i)

        val d6 = "1,Huge"
        val msg =
          """[$[1]]: No value found in enumeration co.blocke.scalajack.delimited.Size$ for Huge
            |1,Huge
            |-----^""".stripMargin
        the[ReadInvalidError] thrownBy sjz.read[Shirt](d6) should have message msg
      }
      it("Empty input") {
        sj.render("") should equal("")
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
          """[$[7]]: For input string: "bogu"
            |5,25,123.45,12.3,x,Hey,true,bogus12345678,0.123458867
            |----------------------------------------^""".stripMargin
        the[ReadMalformedError] thrownBy sj.read[AllPrim](delim) should have message msg
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
        val msg =
          """[$]: Null or missing fields must either be optional or provide default vales for delimited input
            |item,,"2,Two"
            |-----^""".stripMargin
        the[ReadInvalidError] thrownBy sj.read[Nested](delim) should have message msg

      }
      it("Null primitive class field") {
        val delim = ""","1,One","2,Two""""
        val msg =
          """[$]: Null or missing fields must either be optional or provide default vales for delimited input
          |,"1,One","2,Two"
          |^""".stripMargin
        the[ReadInvalidError] thrownBy sj.read[Nested](delim) should have message msg
      }
      it("Null class field value with default") {
        val delim = """item,"1,One","""
        sj.read[Nested](delim) should be(Nested("item", Inside(1, "One"), Inside(99, "dunno")))
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
        val s = WithList(5, List("a", null, "c"))
        val delim = sj.render(s)
        delim should equal("""5,"a,,c"""")
        sj.read[WithList[String]](delim) should be(s)
      }
      it("Null list") {
        val s = WithList[Int](5, null)
        val delim = sj.render[WithList[Int]](s)
        delim should equal("5,")
      }
      it("Empty List") {
        val msg =
          """[$]: Null or missing fields must either be optional or provide default vales for delimited input
            |5,
            |--^""".stripMargin
        the[ReadInvalidError] thrownBy sj.read[WithList[Int]]("5,") should have message msg
      }
      it("Non-nullables") {
        val delim = """5,"1,,3""""
        val msg =
          """[$[1]]: null
          |1,,3
          |--^""".stripMargin
        the[ReadMalformedError] thrownBy sj.read[WithList[Int]](delim) should have message msg
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
        sj.read[WithList[Option[Int]]](delim) should be(WithList(3, List(Some(1), Some(2))))
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
        sj.read[HasTuples2](delim) should be(HasTuples2(("thing", Inside(1, "foo"))))
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
      it("Missing optional case class field w/o default") {
        val delim = ",\"false,9\""
        val msg =
          """[$]: Null or missing fields must either be optional or provide default vales for delimited input
            |,"false,9"
            |^""".stripMargin
        the[ReadInvalidError] thrownBy sj.read[HasTuples](delim) should have message msg
      }
    }
    //case class HasEither(one:Int, two:Either[Int,Inside])
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
        val msg =
          """[$]: Null or missing fields must either be optional or provide default vales for delimited input
            |,"99,foo"
            |^""".stripMargin
        the[ReadInvalidError] thrownBy sj.read[HasEither](delim) should have message msg
      }
      it("Supports Either field value with default specified") {
        val delim = "15,"
        val i = sj.read[HasEither2](delim)
        i should be(HasEither2(15, Right(Inside(1, "ok"))))
      }
      it("Can't parse either side of Either") {
        val msg =
          """[$[1]]: Failed to read either side of Either
            |3,true
            |^""".stripMargin
        the[ReadMalformedError] thrownBy sj.read[Shirt2]("3,true") should have message msg
      }
      it("Either with embedded quote in string value") {
        val s = HasEither3(1, Left("a\"b"))
        val delim = sj.render(s)
        delim should equal("1,\"a\"\"b\"")
        sj.read[HasEither3](delim) should be(s)
      }
    }
  }
}
