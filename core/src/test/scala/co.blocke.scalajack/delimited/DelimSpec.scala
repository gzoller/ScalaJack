package co.blocke.scalajack
package delimited

import model._

import org.scalatest.{ FunSpec, Matchers }

class DelimSpec extends FunSpec with Matchers {

  val sj = ScalaJack(DelimitedFlavor)

  describe("---------------------\n:  Delimited Tests  :\n---------------------") {
    describe("DelimSpec") {
      it("Basic DelimSpec") {
        val all = AllPrim(5, 25L, 123.45, 12.3F, 'x', "Hey", true, BigInt(12345678), BigDecimal(0.123458867))
        val delim = sj.render(all)
        delim should equal("""5,25,123.45,12.3,x,Hey,true,12345678,0.123458867""")
        val inst = sj.read[AllPrim](delim)
        inst should equal(all)
      }
      it("Map fails") {
        the[SJError] thrownBy sj.render(Map("a" -> 1)) should have message "Map-typed data is not supported for delimited output"
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
      it("Null class field value w/no default") {
        val delim = """item,,"2,Two""""
        val msg =
          """[$]: Null or mising fields must either be optional or provide default vales for delimited input
            |item,,"2,Two"
            |-----^""".stripMargin
        the[ReadInvalidError] thrownBy sj.read[Nested](delim) should have message msg
      }
      it("Null primitive class field") {
        val delim = ""","1,One","2,Two""""
        val msg =
          """[$]: Null or mising fields must either be optional or provide default vales for delimited input
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
          """[$]: Null or mising fields must either be optional or provide default vales for delimited input
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
      }
      it("Either missing a value (no default)") {
        val delim = ",\"99,foo\""
        val msg =
          """[$]: Null or mising fields must either be optional or provide default vales for delimited input
            |,"99,foo"
            |^""".stripMargin
        the[ReadInvalidError] thrownBy sj.read[HasEither](delim) should have message msg
      }
      it("Supports Either field value with default specified") {
        val delim = "15,"
        val i = sj.read[HasEither2](delim)
        i should be(HasEither2(15, Right(Inside(1, "ok"))))
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
