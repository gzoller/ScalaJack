package co.blocke.scalajack
package delimited

import TestUtil._
import munit._
import munit.internal.console
import co.blocke.scala_reflection.RType


class DelimSpec extends FunSuite:

  val sj = ScalaJack(DelimitedFlavor())

  test("Basic DelimSpec") {
    describe("---------------------\n:  Delimited Tests  :\n---------------------", Console.BLUE)
    describe("DelimSpec")

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
    assertEquals(delim, """5,25,123.45,12.3,x,Hey,true,12345678,0.123458867""".asInstanceOf[DELIMITED])
    val inst = sj.read[AllPrim](delim)
    assertEquals(inst, all)
  }

  test("SalaJack configurations (DelimitedFlavor)") {
    intercept[ScalaJackError]{
      ScalaJack(DelimitedFlavor()).withAdapters()
    }
    intercept[ScalaJackError]{
      ScalaJack(DelimitedFlavor()).withDefaultHint(null)
    }
    intercept[ScalaJackError]{
      ScalaJack(DelimitedFlavor()).withHintModifiers()
    }
    intercept[ScalaJackError]{
      ScalaJack(DelimitedFlavor()).withHints()
    }
    intercept[ScalaJackError]{
      ScalaJack(DelimitedFlavor()).withTypeValueModifier(null)
    }
    intercept[ScalaJackError]{
      ScalaJack(DelimitedFlavor()).allowPermissivePrimitives()
    }     
    val sjx = ScalaJack(DelimitedFlavor())
      .parseOrElse(RType.of[Address] -> RType.of[DefaultAddress])
    assert(sjx.read[Address]("a;sdlfj".asInstanceOf[DELIMITED]) == DefaultAddress("a;sdlfj"))
  }

  test("Enum support") {
    val sjy = ScalaJack(DelimitedFlavor()).enumsAsInts()
    val i = Shirt(1, Size.Small)
    val d = sjy.render(i)
    assertEquals(d, "1,0".asInstanceOf[DELIMITED])
    assertEquals(sjy.read[Shirt](d), i)

    val sjz = ScalaJack(DelimitedFlavor())
    val d2 = sjz.render(i)
    assertEquals(d2, "1,Small".asInstanceOf[DELIMITED])
    assertEquals(sjz.read[Shirt](d2), i)

    val d3 = "1,\"Small\"".asInstanceOf[DELIMITED]
    assertEquals(sjz.read[Shirt](d3), i)

    val d4 = "1,0".asInstanceOf[DELIMITED]
    assertEquals(sjz.read[Shirt](d4), i)

    val d6 = "1,Huge".asInstanceOf[DELIMITED]
    val msg =
      """No value found in enumeration co.blocke.scalajack.delimited.Size$ for Huge
      |1,Huge
      |--^""".stripMargin
    interceptMessage[ScalaJackError](msg){
      sjz.read[Shirt](d6)
    }
  }

  test("Empty input") {
    assertEquals(sj.render("".asInstanceOf[DELIMITED]), "".asInstanceOf[DELIMITED])
    assertEquals(sj.read[Shirt]("".asInstanceOf[DELIMITED]), null)
  }

  test("Unnecessary quotes on fields work") { // "a",b,"c"
    val delim = "\"a\",b,\"c\""
    val inst = sj.read[ThreeStrings](delim.asInstanceOf[DELIMITED])
    assertEquals(inst, ThreeStrings("a", "b", "c"))
  }

  test("Escaped quotes in string work") { //  a,b""c,d
    val delim = "a,\"b\"\"x\",c"
    val inst = sj.read[ThreeStrings](delim.asInstanceOf[DELIMITED])
    assertEquals(inst, ThreeStrings("a", "b\"x", "c"))
  }

  test("Basic fails") {
    val delim = """5,25,123.45,12.3,x,Hey,true,bogus12345678,0.123458867"""
    val msg =
      """Expected a Number here
      |5,25,123.45,12.3,x,Hey,true,bogus12345678,0.123458867
      |----------------------------^""".stripMargin
    interceptMessage[co.blocke.scalajack.ScalaJackError](msg){
      sj.read[AllPrim](delim.asInstanceOf[DELIMITED])
    }
  }

  test("Nested case class") {
    describe("Classes")
    val n = Nested("item", Inside(1, "One"), Inside(2, "Two"))
    val delim = sj.render(n)
    assertEquals(delim, """item,"1,One","2,Two"""".asInstanceOf[DELIMITED])
    val inst = sj.read[Nested](delim)
    assertEquals(inst, n)
  }
  
  test("Null class field value (nullable) w/no default") {
    val delim = """item,,"2,Two"""".asInstanceOf[DELIMITED]
    val inst = Nested("item", null, Inside(2, "Two"))
    assertEquals(sj.read[Nested](delim), inst)
    assertEquals(sj.render(inst), delim)
  }

  test("Empty (String) and null primitive class field") {
    val delim = ""","1,One","2,Two"""".asInstanceOf[DELIMITED]
    val inst = Nested("", Inside(1, "One"), Inside(2, "Two"))
    assertEquals(sj.read[Nested](delim), inst)
    assertEquals(sj.render(inst), delim)
  }

  test("Null class field value with default") {
    val delim = """item,"1,One",""".asInstanceOf[DELIMITED]
    assertEquals(sj.read[Nested](delim),
      Nested("item", Inside(1, "One"), Inside(99, "dunno"))
    )
  }

  test("Simple list works") {
    describe("Lists")
    val s = WithList(5, List("a", "b", "c"))
    val delim = sj.render(s)
    assertEquals(delim, """5,"a,b,c"""".asInstanceOf[DELIMITED])
    assertEquals(sj.read[WithList[String]](delim), s)
  }
  
  test("Nullables") {
    val s = WithList(5, List(BigInt(5), null, BigInt(7)))
    val delim = sj.render(s)
    assertEquals(delim, """5,"5,,7"""".asInstanceOf[DELIMITED])
    assertEquals(sj.read[WithList[BigInt]](delim), s)
  }

  test("Null list") {
    val s = WithList[Int](5, null)
    val delim = sj.render[WithList[Int]](s)
    assertEquals(delim, "5,".asInstanceOf[DELIMITED])
  }

  test("Empty List") {
    val inst = sj.read[WithList[Int]]("5,".asInstanceOf[DELIMITED])
    assertEquals(inst, WithList(5, null))
    assertEquals(sj.render(inst), "5,".asInstanceOf[DELIMITED])
  }

  test("Non-nullables") {
    val delim = """5,"1,,3"""".asInstanceOf[DELIMITED]
    val msg = """Expected a Number here
              |,,3
              |-^""".stripMargin
    interceptMessage[co.blocke.scalajack.ScalaJackError](msg){
      sj.read[WithList[Int]](delim)
    }
  }

  test("Nested lists") {
    val s = WithList(3, List(List("a,b", "c"), List("x", "y")))
    val delim = sj.render(s)
    assertEquals(delim , "3,\"\"\"\"\"\"\"a,b\"\"\"\",c\"\",\"\"x,y\"\"\"".asInstanceOf[DELIMITED])
    assertEquals(sj.read[WithList[List[String]]](delim), s)
  }

  test("Optional case class fields") {
    describe("Options")
    val s = HasOption(None, Some(7))
    val delim = sj.render(s)
    assertEquals(delim, ",7".asInstanceOf[DELIMITED])
    assertEquals(sj.read[HasOption](delim), s)
  }

  test("Missing optional case class field with default") {
    val s = HasOption(Some(7), None)
    val delim = sj.render(s)
    assertEquals(delim, "7,".asInstanceOf[DELIMITED])
    assertEquals(sj.read[HasOption](delim), HasOption(Some(7), Some(5)))
  }

  test("Optional list members") {
    val s = WithList(3, List(Some(1), None, Some(2)))
    val delim = sj.render(s)
    assertEquals(delim, "3,\"1,2\"".asInstanceOf[DELIMITED])
    assertEquals(sj.read[WithList[Option[Int]]](delim),
      WithList(3, List(Some(1), Some(2)))
    )
    assertEquals(sj.read[WithList[Option[Int]]]("3,\"1,,2\"".asInstanceOf[DELIMITED]), s)
  }

  test("Tuple case class fields") {
    describe("Tuples")
    val s = HasTuples(("a", 3), (false, 9))
    val delim = sj.render(s)
    assertEquals(delim, "\"a,3\",\"false,9\"".asInstanceOf[DELIMITED])
    assertEquals(sj.read[HasTuples](delim), s)
  }
  
  test("Tuple with class value") {
    val delim = "\"thing,\"\"1,foo\"\"\"".asInstanceOf[DELIMITED]
    assertEquals(sj.read[HasTuples2](delim),
      HasTuples2(("thing", Inside(1, "foo")))
    )
  }

  test("Tuple with null value") {
    val delim = "\"thing,\"".asInstanceOf[DELIMITED]
    assertEquals(sj.read[HasTuples2](delim), HasTuples2(("thing", null)))
  }

  test("Tuple with escaped quote in value") {
    val s = HasTuples(("a\"b", 3), (false, 9))
    val delim = sj.render(s)
    assertEquals(delim , "\"\"\"a\"\"\"\"b\"\",3\",\"false,9\"".asInstanceOf[DELIMITED])
    assertEquals(sj.read[HasTuples](delim), s)
  }

  test("Missing optional case class field with default") {
    val delim = "\"a,3\",".asInstanceOf[DELIMITED]
    assertEquals(sj.read[HasTuples](delim), HasTuples(("a", 3), (true, 1)))
  }

  test("Tuple with Classish elements") {
    val inst = HasTuples3((true, Inside(4, "foom")))
    val delim = sj.render(inst)
    assertEquals(delim, "\"true,\"\"4,foom\"\"\"".asInstanceOf[DELIMITED])
    assertEquals(sj.read[HasTuples3](delim), inst)
  }

  test("Supports Either parsing") {
    describe("Either")
    val s1 = HasEither(1, Left(3))
    val delim1 = sj.render(s1)
    assertEquals(delim1, "1,3".asInstanceOf[DELIMITED])
    assertEquals(sj.read[HasEither](delim1), s1)

    val s2 = HasEither(2, Right(Inside(99, "foo")))
    val delim2 = sj.render(s2)
    assertEquals(delim2, "2,\"99,foo\"".asInstanceOf[DELIMITED])
    assertEquals(sj.read[HasEither](delim2), s2)

    val s3 = HasEither(2, Right(null))
    assertEquals( sj.render(s3), "2,".asInstanceOf[DELIMITED])
  }

  test("Null object") {
    assertEquals(sj.render[HasEither](null), "".asInstanceOf[DELIMITED])
  }

  test("Either missing a value (no default)") {
    val delim = ",\"99,foo\"".asInstanceOf[DELIMITED]
    val msg = """Expected a Number here
                |,"99,foo"
                |^""".stripMargin
    interceptMessage[co.blocke.scalajack.ScalaJackError](msg){
      sj.read[HasEither](delim)
    }
  }

  test("Supports Either field value with default specified") {
    val delim = "15,".asInstanceOf[DELIMITED]
    val i = sj.read[HasEither2](delim)
    assertEquals(i, HasEither2(15, Right(Inside(1, "ok"))))
  }

  test("Can't parse either side of Either") {
    val msg =
      """Failed to read either side of Either
      |3,true
      |^""".stripMargin
    interceptMessage[co.blocke.scalajack.ScalaJackError](msg){
      sj.read[Shirt2]("3,true".asInstanceOf[DELIMITED])
    }
  }

  test("Either with embedded quote in string value") {
    val s = HasEither3(1, Left("a\"b"))
    val delim = sj.render(s)
    assertEquals(delim, """1,"a""b"""".asInstanceOf[DELIMITED])
    assertEquals(sj.read[HasEither3](delim), s)
  }

  test("Write a null Either value") {
    val delim = "5,".asInstanceOf[DELIMITED]
    val inst = HasEither(5, null)
    assertEquals(sj.render(inst), delim)
    assertEquals(sj.read[HasEither](delim), inst)
  }

  test("Write a Right non-Classish value") {
    val delim = "5,true".asInstanceOf[DELIMITED]
    val inst = HasEitherRev(5, Right(true))
    assertEquals(sj.render(inst), delim)
    assertEquals(sj.read[HasEitherRev](delim), inst)
  }

  test("Write a Left Classish value") {
    val delim = "5,\"15,Mike\"".asInstanceOf[DELIMITED]
    val inst = HasEitherRev(5, Left(Inside(15, "Mike")))
    assertEquals(sj.render(inst), delim)
    assertEquals(sj.read[HasEitherRev](delim), inst)
  }

  test("Non-case classes fail") {
    describe("Plug coverage holes")
    val delim = "John,35".asInstanceOf[DELIMITED]
    val msg =
      """Only case classes with non-empty constructors are supported for delimited data.
              |John,35
              |^""".stripMargin
    interceptMessage[co.blocke.scalajack.ScalaJackError](msg){
      sj.read[Busted](delim)
    }
  }

  test("No Map support") {
    val delim = "John,35".asInstanceOf[DELIMITED]
    val msg = """No Map support for delimited data.
              |John,35
              |-----^""".stripMargin
    interceptMessage[co.blocke.scalajack.ScalaJackError](msg){
      sj.read[Busted2](delim)
    }
    val msg2 = "Map-typed data is not supported for delimited output"
    interceptMessage[co.blocke.scalajack.ScalaJackError](msg2){
      sj.render(Busted2("foo", Map("a" -> "b")))
    }
  }

  test("Write null string") {
    assertEquals(sj.render(Inside(5, null)), "5,".asInstanceOf[DELIMITED])
  }

  test("Invalid Boolean value") {
    val delim = "John,35".asInstanceOf[DELIMITED]
    val msg = """Expected a Boolean here
              |John,35
              |-----^""".stripMargin
    interceptMessage[co.blocke.scalajack.ScalaJackError](msg){
      sj.read[Busted3](delim)
    }
  }

  test("Null List value") {
    val delim = "John,".asInstanceOf[DELIMITED]
    assertEquals(sj.read[Busted4](delim), Busted4("John", null))
  }

  test("Parser") {
    sj.parse("John,".asInstanceOf[DELIMITED])
  }
