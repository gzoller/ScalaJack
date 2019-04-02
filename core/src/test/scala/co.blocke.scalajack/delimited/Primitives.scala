package co.blocke.scalajack
package delimited

import model._

import org.scalatest.{ FunSpec, Matchers }

class Primitives extends FunSpec with Matchers {

  val sj = ScalaJack(DelimitedFlavor)

  describe("---------------------\n:  Delimited Tests  :\n---------------------") {
    describe("Primitives") {
      it("Basic Primitives") {
        val all = AllPrim(5, 25L, 123.45, 12.3F, 'x', "Hey", true, BigInt(12345678), BigDecimal(0.123458867))
        val delim = sj.render(all)
        delim should equal("""5,25,123.45,12.3,x,Hey,true,12345678,0.123458867""")
        val inst = sj.read[AllPrim](delim)
        inst should equal(all)
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
  }
}
