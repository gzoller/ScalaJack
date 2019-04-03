package co.blocke.scalajack
package json.primitives

import org.scalatest.{ FunSpec, Matchers }

import scala.math.BigDecimal

case class Holder[T](value: T)

class PermissivePrimitiveSpec() extends FunSpec with Matchers {

  val sj = ScalaJack().allowPermissivePrimitives()

  describe("---------------------------------\n:  Permissive DelimSpec Tests  :\n---------------------------------") {
    it("Boolean must work") {
      assertResult(Holder(true)) { sj.read[Holder[Boolean]]("""{"value":true}""") }
      assertResult(Holder(true)) { sj.read[Holder[Boolean]]("""{"value":"true"}""") }
      assertResult(Holder(false)) { sj.read[Holder[Boolean]]("""{"value":false}""") }
      assertResult(Holder(false)) { sj.read[Holder[Boolean]]("""{"value":"false"}""") }
      assertResult(Holder(java.lang.Boolean.TRUE)) { sj.read[Holder[java.lang.Boolean]]("""{"value":true}""") }
      assertResult(Holder(java.lang.Boolean.TRUE)) { sj.read[Holder[java.lang.Boolean]]("""{"value":"true"}""") }
      assertResult(Holder(java.lang.Boolean.FALSE)) { sj.read[Holder[java.lang.Boolean]]("""{"value":false}""") }
      assertResult(Holder(java.lang.Boolean.FALSE)) { sj.read[Holder[java.lang.Boolean]]("""{"value":"false"}""") }
      assertResult(Holder[java.lang.Boolean](null)) { sj.read[Holder[java.lang.Boolean]]("""{"value":null}""") }
      val msg = """[$.value]: Expected Boolean here but found End
                  |
                  |^""".stripMargin
      the[co.blocke.scalajack.model.ReadUnexpectedError] thrownBy sj.read[Holder[java.lang.Boolean]]("""{"value":""}""") should have message msg
      assertResult("""{"value":true}""") { sj.render(Holder(true)) }
      assertResult("""{"value":false}""") { sj.render(Holder(false)) }
      assertResult("""{"value":true}""") { sj.render(Holder(java.lang.Boolean.TRUE)) }
      assertResult("""{"value":false}""") { sj.render(Holder(java.lang.Boolean.FALSE)) }
      assertResult("""{"value":null}""") { sj.render(Holder[java.lang.Boolean](null)) }
    }
    it("Byte must work") {
      assertResult(Holder(42.toByte)) { sj.read[Holder[Byte]]("""{"value":42}""") }
      assertResult(Holder(42.toByte)) { sj.read[Holder[Byte]]("""{"value":"42"}""") }
      assertResult(Holder(java.lang.Byte.valueOf(42.toByte))) { sj.read[Holder[java.lang.Byte]]("""{"value":42}""") }
      assertResult(Holder(java.lang.Byte.valueOf(42.toByte))) { sj.read[Holder[java.lang.Byte]]("""{"value":"42"}""") }
      assertResult(Holder[java.lang.Byte](null)) { sj.read[Holder[java.lang.Byte]]("""{"value":null}""") }
      val msg = """[$.value]: Expected Number here but found End
                  |
                  |^""".stripMargin
      the[co.blocke.scalajack.model.ReadUnexpectedError] thrownBy sj.read[Holder[java.lang.Byte]]("""{"value":""}""") should have message msg
      assertResult("""{"value":42}""") { sj.render(Holder(42.toByte)) }
      assertResult("""{"value":42}""") { sj.render(Holder(java.lang.Byte.valueOf(42.toByte))) }
      assertResult("""{"value":null}""") { sj.render(Holder[java.lang.Byte](null)) }
    }
    it("Double must work") {
      assertResult(Holder(42.5)) { sj.read[Holder[Double]]("""{"value":42.5}""") }
      assertResult(Holder(42.5)) { sj.read[Holder[Double]]("""{"value":"42.5"}""") }
      assertResult(Holder(java.lang.Double.valueOf(42.5))) { sj.read[Holder[java.lang.Double]]("""{"value":42.5}""") }
      assertResult(Holder(java.lang.Double.valueOf(42.5))) { sj.read[Holder[java.lang.Double]]("""{"value":"42.5"}""") }
      assertResult(Holder[java.lang.Double](null)) { sj.read[Holder[java.lang.Double]]("""{"value":null}""") }
      val msg = """[$.value]: Expected Number here but found End
                  |
                  |^""".stripMargin
      the[co.blocke.scalajack.model.ReadUnexpectedError] thrownBy sj.read[Holder[java.lang.Double]]("""{"value":""}""") should have message msg
      assertResult("""{"value":42.5}""") { sj.render(Holder(42.5)) }
      assertResult("""{"value":42.5}""") { sj.render(Holder(java.lang.Double.valueOf(42.5))) }
      assertResult("""{"value":null}""") { sj.render(Holder[java.lang.Double](null)) }
    }
    it("Float must work") {
      assertResult(Holder(42.5.toFloat)) { sj.read[Holder[Float]]("""{"value":42.5}""") }
      assertResult(Holder(42.5.toFloat)) { sj.read[Holder[Float]]("""{"value":"42.5"}""") }
      assertResult(Holder(java.lang.Float.valueOf(42.5.toFloat))) { sj.read[Holder[java.lang.Float]]("""{"value":42.5}""") }
      assertResult(Holder(java.lang.Float.valueOf(42.5.toFloat))) { sj.read[Holder[java.lang.Float]]("""{"value":"42.5"}""") }
      assertResult(Holder[java.lang.Float](null)) { sj.read[Holder[java.lang.Float]]("""{"value":null}""") }
      val msg = """[$.value]: Expected Number here but found End
                  |
                  |^""".stripMargin
      the[co.blocke.scalajack.model.ReadUnexpectedError] thrownBy sj.read[Holder[java.lang.Float]]("""{"value":""}""") should have message msg
      assertResult("""{"value":42.5}""") { sj.render(Holder(42.5.toFloat)) }
      assertResult("""{"value":42.5}""") { sj.render(Holder(java.lang.Float.valueOf(42.5.toFloat))) }
      assertResult("""{"value":null}""") { sj.render(Holder[java.lang.Float](null)) }
    }
    it("Int must work") {
      assertResult(Holder(42)) { sj.read[Holder[Int]]("""{"value":42}""") }
      assertResult(Holder(42)) { sj.read[Holder[Int]]("""{"value":"42"}""") }
      assertResult(Holder(java.lang.Integer.valueOf(42))) { sj.read[Holder[java.lang.Integer]]("""{"value":42}""") }
      assertResult(Holder(java.lang.Integer.valueOf(42))) { sj.read[Holder[java.lang.Integer]]("""{"value":"42"}""") }
      assertResult(Holder[java.lang.Integer](null)) { sj.read[Holder[java.lang.Integer]]("""{"value":null}""") }
      val msg = """[$.value]: Expected Number here but found End
                  |
                  |^""".stripMargin
      the[co.blocke.scalajack.model.ReadUnexpectedError] thrownBy sj.read[Holder[java.lang.Integer]]("""{"value":""}""") should have message msg
      assertResult("""{"value":42}""") { sj.render(Holder(42)) }
      assertResult("""{"value":42}""") { sj.render(Holder(java.lang.Integer.valueOf(42))) }
      assertResult("""{"value":null}""") { sj.render(Holder[java.lang.Integer](null)) }
    }
    it("Long must work") {
      assertResult(Holder(42.toLong)) { sj.read[Holder[Long]]("""{"value":42}""") }
      assertResult(Holder(42.toLong)) { sj.read[Holder[Long]]("""{"value":"42"}""") }
      assertResult(Holder(java.lang.Long.valueOf(42.toLong))) { sj.read[Holder[java.lang.Long]]("""{"value":42}""") }
      assertResult(Holder(java.lang.Long.valueOf(42.toLong))) { sj.read[Holder[java.lang.Long]]("""{"value":"42"}""") }
      assertResult(Holder[java.lang.Long](null)) { sj.read[Holder[java.lang.Long]]("""{"value":null}""") }
      val msg = """[$.value]: Expected Number here but found End
                  |
                  |^""".stripMargin
      the[co.blocke.scalajack.model.ReadUnexpectedError] thrownBy sj.read[Holder[java.lang.Long]]("""{"value":""}""") should have message msg
      assertResult("""{"value":42}""") { sj.render(Holder(42.toLong)) }
      assertResult("""{"value":42}""") { sj.render(Holder(java.lang.Long.valueOf(42.toLong))) }
      assertResult("""{"value":null}""") { sj.render(Holder[java.lang.Long](null)) }
    }
    it("Short must work") {
      assertResult(Holder(42.toShort)) { sj.read[Holder[Short]]("""{"value":42}""") }
      assertResult(Holder(42.toShort)) { sj.read[Holder[Short]]("""{"value":"42"}""") }
      assertResult(Holder(java.lang.Short.valueOf(42.toShort))) { sj.read[Holder[java.lang.Short]]("""{"value":42}""") }
      assertResult(Holder(java.lang.Short.valueOf(42.toShort))) { sj.read[Holder[java.lang.Short]]("""{"value":"42"}""") }
      assertResult(Holder[java.lang.Short](null)) { sj.read[Holder[java.lang.Short]]("""{"value":null}""") }
      val msg = """[$.value]: Expected Number here but found End
                  |
                  |^""".stripMargin
      the[co.blocke.scalajack.model.ReadUnexpectedError] thrownBy sj.read[Holder[java.lang.Short]]("""{"value":""}""") should have message msg
      assertResult("""{"value":42}""") { sj.render(Holder(42.toShort)) }
      assertResult("""{"value":42}""") { sj.render(Holder(java.lang.Short.valueOf(42.toShort))) }
      assertResult("""{"value":null}""") { sj.render(Holder[java.lang.Short](null)) }
    }
    it("Scala BigInt must work") {
      assertResult(Holder(BigInt(42))) { sj.read[Holder[BigInt]]("""{"value":42}""") }
      assertResult(Holder(BigInt(42))) { sj.read[Holder[BigInt]]("""{"value":"42"}""") }
      assertResult(Holder[BigInt](null)) { sj.read[Holder[BigInt]]("""{"value":null}""") }
      val msg = """[$.value]: Expected Number here but found End
                  |
                  |^""".stripMargin
      the[co.blocke.scalajack.model.ReadUnexpectedError] thrownBy sj.read[Holder[BigInt]]("""{"value":""}""") should have message msg
      assertResult("""{"value":42}""") { sj.render(Holder(BigInt(42))) }
      assertResult("""{"value":null}""") { sj.render(Holder[BigInt](null)) }
    }
    it("Java BigInteger must work") {
      assertResult(Holder(java.math.BigInteger.valueOf(42))) { sj.read[Holder[java.math.BigInteger]]("""{"value":42}""") }
      assertResult(Holder(java.math.BigInteger.valueOf(42))) { sj.read[Holder[java.math.BigInteger]]("""{"value":"42"}""") }
      assertResult(Holder[java.math.BigInteger](null)) { sj.read[Holder[java.math.BigInteger]]("""{"value":null}""") }
      val msg = """[$.value]: Expected Number here but found End
                  |
                  |^""".stripMargin
      the[co.blocke.scalajack.model.ReadUnexpectedError] thrownBy sj.read[Holder[java.math.BigInteger]]("""{"value":""}""") should have message msg
      assertResult("""{"value":42}""") { sj.render(Holder(java.math.BigInteger.valueOf(42))) }
      assertResult("""{"value":null}""") { sj.render(Holder[java.math.BigInteger](null)) }
    }
    it("Scala BigDecimal must work") {
      assertResult(Holder(BigDecimal("12.34"))) { sj.read[Holder[BigDecimal]]("""{"value":12.34}""") }
      assertResult(Holder(BigDecimal("12.34"))) { sj.read[Holder[BigDecimal]]("""{"value":"12.34"}""") }
      assertResult(Holder[BigDecimal](null)) { sj.read[Holder[BigDecimal]]("""{"value":null}""") }
      val msg = """[$.value]: Expected Number here but found End
                  |
                  |^""".stripMargin
      the[co.blocke.scalajack.model.ReadUnexpectedError] thrownBy sj.read[Holder[BigDecimal]]("""{"value":""}""") should have message msg
      assertResult("""{"value":12.34}""") { sj.render(Holder(BigDecimal("12.34"))) }
      assertResult("""{"value":null}""") { sj.render(Holder[BigDecimal](null)) }
    }
    it("Java BigDecimal must work") {
      assertResult(Holder(new java.math.BigDecimal("12.34"))) { sj.read[Holder[java.math.BigDecimal]]("""{"value":12.34}""") }
      assertResult(Holder(new java.math.BigDecimal("12.34"))) { sj.read[Holder[java.math.BigDecimal]]("""{"value":"12.34"}""") }
      assertResult(Holder[java.math.BigDecimal](null)) { sj.read[Holder[java.math.BigDecimal]]("""{"value":null}""") }
      val msg = """[$.value]: Expected Number here but found End
                  |
                  |^""".stripMargin
      the[co.blocke.scalajack.model.ReadUnexpectedError] thrownBy sj.read[Holder[java.math.BigDecimal]]("""{"value":""}""") should have message msg
      assertResult("""{"value":12.34}""") { sj.render(Holder(new java.math.BigDecimal("12.34"))) }
      assertResult("""{"value":null}""") { sj.render(Holder[java.math.BigDecimal](null)) }
    }
    it("Java Number must work") {
      assertResult(Holder(java.lang.Double.valueOf(42.5).asInstanceOf[Number])) { sj.read[Holder[java.lang.Number]]("""{"value":42.5}""") }
      assertResult(Holder(java.lang.Double.valueOf(42.5).asInstanceOf[Number])) { sj.read[Holder[java.lang.Number]]("""{"value":"42.5"}""") }
      assertResult(Holder[java.lang.Number](null)) { sj.read[Holder[java.lang.Number]]("""{"value":null}""") }
      val msg = """[$.value]: Expected Number here but found End
                  |
                  |^""".stripMargin
      the[co.blocke.scalajack.model.ReadUnexpectedError] thrownBy sj.read[Holder[java.lang.Number]]("""{"value":""}""") should have message msg
      assertResult("""{"value":42.5}""") { sj.render(Holder(java.lang.Double.valueOf(42.5).asInstanceOf[Number])) }
      assertResult("""{"value":null}""") { sj.render(Holder[java.lang.Number](null)) }
    }
  }
}
