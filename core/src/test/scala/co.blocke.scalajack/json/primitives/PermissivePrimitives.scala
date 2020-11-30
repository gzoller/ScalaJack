package co.blocke.scalajack
package json.primitives

import TestUtil._
import munit._
import munit.internal.console
import co.blocke.scalajack.json.JSON
import co.blocke.scala_reflection._

class PermissivePrimitives() extends FunSuite:

  val sj = co.blocke.scalajack.ScalaJack().allowPermissivePrimitives()

  test("Boolean must work") {
    describe("--------------------------------\n:  Permissive Primitive Tests  :\n--------------------------------", Console.BLUE)

    assertEquals(Holder(true), sj.read[Holder[Boolean]]("""{"value":true}""".asInstanceOf[JSON]))
    assertEquals(Holder(true), sj.read[Holder[Boolean]]("""{"value":"true"}""".asInstanceOf[JSON]))
    assertEquals(Holder(false), sj.read[Holder[Boolean]]("""{"value":false}""".asInstanceOf[JSON]))
    assertEquals(Holder(false), sj.read[Holder[Boolean]]("""{"value":"false"}""".asInstanceOf[JSON]))
    assertEquals(Holder(java.lang.Boolean.TRUE), sj.read[Holder[java.lang.Boolean]]("""{"value":true}""".asInstanceOf[JSON]))
    assertEquals(Holder(java.lang.Boolean.TRUE), sj.read[Holder[java.lang.Boolean]]("""{"value":"true"}""".asInstanceOf[JSON]))
    assertEquals(Holder(java.lang.Boolean.FALSE), sj.read[Holder[java.lang.Boolean]]("""{"value":false}""".asInstanceOf[JSON]))
    assertEquals(Holder(java.lang.Boolean.FALSE), sj.read[Holder[java.lang.Boolean]]("""{"value":"false"}""".asInstanceOf[JSON]))
    assertEquals(Holder[java.lang.Boolean](null), sj.read[Holder[java.lang.Boolean]]("""{"value":null}""".asInstanceOf[JSON]))
    
    val msg = """Expected a java.lang.Boolean here
              |{"value":""}
              |----------^""".stripMargin
    interceptMessage[co.blocke.scalajack.ScalaJackError](msg){
      sj.read[Holder[java.lang.Boolean]]("""{"value":""}""".asInstanceOf[JSON])
    }

    assertEquals("""{"value":true}""".asInstanceOf[JSON], sj.render(Holder(true)))
    assertEquals("""{"value":false}""".asInstanceOf[JSON], sj.render(Holder(false)))
    assertEquals("""{"value":true}""".asInstanceOf[JSON], sj.render(Holder(java.lang.Boolean.TRUE)))
    assertEquals("""{"value":false}""".asInstanceOf[JSON], sj.render(Holder(java.lang.Boolean.FALSE)))
    assertEquals("""{"value":null}""".asInstanceOf[JSON], sj.render(Holder[java.lang.Boolean](null)))
  }

  test("Byte must work") {
    assertEquals(Holder(42.toByte), sj.read[Holder[Byte]]("""{"value":42}""".asInstanceOf[JSON]))
    assertEquals(Holder(42.toByte), sj.read[Holder[Byte]]("""{"value":"42"}""".asInstanceOf[JSON]))
    assertEquals(Holder(java.lang.Byte.valueOf(42.toByte)), sj.read[Holder[java.lang.Byte]]("""{"value":42}""".asInstanceOf[JSON]))
    assertEquals(Holder(java.lang.Byte.valueOf(42.toByte)), sj.read[Holder[java.lang.Byte]]("""{"value":"42"}""".asInstanceOf[JSON]))
    // assertEquals(Holder[java.lang.Byte](null), sj.read[Holder[java.lang.Byte]]("""{"value":null}""".asInstanceOf[JSON]))

    val msg = """Expected a java.lang.Byte here
              |{"value":""}
              |----------^""".stripMargin
    interceptMessage[co.blocke.scalajack.ScalaJackError](msg){
      sj.read[Holder[java.lang.Byte]]("""{"value":""}""".asInstanceOf[JSON])
    }

    assertEquals("""{"value":42}""".asInstanceOf[JSON], sj.render(Holder(42.toByte)))
    assertEquals("""{"value":42}""".asInstanceOf[JSON], sj.render(Holder(java.lang.Byte.valueOf(42.toByte))))
    assertEquals("""{"value":null}""".asInstanceOf[JSON], sj.render(Holder[java.lang.Byte](null)))
  }

  test("Double must work") {
    assertEquals(Holder(42.5), sj.read[Holder[Double]]("""{"value":42.5}""".asInstanceOf[JSON]))
    assertEquals(Holder(42.5), sj.read[Holder[Double]]("""{"value":"42.5"}""".asInstanceOf[JSON]))
    assertEquals(Holder(java.lang.Double.valueOf(42.5)), sj.read[Holder[java.lang.Double]]("""{"value":42.5}""".asInstanceOf[JSON]))
    assertEquals(Holder(java.lang.Double.valueOf(42.5)), sj.read[Holder[java.lang.Double]]("""{"value":"42.5"}""".asInstanceOf[JSON]))
    // assertEquals(Holder[java.lang.Double](null), sj.read[Holder[java.lang.Double]]("""{"value":null}""".asInstanceOf[JSON]))

    val msg = """Expected a java.lang.Double here
              |{"value":""}
              |----------^""".stripMargin
    interceptMessage[co.blocke.scalajack.ScalaJackError](msg){
      sj.read[Holder[java.lang.Double]]("""{"value":""}""".asInstanceOf[JSON])
    }

    assertEquals("""{"value":42.5}""".asInstanceOf[JSON], sj.render(Holder(42.5)))
    assertEquals("""{"value":42.5}""".asInstanceOf[JSON], sj.render(Holder(java.lang.Double.valueOf(42.5))))
    assertEquals("""{"value":null}""".asInstanceOf[JSON], sj.render(Holder[java.lang.Double](null)))
  }

  test("Float must work") {
    assertEquals(Holder(42.5.toFloat), sj.read[Holder[Float]]("""{"value":42.5}""".asInstanceOf[JSON]))
    assertEquals(Holder(42.5.toFloat), sj.read[Holder[Float]]("""{"value":"42.5"}""".asInstanceOf[JSON]))
    assertEquals(Holder(java.lang.Float.valueOf(42.5.toFloat)), sj.read[Holder[java.lang.Float]]("""{"value":42.5}""".asInstanceOf[JSON]))
    assertEquals(Holder(java.lang.Float.valueOf(42.5.toFloat)), sj.read[Holder[java.lang.Float]]("""{"value":"42.5"}""".asInstanceOf[JSON]))
    // assertEquals(Holder[java.lang.Float](null), sj.read[Holder[java.lang.Float]]("""{"value":null}""".asInstanceOf[JSON]))

    val msg = """Expected a java.lang.Float here
              |{"value":""}
              |----------^""".stripMargin
    interceptMessage[co.blocke.scalajack.ScalaJackError](msg){
      sj.read[Holder[java.lang.Float]]("""{"value":""}""".asInstanceOf[JSON])
    }

    assertEquals("""{"value":42.5}""".asInstanceOf[JSON], sj.render(Holder(42.5.toFloat)))
    assertEquals("""{"value":42.5}""".asInstanceOf[JSON], sj.render(Holder(java.lang.Float.valueOf(42.5.toFloat))))
    assertEquals("""{"value":null}""".asInstanceOf[JSON], sj.render(Holder[java.lang.Float](null)))
  }

  test("Int must work") {
    assertEquals(Holder(42), sj.read[Holder[Int]]("""{"value":42}""".asInstanceOf[JSON]))
    assertEquals(Holder(42), sj.read[Holder[Int]]("""{"value":"42"}""".asInstanceOf[JSON]))
    assertEquals(Holder(java.lang.Integer.valueOf(42)), sj.read[Holder[java.lang.Integer]]("""{"value":42}""".asInstanceOf[JSON]))
    assertEquals(Holder(java.lang.Integer.valueOf(42)), sj.read[Holder[java.lang.Integer]]("""{"value":"42"}""".asInstanceOf[JSON]))
    // assertEquals(Holder[java.lang.Integer](null), sj.read[Holder[java.lang.Integer]]("""{"value":null}""".asInstanceOf[JSON]))

    val msg = """Expected a java.lang.Integer here
              |{"value":""}
              |----------^""".stripMargin
    interceptMessage[co.blocke.scalajack.ScalaJackError](msg){
      sj.read[Holder[java.lang.Integer]]("""{"value":""}""".asInstanceOf[JSON])
    }

    assertEquals("""{"value":42}""".asInstanceOf[JSON], sj.render(Holder(42)))
    assertEquals("""{"value":42}""".asInstanceOf[JSON], sj.render(Holder(java.lang.Integer.valueOf(42))))
    assertEquals("""{"value":null}""".asInstanceOf[JSON], sj.render(Holder[java.lang.Integer](null)))
  }

  test("Long must work") {
    assertEquals(Holder(42.toLong), sj.read[Holder[Long]]("""{"value":42}""".asInstanceOf[JSON]))
    assertEquals(Holder(42.toLong), sj.read[Holder[Long]]("""{"value":"42"}""".asInstanceOf[JSON]))
    assertEquals(Holder(java.lang.Long.valueOf(42.toLong)), sj.read[Holder[java.lang.Long]]("""{"value":42}""".asInstanceOf[JSON]))
    assertEquals(Holder(java.lang.Long.valueOf(42.toLong)), sj.read[Holder[java.lang.Long]]("""{"value":"42"}""".asInstanceOf[JSON]))
    // assertEquals(Holder[java.lang.Long](null), sj.read[Holder[java.lang.Long]]("""{"value":null}""".asInstanceOf[JSON]))

    val msg = """Expected a java.lang.Long here
              |{"value":""}
              |----------^""".stripMargin
    interceptMessage[co.blocke.scalajack.ScalaJackError](msg){
      sj.read[Holder[java.lang.Long]]("""{"value":""}""".asInstanceOf[JSON])
    }

    assertEquals("""{"value":42}""".asInstanceOf[JSON], sj.render(Holder(42.toLong)))
    assertEquals("""{"value":42}""".asInstanceOf[JSON], sj.render(Holder(java.lang.Long.valueOf(42.toLong))))
    assertEquals("""{"value":null}""".asInstanceOf[JSON], sj.render(Holder[java.lang.Long](null)))
  }

  test("Short must work") {
    assertEquals(Holder(42.toShort), sj.read[Holder[Short]]("""{"value":42}""".asInstanceOf[JSON]))
    assertEquals(Holder(42.toShort), sj.read[Holder[Short]]("""{"value":"42"}""".asInstanceOf[JSON]))
    assertEquals(Holder(java.lang.Short.valueOf(42.toShort)), sj.read[Holder[java.lang.Short]]("""{"value":42}""".asInstanceOf[JSON]))
    assertEquals(Holder(java.lang.Short.valueOf(42.toShort)), sj.read[Holder[java.lang.Short]]("""{"value":"42"}""".asInstanceOf[JSON]))
    // assertEquals(Holder[java.lang.Short](null), sj.read[Holder[java.lang.Short]]("""{"value":null}""".asInstanceOf[JSON]))

    val msg = """Expected a java.lang.Short here
              |{"value":""}
              |----------^""".stripMargin
    interceptMessage[co.blocke.scalajack.ScalaJackError](msg){
      sj.read[Holder[java.lang.Short]]("""{"value":""}""".asInstanceOf[JSON])
    }

    assertEquals("""{"value":42}""".asInstanceOf[JSON], sj.render(Holder(42.toShort)))
    assertEquals("""{"value":42}""".asInstanceOf[JSON], sj.render(Holder(java.lang.Short.valueOf(42.toShort))))
    assertEquals("""{"value":null}""".asInstanceOf[JSON], sj.render(Holder[java.lang.Short](null)))
  }

  test("Scala BigInt must work") {
    assertEquals(Holder(BigInt(42)), sj.read[Holder[BigInt]]("""{"value":42}""".asInstanceOf[JSON]))
    assertEquals(Holder(BigInt(42)), sj.read[Holder[BigInt]]("""{"value":"42"}""".asInstanceOf[JSON]))
    assertEquals(Holder[BigInt](null), sj.read[Holder[BigInt]]("""{"value":null}""".asInstanceOf[JSON]))

    val msg = """Expected a scala.math.BigInt here
              |{"value":""}
              |----------^""".stripMargin
    interceptMessage[co.blocke.scalajack.ScalaJackError](msg){
      sj.read[Holder[BigInt]]("""{"value":""}""".asInstanceOf[JSON])
    }

    assertEquals("""{"value":42}""".asInstanceOf[JSON], sj.render(Holder(BigInt(42))))
    assertEquals("""{"value":null}""".asInstanceOf[JSON], sj.render(Holder[BigInt](null)))
  }

  test("Java BigInteger must work") {
    assertEquals(Holder(java.math.BigInteger.valueOf(42)), sj.read[Holder[java.math.BigInteger]]("""{"value":42}""".asInstanceOf[JSON]))
    assertEquals(Holder(java.math.BigInteger.valueOf(42)), sj.read[Holder[java.math.BigInteger]]("""{"value":"42"}""".asInstanceOf[JSON]))
    // assertEquals(Holder[java.math.BigInteger](null), sj.read[Holder[java.math.BigInteger]]("""{"value":null}""".asInstanceOf[JSON]))

    val msg = """Expected a java.math.BigInteger here
              |{"value":""}
              |----------^""".stripMargin
    interceptMessage[co.blocke.scalajack.ScalaJackError](msg){
      sj.read[Holder[java.math.BigInteger]]("""{"value":""}""".asInstanceOf[JSON])
    }

    assertEquals("""{"value":42}""".asInstanceOf[JSON], sj.render(Holder(java.math.BigInteger.valueOf(42))))
    assertEquals("""{"value":null}""".asInstanceOf[JSON], sj.render(Holder[java.math.BigInteger](null)))
  }

  test("Scala BigDecimal must work") {
    assertEquals(Holder(BigDecimal("12.34")), sj.read[Holder[BigDecimal]]("""{"value":12.34}""".asInstanceOf[JSON]))
    assertEquals(Holder(BigDecimal("12.34")), sj.read[Holder[BigDecimal]]("""{"value":"12.34"}""".asInstanceOf[JSON]))
    // assertEquals(Holder[BigDecimal](null), sj.read[Holder[BigDecimal]]("""{"value":null}""".asInstanceOf[JSON]))

    val msg = """Expected a scala.math.BigDecimal here
              |{"value":""}
              |----------^""".stripMargin
    interceptMessage[co.blocke.scalajack.ScalaJackError](msg){
      sj.read[Holder[BigDecimal]]("""{"value":""}""".asInstanceOf[JSON])
    }

    assertEquals("""{"value":12.34}""".asInstanceOf[JSON], sj.render(Holder(BigDecimal("12.34"))))
    assertEquals("""{"value":null}""".asInstanceOf[JSON], sj.render(Holder[BigDecimal](null)))
  }

  test("Java BigDecimal must work") {
    assertEquals(Holder(new java.math.BigDecimal("12.34")), sj.read[Holder[java.math.BigDecimal]]("""{"value":12.34}""".asInstanceOf[JSON]))
    assertEquals(Holder(new java.math.BigDecimal("12.34")), sj.read[Holder[java.math.BigDecimal]]("""{"value":"12.34"}""".asInstanceOf[JSON]))
    // assertEquals(Holder[java.math.BigDecimal](null), sj.read[Holder[java.math.BigDecimal]]("""{"value":null}""".asInstanceOf[JSON]))

    val msg = """Expected a java.math.BigDecimal here
              |{"value":""}
              |----------^""".stripMargin
    interceptMessage[co.blocke.scalajack.ScalaJackError](msg){
      sj.read[Holder[java.math.BigDecimal]]("""{"value":""}""".asInstanceOf[JSON])
    }

    assertEquals("""{"value":12.34}""".asInstanceOf[JSON], sj.render(Holder(new java.math.BigDecimal("12.34"))))
    assertEquals("""{"value":null}""".asInstanceOf[JSON], sj.render(Holder[java.math.BigDecimal](null)))
  }

  test("Java Number must work") {
    assertEquals(Holder(java.lang.Double.valueOf(42.5).asInstanceOf[Number]), sj.read[Holder[java.lang.Number]]("""{"value":42.5}""".asInstanceOf[JSON]))
    assertEquals(Holder(java.lang.Double.valueOf(42.5).asInstanceOf[Number]), sj.read[Holder[java.lang.Number]]("""{"value":"42.5"}""".asInstanceOf[JSON]))
    // assertEquals(Holder[java.lang.Number](null), sj.read[Holder[java.lang.Number]]("""{"value":null}""".asInstanceOf[JSON]))

    val msg = """Expected a java.lang.Number here
              |{"value":""}
              |----------^""".stripMargin
    interceptMessage[co.blocke.scalajack.ScalaJackError](msg){
      sj.read[Holder[java.lang.Number]]("""{"value":""}""".asInstanceOf[JSON])
    }

    assertEquals("""{"value":42.5}""".asInstanceOf[JSON], sj.render(Holder(java.lang.Double.valueOf(42.5).asInstanceOf[Number])))
    assertEquals("""{"value":null}""".asInstanceOf[JSON], sj.render(Holder[java.lang.Number](null)))
  }