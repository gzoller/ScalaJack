package co.blocke.scalajack
package json.test.noncanonical

import org.scalatest.{ FunSpec, Matchers }

class ScalaPrimKeys() extends FunSpec with Matchers {

  val sj = ScalaJack().isCanonical(false)

  describe("----------------------------------------\n:  Scala Primitive Noncanonical Tests  :\n----------------------------------------") {
    describe("+++ Positive Tests +++") {
      it("With Any Key") {
        val inst = AnyShell(Map(Size.Small -> "ok", 123.456 -> true, 293845 -> "Greg", false -> "16", "Fred" -> "Wilma", 16.toByte -> null))
        val js = sj.render(inst)
        assertResult("""{"m":{false:"16","Small":"ok",123.456:true,"Fred":"Wilma",293845:"Greg",16:null}}""") { js }
        val read = sj.read[AnyShell](js)
        assertResult("""List((Small,java.lang.String), (Fred,java.lang.String), (16,java.lang.Long), (293845,java.lang.Long), (123.456,java.lang.Double), (false,java.lang.Boolean))""") {
          read.m.keySet.map(z => (z, z.getClass.getName)).toList.sortWith((a, b) => a._2 > b._2).toString
        }
      }
      it("With BigDecimal Key") {
        val inst = SampleBigDecimal(Map(BigDecimal(123.456) -> BigDecimal(1), BigDecimal(789.123) -> BigDecimal(2)))
        val js = sj.render(inst)
        assertResult("""{"m":{123.456:1,789.123:2}}""") { js }
        assertResult(inst) {
          sj.read[SampleBigDecimal](js)
        }
      }
      it("With BigInt Key") {
        val inst = SampleBigInt(Map(BigInt(123) -> BigInt(1), BigInt(789) -> BigInt(2)))
        val js = sj.render(inst)
        assertResult("""{"m":{123:1,789:2}}""") { js }
        assertResult(inst) {
          sj.read[SampleBigInt](js)
        }
      }
      it("With Boolean Key") {
        val inst = SampleBoolean(Map(true -> false, false -> true))
        val js = sj.render(inst)
        assertResult("""{"m":{true:false,false:true}}""") { js }
        assertResult(inst) {
          sj.read[SampleBoolean](js)
        }
      }
      it("With Byte Key") {
        val inst = SampleByte(Map(16.toByte -> 2.toByte, 48.toByte -> 9.toByte))
        val js = sj.render(inst)
        assertResult("""{"m":{16:2,48:9}}""") { js }
        assertResult(inst) {
          sj.read[SampleByte](js)
        }
      }
      it("With Char Key") {
        val inst = SampleChar(Map('a' -> 'A', 'z' -> 'Z'))
        val js = sj.render(inst)
        assertResult("""{"m":{"a":"A","z":"Z"}}""") { js }
        assertResult(inst) {
          sj.read[SampleChar](js)
        }
      }
      it("With Double Key") {
        val inst = SampleDouble(Map(12.34 -> 56.78, 90.12 -> 34.56))
        val js = sj.render(inst)
        assertResult("""{"m":{12.34:56.78,90.12:34.56}}""") { js }
        assertResult(inst) {
          sj.read[SampleDouble](js)
        }
      }
      it("With Enumeration Key") {
        val inst = SampleEnumeration(Map(Size.Small -> Size.Large, Size.Large -> Size.Medium))
        val js = sj.render(inst)
        assertResult("""{"m":{"Small":"Large","Large":"Medium"}}""") { js }
        assertResult(inst) {
          sj.read[SampleEnumeration](js)
        }
      }
      it("With Float Key") {
        val inst = SampleFloat(Map(12.34F -> 56.78F, 90.12F -> 34.56F))
        val js = sj.render(inst)
        assertResult("""{"m":{12.34:56.78,90.12:34.56}}""") { js }
        assertResult(inst) {
          sj.read[SampleFloat](js)
        }
      }
      it("With Int Key") {
        val inst = SampleInt(Map(12 -> 56, 90 -> 34))
        val js = sj.render(inst)
        assertResult("""{"m":{12:56,90:34}}""") { js }
        assertResult(inst) {
          sj.read[SampleInt](js)
        }
      }
      it("With Long Key") {
        val inst = SampleLong(Map(12L -> 56L, 90L -> 34L))
        val js = sj.render(inst)
        assertResult("""{"m":{12:56,90:34}}""") { js }
        assertResult(inst) {
          sj.read[SampleLong](js)
        }
      }
      it("With Short Key") {
        val inst = SampleShort(Map(12.toShort -> 56.toShort, 90.toShort -> 34.toShort))
        val js = sj.render(inst)
        assertResult("""{"m":{12:56,90:34}}""") { js }
        assertResult(inst) {
          sj.read[SampleShort](js)
        }
      }
    }
    describe("--- Negative Tests ---") {
      it("Bad BigDecimal Key") {
        val js = """{"m":{789.123:1,"fred":2}}"""
        val msg = """DeserializationException(1 error):
                    |  [$.m.fred] Expected a JSON number, not JString(fred) (reported by: co.blocke.scalajack.typeadapter.BigDecimalDeserializer)""".stripMargin
        the[DeserializationException] thrownBy sj.read[SampleBigDecimal](js) should have message msg
      }
      it("Bad BigInt Key") {
        val js = """{"m":{"fred":1,789:2}}"""
        val msg = """DeserializationException(1 error):
                    |  [$.m.fred] Expected a JSON number (integer value) (reported by: co.blocke.scalajack.typeadapter.BigIntDeserializer)""".stripMargin
        the[DeserializationException] thrownBy sj.read[SampleBigInt](js) should have message msg
      }
      it("Bad Boolean Key") {
        val js = """{"m":{true:false,123:true}}"""
        val msg = """DeserializationException(1 error):
                    |  [$.m.123] Expected a JSON boolean (reported by: co.blocke.scalajack.typeadapter.BooleanDeserializer)""".stripMargin
        the[DeserializationException] thrownBy sj.read[SampleBoolean](js) should have message msg
      }
      it("Bad Byte Key") {
        val js = """{"m":{16:2,"x48":9}}"""
        val msg = """DeserializationException(1 error):
                    |  [$.m.x48] Expected a JSON number (byte) (reported by: co.blocke.scalajack.typeadapter.ByteDeserializer)""".stripMargin
        the[DeserializationException] thrownBy sj.read[SampleByte](js) should have message msg
      }
      it("Bad Char Key") { // NOTE: This comprehensively tests for any null keyed Map
        val js = """{"m":{null:"A","z":"Z"}}"""
        val msg = """DeserializationException(1 error):
                    |  [$.m.null] Expected a char (JSON string of length 1), not null (reported by: co.blocke.scalajack.typeadapter.CharDeserializer)""".stripMargin
        the[DeserializationException] thrownBy sj.read[SampleChar](js) should have message msg
      }
      it("Bad Double Key") {
        val js = """{"m":{12.34:56.78,true:34.56}}"""
        val msg = """DeserializationException(1 error):
                    |  [$.m.true] Expected a JSON number, not JBool(true) (reported by: co.blocke.scalajack.typeadapter.DoubleDeserializer)""".stripMargin
        the[DeserializationException] thrownBy sj.read[SampleDouble](js) should have message msg
      }
      it("Bad Enumeration Key") {
        val js = """{"m":{"Small":"Large","Bogus":"Medium"}}"""
        val msg = """DeserializationException(1 error):
                    |  [$.m.Bogus] Enumeration co.blocke.scalajack.json.test.noncanonical.Size$ does not contain a value named Bogus (reported by: co.blocke.scalajack.typeadapter.EnumerationValueDeserializer)""".stripMargin
        the[DeserializationException] thrownBy sj.read[SampleEnumeration](js) should have message msg
      }
      it("Bad Float Key") {
        val js = """{"m":{12.34:56.78,90.12.3:34.56}}"""
        val msg = """DeserializationException(1 error):
                    |  [???] Exception was thrown: java.lang.NumberFormatException (reported by: unknown)""".stripMargin
        the[DeserializationException] thrownBy sj.read[SampleFloat](js) should have message msg
      }
      it("Bad Int Key") {
        val js = """{"m":{12.0:56,90:34}}"""
        val msg = """DeserializationException(1 error):
                    |  [$.m.12.0] Expected a JSON int, not JDecimal(12.0) (reported by: co.blocke.scalajack.typeadapter.IntDeserializer)""".stripMargin
        the[DeserializationException] thrownBy sj.read[SampleInt](js) should have message msg
      }
      it("Bad Long Key") {
        val js = """{"m":{12:56,"hey":34}}"""
        val msg = """DeserializationException(1 error):
                    |  [$.m.hey] Expected a JSON number (long) (reported by: co.blocke.scalajack.typeadapter.LongDeserializer)""".stripMargin
        the[DeserializationException] thrownBy sj.read[SampleLong](js) should have message msg
      }
      it("Bad Short Key") {
        val js = """{"m":{99999:56,90:34}}"""
        val msg = """DeserializationException(1 error):
                    |  [$.m.99999] Short value out of range (reported by: co.blocke.scalajack.typeadapter.ShortDeserializer)""".stripMargin
        the[DeserializationException] thrownBy sj.read[SampleShort](js) should have message msg
      }
    }
  }
}
