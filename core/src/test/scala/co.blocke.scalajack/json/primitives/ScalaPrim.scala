package co.blocke.scalajack
package json.test.primitives

import org.scalatest.{ FunSpec, Matchers }
import scala.math.BigDecimal
import model._
import util.Path
import java.util.UUID

class ScalaPrim() extends FunSpec with Matchers {

  val sj = ScalaJack()

  def expectUnexpected(fn: () => Any, path: Path, related: List[String]): Boolean = {
    try {
      fn()
      throw new Exception("fn() worked--it shoudln't have!")
    } catch {
      case t: ReadUnexpectedError =>
        if (t.path != path)
          throw new Exception("Exeption path " + t.path + " didn't match expected path " + path)
        if (t.related != related)
          throw new Exception("Exeption related " + t.related.mkString("(", ",", ")") + " didn't match expected related " + related.mkString("(", ",", ")"))
      case x =>
        throw x
    }
    true
  }

  def expectInvalid(fn: () => Any, path: Path, related: List[String]): Boolean = {
    try {
      fn()
      throw new Exception("fn() worked--it shoudln't have!")
    } catch {
      case t: ReadInvalidError =>
        if (t.path != path)
          throw new Exception("Exeption path " + t.path + " didn't match expected path " + path)
        if (t.related != related) {
          println(t.msg)
          throw new Exception("Exeption related " + t.related.mkString("(", ",", ")") + " didn't match expected related " + related.mkString("(", ",", ")"))
        }
      case x =>
        throw x
    }
    true
  }

  def expectMalformed[W](fn: () => Any, path: Path, related: List[String])(implicit tt: TypeTag[W]): Boolean = {
    try {
      fn()
      throw new Exception("fn() worked--it shoudln't have!")
    } catch {
      case t: ReadMalformedError =>
        if (t.path != path)
          throw new Exception("Exeption path " + t.path + " didn't match expected path " + path)
        if (t.related != related)
          throw new Exception("Exeption related " + t.related.mkString("(", ",", ")") + " didn't match expected related " + related.mkString("(", ",", ")"))
        if (tt.tpe.typeSymbol.fullName != t.wrappedException.getClass.getCanonicalName)
          throw new Exception("Expected a wrapped exception " + tt.tpe.typeSymbol.fullName + " but found " + t.wrappedException.getClass.getCanonicalName)
      case x =>
        throw x
    }
    true
  }

  describe("----------------------------\n:  Scala Primitives Tests  :\n----------------------------") {
    describe("+++ Positive Tests +++") {
      it("BigDecimal must work") {
        val inst = SampleBigDecimal(BigDecimal(123L), BigDecimal(1.23), BigDecimal(0), BigDecimal("123.456"), BigDecimal("0.1499999999999999944488848768742172978818416595458984375"), null)
        val js = sj.render(inst)
        assertResult("""{"bd1":123,"bd2":1.23,"bd3":0,"bd4":123.456,"bd5":0.1499999999999999944488848768742172978818416595458984375,"bd6":null}""") { js }
        assertResult(inst) {
          sj.read[SampleBigDecimal](js)
        }
      }
      it("BigInt must work") {
        val inst = SampleBigInt(BigInt("-90182736451928374653345"), BigInt("90182736451928374653345"), BigInt(0), null)
        val js = sj.render(inst)
        assertResult("""{"bi1":-90182736451928374653345,"bi2":90182736451928374653345,"bi3":0,"bi4":null}""") { js }
        assertResult(inst) {
          sj.read[SampleBigInt](js)
        }
      }
      it("Boolean must work (not nullable)") {
        val inst = SampleBoolean(true, false)
        val js = sj.render(inst)
        assertResult("""{"bool1":true,"bool2":false}""") { js }
        assertResult(inst) {
          sj.read[SampleBoolean](js)
        }
      }
      it("Byte must work (not nullable)") {
        val inst = SampleByte(Byte.MaxValue, Byte.MinValue, 0, 64)
        val js = sj.render(inst)
        assertResult("""{"b1":127,"b2":-128,"b3":0,"b4":64}""") { js }
        assertResult(inst) {
          sj.read[SampleByte](js)
        }
      }
      it("Char must work (not nullable)") {
        val inst = SampleChar(Char.MaxValue, 'Z', '\u20A0')
        val js = sj.render(inst)
        assertResult("""{"c1":"\""" + """uFFFF","c2":"Z","c3":"\""" + """u20A0"}""") { js }
        assertResult(inst) {
          sj.read[SampleChar](js)
        }
      }
      it("Double must work (not nullable)") {
        val inst = SampleDouble(Double.MaxValue, Double.MinValue, 0.0, -123.4567)
        val js = sj.render(inst)
        assertResult("""{"d1":1.7976931348623157E308,"d2":-1.7976931348623157E308,"d3":0.0,"d4":-123.4567}""") { js }
        assertResult(inst) {
          sj.read[SampleDouble](js)
        }
      }
      it("Enumeration must work (not nullable)") {
        val inst = SampleEnum(Size.Small, Size.Medium, Size.Large, null, Size.Medium)
        val js = sj.render(inst)
        assertResult("""{"e1":"Small","e2":"Medium","e3":"Large","e4":null,"e5":"Medium"}""") { js }
        // mutate e5 into an ordinal...
        val js2 = js.replaceAll(""""e5":"Medium"""", """"e5":1""")
        assertResult(inst) {
          sj.read[SampleEnum](js2)
        }
      }
      it("Float must work") {
        val inst = SampleFloat(Float.MaxValue, Float.MinValue, 0.0F, -123.4567F)
        val js = sj.render(inst)
        assertResult("""{"f1":3.4028234663852886E38,"f2":-3.4028234663852886E38,"f3":0.0,"f4":-123.45670318603516}""") { js }
        assertResult(inst) {
          sj.read[SampleFloat](js)
        }
      }
      it("Int must work (not nullable)") {
        val inst = SampleInt(Int.MaxValue, Int.MinValue, 0, 123)
        val js = sj.render(inst)
        assertResult("""{"i1":2147483647,"i2":-2147483648,"i3":0,"i4":123}""") { js }
        assertResult(inst) {
          sj.read[SampleInt](js)
        }
      }
      it("Long must work (not nullable)") {
        val inst = SampleLong(Long.MaxValue, Long.MinValue, 0L, 123L)
        val js = sj.render(inst)
        assertResult("""{"l1":9223372036854775807,"l2":-9223372036854775808,"l3":0,"l4":123}""") { js }
        assertResult(inst) {
          sj.read[SampleLong](js)
        }
      }
      it("Short must work (not nullable)") {
        val inst = SampleShort(Short.MaxValue, Short.MinValue, 0, 123)
        val js = sj.render(inst)
        assertResult("""{"s1":32767,"s2":-32768,"s3":0,"s4":123}""") { js }
        assertResult(inst) {
          sj.read[SampleShort](js)
        }
      }
      it("String must work") {
        val inst = SampleString("something\b\n\f\r\t☆", "", null)
        val js = sj.render(inst)
        // The weird '+' here is to break up the unicode so it won't be interpreted and wreck the test.
        assertResult("""{"s1":"something\b\n\f\r\t\""" + """u2606","s2":"","s3":null}""") { js }
        assertResult(inst) {
          sj.read[SampleString](js)
        }
      }
      it("UUID must work") {
        val inst = SampleUUID(UUID.fromString("b81306aa-2fab-427e-9e3c-817a494ccc58"), UUID.fromString("580afe0d-81c0-458f-9e09-4486c7af0fe9"))
        val js = sj.render(inst)
        assertResult("""{"u1":"b81306aa-2fab-427e-9e3c-817a494ccc58","u2":"580afe0d-81c0-458f-9e09-4486c7af0fe9"}""") { js }
        assertResult(inst) {
          sj.read[SampleUUID](js)
        }
      }
    }
    describe("--- Negative Tests ---") {
      it("BigDecimal must break") {
        val js = """{"bd1":123,"bd2":1.23,"bd3":0,"bd4":123.456,"bd5":"0.1499999999999999944488848768742172978818416595458984375","bd6":null}"""
        assert(expectUnexpected(() => sj.read[SampleBigDecimal](js), Path.Root \ "bd5", List("String")))
      }
      it("BigInt must break") {
        val js = """{"bi1":"-90182736451928374653345","bi2":90182736451928374653345,"bi3":0,"bi4":null}"""
        assert(expectUnexpected(() => sj.read[SampleBigInt](js), Path.Root \ "bi1", List("String")))
      }
      it("Boolean must break") {
        val js = """{"bool1":true,"bool2":"false"}"""
        assert(expectUnexpected(() => sj.read[SampleBoolean](js), Path.Root \ "bool2", List("String")))
        val js2 = """{"bool1":true,"bool2":123}"""
        assert(expectUnexpected(() => sj.read[SampleBoolean](js2), Path.Root \ "bool2", List("Number")))
        val js3 = """{"bool1":true,"bool2":null}"""
        assert(expectUnexpected(() => sj.read[SampleBoolean](js3), Path.Root \ "bool2", List("Null")))
      }
      it("Byte must break") {
        val js = """{"b1":true,"b2":-128,"b3":0,"b4":64}"""
        assert(expectUnexpected(() => sj.read[SampleByte](js), Path.Root \ "b1", List("True")))
        val js2 = """{"b1":12,"b2":-128,"b3":0,"b4":null}"""
        assert(expectUnexpected(() => sj.read[SampleByte](js2), Path.Root \ "b4", List("Null")))
      }
      it("Char must break") {
        val js = """{"c1":null,"c2":"Y","c3":"Z"}"""
        assert(expectInvalid(() => sj.read[SampleChar](js), Path.Root \ "c1", List("Null")))
        val js2 = """{"c1":"","c2":"Y","c3":"Z"}"""
        assert(expectInvalid(() => sj.read[SampleChar](js2), Path.Root \ "c1", List("Empty String")))
      }
      it("Double must break") {
        val js = """{"d1":1.79769313486E23157E308,"d2":-1.7976931348623157E308,"d3":0.0,"d4":-123.4567}"""
        assert(expectMalformed[NumberFormatException](() => sj.read[SampleDouble](js), Path.Root \ "d1", List.empty[String]))
      }
      it("Enumeration must break") {
        val js = """{"e1":"Small","e2":"Bogus","e3":"Large","e4":null,"e5":"Medium"}"""
        assert(expectInvalid(() => sj.read[SampleEnum](js), Path.Root \ "e2", List("co.blocke.scalajack.json.test.primitives.Size$", "Bogus")))
        val js2 = """{"e1":"Small","e2":"Medium","e3":"Large","e4":null,"e5":9}"""
        assert(expectInvalid(() => sj.read[SampleEnum](js2), Path.Root \ "e5", List("co.blocke.scalajack.json.test.primitives.Size$", "9")))
      }
      it("Float must break") {
        val js = """{"f1":3.4028235E38,"f2":"-3.4028235E38","f3":0.0,"f4":-123.4567}"""
        assert(expectUnexpected(() => sj.read[SampleFloat](js), Path.Root \ "f2", List("String")))
      }
      it("Int must break") {
        val js = """{"i1":2147483647,"i2":-2147483648,"i3":"0","i4":123}"""
        assert(expectUnexpected(() => sj.read[SampleInt](js), Path.Root \ "i3", List("String")))
        val js2 = """{"i1":2147483647,"i2":-2147483648,"i3":2.3,"i4":123}"""
        assert(expectMalformed[NumberFormatException](() => sj.read[SampleInt](js2), Path.Root \ "i3", List.empty[String]))
      }
      it("Long must break") {
        val js = """{"l1":9223372036854775807,"l2":-9223372036854775808,"l3":true,"l4":123}"""
        assert(expectUnexpected(() => sj.read[SampleLong](js), Path.Root \ "l3", List("True")))
        val js2 = """{"l1":9223372036854775807,"l2":-9223372036854775808,"l3":0.3,"l4":123}"""
        assert(expectMalformed[NumberFormatException](() => sj.read[SampleLong](js2), Path.Root \ "l3", List.empty[String]))
      }
      it("Short must break") {
        val js = """{"s1":32767,"s2":true,"s3":0,"s4":123}"""
        assert(expectUnexpected(() => sj.read[SampleShort](js), Path.Root \ "s2", List("True")))
        val js2 = """{"s1":32767,"s2":3.4,"s3":0,"s4":123}"""
        assert(expectMalformed[NumberFormatException](() => sj.read[SampleShort](js2), Path.Root \ "s2", List.empty[String]))
      }
      it("String must break") {
        val js = """{"s1":"something","s2":-19,"s3":null}"""
        assert(expectUnexpected(() => sj.read[SampleString](js), Path.Root \ "s2", List("Number")))
      }
      it("UUID must break") {
        val js = """{"u1":"bogus","u2":"580afe0d-81c0-458f-9e09-4486c7af0fe9"}"""
        assert(expectMalformed[IllegalArgumentException](() => sj.read[SampleUUID](js), Path.Root \ "u1", List("bogus")))
      }
      it("Can't find TypeAdapter for given type") {
        val js = """{"hey":"you"}"""
        val msg = "Unable to find a type adapter for Process"
        the[java.lang.IllegalArgumentException] thrownBy sj.read[java.lang.Process](js) should have message msg
      }
    }
  }
}
