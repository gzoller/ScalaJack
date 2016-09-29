package co.blocke.scalajack
package test.v4

import json._
import TokenType._
import typeadapter.{ BasicTypeAdapter, SimpleTypeAdapter }
import org.scalatest.{ BeforeAndAfterAll, FunSpec, GivenWhenThen }
import org.scalatest.Matchers._

import scala.language.postfixOps
import scala.util.Try
import org.joda.time.{ DateTime, DateTimeZone }
import org.joda.time.format._
import java.nio.charset.Charset

/*
 * Tests to exercise custom rendering for both value classes (via companion objects)
 * and for Java classes (via VisitorContext registration of CustomReadRender handlers).
 */

// Overriding a primitive requires assigning it to a new type, so as to differentiate it from 
// the vanilla flavor of the primitive.
object Overrides {
  type SpecialTime = DateTime
  type CCNumStr = String
}
import Overrides._

object ISOTimeAdapter extends BasicTypeAdapter[SpecialTime] {
  override def read(reader: Reader): SpecialTime = {
    reader.peek match {
      case TokenType.String ⇒
        ISODateTimeFormat.dateTime().parseDateTime(reader.readString())
      case TokenType.Null ⇒
        reader.readNull()
    }
  }

  override def write(value: SpecialTime, writer: Writer): Unit =
    if (value == null) {
      writer.writeNull()
    } else {
      writer.writeString(ISODateTimeFormat.dateTime().withZoneUTC().print(value))
    }
}

// object ISOTime extends ValueClassCustom {
//   def read: PartialFunction[(KindMarker, _), Any] = {
//     case (jk: JsonKind, js: String) => ISODateTimeFormat.dateTime().parseDateTime(js)
//   }
//   def render: PartialFunction[(KindMarker, _), Any] = {
//     case (jk: JsonKind, iso: DateTime) => '"' + ISODateTimeFormat.dateTime().withZoneUTC().print(iso) + '"'
//   }
// }
class ISOTime(val dt: SpecialTime) extends AnyVal

object CCNumTypeAdapter extends BasicTypeAdapter[CCNumStr] {
  override def read(reader: Reader): CCNumStr = {
    reader.peek match {
      case TokenType.String ⇒
        reader.readString().replaceAllLiterally("-", "")
      case TokenType.Null ⇒
        reader.readNull()
    }
  }

  override def write(value: CCNumStr, writer: Writer): Unit =
    if (value == null) {
      writer.writeNull()
    } else {
      writer.writeString(value.grouped(4).toList.mkString("-"))
    }
}

class CCNum(val s: CCNumStr) extends AnyVal
case class TestTime(period: ISOTime, cc: CCNum, num: Numa)

class Numa(val s: String) extends AnyVal

case class Naked(name: String, when: SpecialTime, charset: Charset)
case class Bunched(name: String, group: Map[String, SpecialTime], many: List[Charset])
case class WithCustomType[R, S](r: R, s: S)

object CharsetTypeAdapter extends SimpleTypeAdapter.ForTypeSymbolOf[Charset] {
  override def read(reader: Reader): Charset = {
    reader.peek match {
      case TokenType.String ⇒
        Charset.forName(reader.readString())
      case TokenType.Null ⇒
        reader.readNull()
    }
  }

  override def write(value: Charset, writer: Writer): Unit =
    if (value == null) {
      writer.writeNull()
    } else {
      writer.writeString(value.toString)
    }
}

class CustomSpec extends FunSpec {
  // val handlerMap = Map(
  //   "co.blocke.scalajack.test.v4.Overrides.SpecialTime" -> specialTimeHandler,
  //   "java.nio.charset.Charset" -> charsetHandler
  // )

  val sjJS = ScalaJack().withAdapters(CharsetTypeAdapter, CCNumTypeAdapter, ISOTimeAdapter)
  // val vc_nc_v = VisitorContext().copy(customHandlers = handlerMap, isCanonical = false, isValidating = true)
  // val vc_c_v = VisitorContext().copy(customHandlers = handlerMap, isValidating = true)
  // val vc_nc_nv = VisitorContext().copy(customHandlers = handlerMap, isCanonical = false)
  // No vc = c_nv canonical (c) and non-validating (nv)

  object JSMaster {
    val a = """{"period":"2015-09-18T04:00:00.000Z","cc":"1234-5678-9012-3456","num":"123"}"""
    val b = """{"name":"wow","when":"2015-09-18T04:00:00.000Z","charset":"UTF-8"}"""
    val c = """{"name":"eek","group":{"a":"2015-09-18T04:00:00.000Z","b":"2015-09-18T04:00:00.000Z"},"many":["UTF-8","UTF-8"]}"""
    val d = """{"r":"2015-09-18T04:00:00.000Z","s":"UTF-8"}"""
  }

  val theTime: SpecialTime = ISODateTimeFormat.dateTime().parseDateTime("2015-09-18T04:00:00.000Z")

  object ScalaMaster {
    val a = TestTime(new ISOTime(theTime), new CCNum("1234567890123456"), new Numa("123"))
    val b = Naked("wow", theTime, Charset.defaultCharset)
    val c = Bunched("eek", Map("a" -> theTime, "b" -> theTime), List(Charset.defaultCharset, Charset.defaultCharset))
    val d = WithCustomType(theTime, Charset.defaultCharset)
  }

  describe("=========================\n| -- Custom VC Tests -- |\n=========================") {
    describe("Value class custom rendering") {
      it("Render Tests - CNV") {
        sjJS.render(ScalaMaster.a) should be(JSMaster.a)
      }
      it("Read custom value class - CNV") {
        sjJS.read[TestTime](JSMaster.a) should be(ScalaMaster.a)
      }
    }
    describe("Java class custom rendering - Single") {
      it("Render Tests - CNV") {
        sjJS.render(ScalaMaster.b) should be(JSMaster.b)
      }
      it("Read custom Java class - CNV") {
        sjJS.read[Naked](JSMaster.b) should be(ScalaMaster.b)
      }
    }
    describe("Java class custom rendering - Collections") {
      it("Render Tests - CNV") {
        sjJS.render(ScalaMaster.c) should be(JSMaster.c)
      }
      it("Read custom Java class - CNV") {
        sjJS.read[Bunched](JSMaster.c) should be(ScalaMaster.c)
      }
    }
    describe("Java class custom rendering - Custom type parameter renderings") {
      it("Render Tests - CNV") {
        sjJS.render(ScalaMaster.d) should be(JSMaster.d)
      }
      it("Read custom Java class - CNV") {
        sjJS.read[WithCustomType[SpecialTime, Charset]](JSMaster.d) should be(ScalaMaster.d)
      }
    }
  }
}
