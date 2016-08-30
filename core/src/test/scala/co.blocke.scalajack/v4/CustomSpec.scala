package co.blocke.scalajack
package test.v4

import json.JsonKind
import org.scalatest.{ FunSpec, GivenWhenThen, BeforeAndAfterAll }
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

object ISOTime extends ValueClassCustom {
  def read: PartialFunction[(KindMarker, _), Any] = {
    case (jk: JsonKind, js: String) => ISODateTimeFormat.dateTime().parseDateTime(js)
  }
  def render: PartialFunction[(KindMarker, _), Any] = {
    case (jk: JsonKind, iso: DateTime) => '"' + ISODateTimeFormat.dateTime().withZoneUTC().print(iso) + '"'
  }
}
class ISOTime(val dt: DateTime) extends AnyVal

object CCNum extends ValueClassCustom {
  def read: PartialFunction[(KindMarker, _), Any] = {
    case (jk: JsonKind, js: String) => js.replaceAllLiterally("-", "")
  }
  def render: PartialFunction[(KindMarker, _), Any] = {
    case (jk: JsonKind, s: String) => '"' + s.grouped(4).toList.mkString("-") + '"'
  }
}
class CCNum(val s: String) extends AnyVal
case class TestTime(period: ISOTime, cc: CCNum, num: Numa)

class Numa(val s: String) extends AnyVal

// Overriding a primitive requires assigning it to a new type, so as to differentiate it from 
// the vanilla flavor of the primitive.
object Overrides {
  type SpecialTime = DateTime
}
import Overrides._

case class Naked(name: String, when: SpecialTime, charset: Charset)
case class Bunched(name: String, group: Map[String, SpecialTime], many: List[Charset])
case class WithCustomType[R, S](r: R, s: S)

class CustomSpec extends FunSpec {
  val specialTimeHandler = CustomReadRender(
    {
      case (j: JsonKind, js: String) => ISODateTimeFormat.dateTime().parseDateTime(js)
    },
    {
      case (j: JsonKind, thing: DateTime) => '"' + ISODateTimeFormat.dateTime().withZoneUTC().print(thing) + '"'
    }
  )
  val charsetHandler = CustomReadRender(
    {
      case (j: JsonKind, js: String) => Charset.forName(js)
    },
    {
      case (j: JsonKind, thing: Charset) => '"' + thing.toString + '"'
    }
  )

  val handlerMap = Map(
    "co.blocke.scalajack.test.v4.Overrides.SpecialTime" -> specialTimeHandler,
    "java.nio.charset.Charset" -> charsetHandler
  )

  val sjJS = ScalaJack()
  val vc = VisitorContext().copy(customHandlers = handlerMap)
  val vc_nc_v = VisitorContext().copy(customHandlers = handlerMap, isCanonical = false, isValidating = true)
  val vc_c_v = VisitorContext().copy(customHandlers = handlerMap, isValidating = true)
  val vc_nc_nv = VisitorContext().copy(customHandlers = handlerMap, isCanonical = false)
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
      it("Render Tests - NCNV") {
        sjJS.render(ScalaMaster.a, vc_nc_nv) should be(JSMaster.a)
      }
      it("Read custom value class - NCNV") {
        sjJS.read[TestTime](JSMaster.a, vc_nc_nv) should be(ScalaMaster.a)
      }
      it("Render Tests - CV") {
        sjJS.render(ScalaMaster.a, vc_c_v) should be(JSMaster.a)
      }
      it("Read custom value class - CV") {
        sjJS.read[TestTime](JSMaster.a, vc_c_v) should be(ScalaMaster.a)
      }
      it("Render Tests - NCV") {
        sjJS.render(ScalaMaster.a, vc_nc_v) should be(JSMaster.a)
      }
      it("Read custom value class - NCV") {
        sjJS.read[TestTime](JSMaster.a, vc_nc_v) should be(ScalaMaster.a)
      }
    }
    describe("Java class custom rendering - Single") {
      it("Render Tests - CNV") {
        sjJS.render(ScalaMaster.b, vc) should be(JSMaster.b)
      }
      it("Read custom Java class - CNV") {
        sjJS.read[Naked](JSMaster.b, vc) should be(ScalaMaster.b)
      }
      it("Render Tests - NCNV") {
        sjJS.render(ScalaMaster.b, vc_nc_nv) should be(JSMaster.b)
      }
      it("Read custom Java class - NCNV") {
        sjJS.read[Naked](JSMaster.b, vc_nc_nv) should be(ScalaMaster.b)
      }
      it("Render Tests - CV") {
        sjJS.render(ScalaMaster.b, vc_c_v) should be(JSMaster.b)
      }
      it("Read custom Java class - CV") {
        sjJS.read[Naked](JSMaster.b, vc_c_v) should be(ScalaMaster.b)
      }
      it("Render Tests - NCV") {
        sjJS.render(ScalaMaster.b, vc_nc_v) should be(JSMaster.b)
      }
      it("Read custom Java class - NCV") {
        sjJS.read[Naked](JSMaster.b, vc_nc_v) should be(ScalaMaster.b)
      }
    }
    describe("Java class custom rendering - Collections") {
      it("Render Tests - CNV") {
        sjJS.render(ScalaMaster.c, vc) should be(JSMaster.c)
      }
      it("Read custom Java class - CNV") {
        sjJS.read[Bunched](JSMaster.c, vc) should be(ScalaMaster.c)
      }
      it("Render Tests - NCNV") {
        sjJS.render(ScalaMaster.c, vc_nc_nv) should be(JSMaster.c)
      }
      it("Read custom Java class - NCNV") {
        sjJS.read[Bunched](JSMaster.c, vc_nc_nv) should be(ScalaMaster.c)
      }
      it("Render Tests - CV") {
        sjJS.render(ScalaMaster.c, vc_c_v) should be(JSMaster.c)
      }
      it("Read custom Java class - CV") {
        sjJS.read[Bunched](JSMaster.c, vc_c_v) should be(ScalaMaster.c)
      }
      it("Render Tests - NCV") {
        sjJS.render(ScalaMaster.c, vc_nc_v) should be(JSMaster.c)
      }
      it("Read custom Java class - NCV") {
        sjJS.read[Bunched](JSMaster.c, vc_nc_v) should be(ScalaMaster.c)
      }
    }
    describe("Java class custom rendering - Custom type parameter renderings") {
      it("Render Tests - CNV") {
        sjJS.render(ScalaMaster.d, vc) should be(JSMaster.d)
      }
      it("Read custom Java class - CNV") {
        sjJS.read[WithCustomType[SpecialTime, Charset]](JSMaster.d, vc) should be(ScalaMaster.d)
      }
      it("Render Tests - NCNV") {
        sjJS.render(ScalaMaster.d, vc_nc_nv) should be(JSMaster.d)
      }
      it("Read custom Java class - NCNV") {
        sjJS.read[WithCustomType[SpecialTime, Charset]](JSMaster.d, vc_nc_nv) should be(ScalaMaster.d)
      }
      it("Render Tests - CV") {
        sjJS.render(ScalaMaster.d, vc_c_v) should be(JSMaster.d)
      }
      it("Read custom Java class - CV") {
        sjJS.read[WithCustomType[SpecialTime, Charset]](JSMaster.d, vc_c_v) should be(ScalaMaster.d)
      }
      it("Render Tests - NCV") {
        sjJS.render(ScalaMaster.d, vc_nc_v) should be(JSMaster.d)
      }
      it("Read custom Java class - NCV") {
        sjJS.read[WithCustomType[SpecialTime, Charset]](JSMaster.d, vc_nc_v) should be(ScalaMaster.d)
      }
    }
  }
}
