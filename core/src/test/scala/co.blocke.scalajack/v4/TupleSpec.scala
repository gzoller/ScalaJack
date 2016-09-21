package co.blocke.scalajack
package test.v4

import org.scalatest.{ FunSpec, GivenWhenThen, BeforeAndAfterAll }
import org.scalatest.Matchers._
import scala.language.postfixOps
import scala.util.Try
import org.joda.time.{ DateTime, DateTimeZone }
import org.joda.time.format.DateTimeFormat

case class Hip(name: String, avail: (Int, Boolean))
case class Hop(name: String, avail: Boolean)

class TupleSpec extends FunSpec {
  val sjJS = ScalaJack()
  /*
  val vc_nc_v = VisitorContext().copy(isCanonical = false, isValidating = true)
  val vc_c_v = VisitorContext().copy(isValidating = true)
  val vc_nc_nv = VisitorContext().copy(isCanonical = false)
  // No vc = c_nv canonical (c) and non-validating (nv)
  */

  type tupA = (String, String)
  type tupB = List[(String, Int)]
  type tupC = Map[String, List[(String, Int)]]
  type tupD = List[(String, Int, Boolean)]
  type tupE = (String, Hop, Int)

  object JSMaster {
    val a = """["Fred","Mary"]"""
    val b = """[["Fred",5],["George",6]]"""
    val c = """{"a":[["Fred",5],["George",6]],"b":[["Mary",1],["Linda",2]]}"""
    val d = """[["Fred",5,false],["George",6,true]]"""
    val e = """["Fred",{"name":"Mike","avail":false},5]"""
    val f = """{"name":"Greg","avail":[5,true]}"""
  }

  object ScalaMaster {
    val a: tupA = ("Fred", "Mary")
    val b: tupB = List(("Fred", 5), ("George", 6))
    val c: tupC = Map("a" -> List(("Fred", 5), ("George", 6)), "b" -> List(("Mary", 1), ("Linda", 2)))
    val d: tupD = List(("Fred", 5, false), ("George", 6, true))
    val e: tupE = ("Fred", Hop("Mike", false), 5)
    val f = Hip("Greg", (5, true))
  }

  describe("=====================\n| -- Tuple Tests -- |\n=====================") {
    it("Render Tests - CNV") {
      sjJS.render(ScalaMaster.a) should be(JSMaster.a)
      sjJS.render(ScalaMaster.b) should be(JSMaster.b)
      sjJS.render(ScalaMaster.c) should be(JSMaster.c)
      sjJS.render(ScalaMaster.d) should be(JSMaster.d)
      sjJS.render(ScalaMaster.e) should be(JSMaster.e)
      sjJS.render(ScalaMaster.f) should be(JSMaster.f)
    }
    it("Read tuples - CNV") {
      sjJS.read[tupA](JSMaster.a) should be(ScalaMaster.a)
      sjJS.read[tupB](JSMaster.b) should be(ScalaMaster.b)
      sjJS.read[tupC](JSMaster.c) should be(ScalaMaster.c)
      sjJS.read[tupD](JSMaster.d) should be(ScalaMaster.d)
      sjJS.read[tupE](JSMaster.e) should be(ScalaMaster.e)
      sjJS.read[Hip](JSMaster.f) should be(ScalaMaster.f)
    }
    /*
    it("Render Tests - NCNV") {
      sjJS.render(ScalaMaster.a, vc_nc_v) should be(JSMaster.a)
      sjJS.render(ScalaMaster.b, vc_nc_v) should be(JSMaster.b)
      sjJS.render(ScalaMaster.c, vc_nc_v) should be(JSMaster.c)
      sjJS.render(ScalaMaster.d, vc_nc_v) should be(JSMaster.d)
      sjJS.render(ScalaMaster.e, vc_nc_v) should be(JSMaster.e)
      sjJS.render(ScalaMaster.f, vc_nc_v) should be(JSMaster.f)
    }
    it("Read tuples - NCNV") {
      sjJS.read[tupA](JSMaster.a, vc_nc_v) should be(ScalaMaster.a)
      sjJS.read[tupB](JSMaster.b, vc_nc_v) should be(ScalaMaster.b)
      sjJS.read[tupC](JSMaster.c, vc_nc_v) should be(ScalaMaster.c)
      sjJS.read[tupD](JSMaster.d, vc_nc_v) should be(ScalaMaster.d)
      sjJS.read[tupE](JSMaster.e, vc_nc_v) should be(ScalaMaster.e)
      sjJS.read[Hip](JSMaster.f, vc_nc_v) should be(ScalaMaster.f)
    }
    it("Render Tests - CV") {
      sjJS.render(ScalaMaster.a, vc_c_v) should be(JSMaster.a)
      sjJS.render(ScalaMaster.b, vc_c_v) should be(JSMaster.b)
      sjJS.render(ScalaMaster.c, vc_c_v) should be(JSMaster.c)
      sjJS.render(ScalaMaster.d, vc_c_v) should be(JSMaster.d)
      sjJS.render(ScalaMaster.e, vc_c_v) should be(JSMaster.e)
      sjJS.render(ScalaMaster.f, vc_c_v) should be(JSMaster.f)
    }
    it("Read tuples - CV") {
      sjJS.read[tupA](JSMaster.a, vc_c_v) should be(ScalaMaster.a)
      sjJS.read[tupB](JSMaster.b, vc_c_v) should be(ScalaMaster.b)
      sjJS.read[tupC](JSMaster.c, vc_c_v) should be(ScalaMaster.c)
      sjJS.read[tupD](JSMaster.d, vc_c_v) should be(ScalaMaster.d)
      sjJS.read[tupE](JSMaster.e, vc_c_v) should be(ScalaMaster.e)
      sjJS.read[Hip](JSMaster.f, vc_c_v) should be(ScalaMaster.f)
    }
    it("Render Tests - NCV") {
      sjJS.render(ScalaMaster.a, vc_nc_v) should be(JSMaster.a)
      sjJS.render(ScalaMaster.b, vc_nc_v) should be(JSMaster.b)
      sjJS.render(ScalaMaster.c, vc_nc_v) should be(JSMaster.c)
      sjJS.render(ScalaMaster.d, vc_nc_v) should be(JSMaster.d)
      sjJS.render(ScalaMaster.e, vc_nc_v) should be(JSMaster.e)
      sjJS.render(ScalaMaster.f, vc_nc_v) should be(JSMaster.f)
    }
    it("Read tuples - NCV") {
      sjJS.read[tupA](JSMaster.a, vc_nc_v) should be(ScalaMaster.a)
      sjJS.read[tupB](JSMaster.b, vc_nc_v) should be(ScalaMaster.b)
      sjJS.read[tupC](JSMaster.c, vc_nc_v) should be(ScalaMaster.c)
      sjJS.read[tupD](JSMaster.d, vc_nc_v) should be(ScalaMaster.d)
      sjJS.read[tupE](JSMaster.e, vc_nc_v) should be(ScalaMaster.e)
      sjJS.read[Hip](JSMaster.f, vc_nc_v) should be(ScalaMaster.f)
    }
    */
  }
}
