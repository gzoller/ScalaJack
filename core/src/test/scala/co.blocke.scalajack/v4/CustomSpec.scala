package co.blocke.scalajack
package test.v4

import json.JsonKind
import org.scalatest.{ FunSpec, GivenWhenThen, BeforeAndAfterAll }
import org.scalatest.Matchers._
import scala.language.postfixOps
import scala.util.Try
import org.joda.time.{DateTime,DateTimeZone}
import org.joda.time.format._

object ISOTime extends ValueClassCustom {
	def read:PartialFunction[(KindMarker,_), Any] = {
	  case (jk:JsonKind,js:String) => ISODateTimeFormat.dateTime().parseDateTime(js)
	}
	def render:PartialFunction[(KindMarker,_), Any] = {
	  case (jk:JsonKind,iso:DateTime) => '"'+ISODateTimeFormat.dateTime().withZoneUTC().print(iso)+'"'
	}
}
class ISOTime( val dt:DateTime ) extends AnyVal

object CCNum extends ValueClassCustom {
	def read:PartialFunction[(KindMarker,_), Any] = {
	  case (jk:JsonKind,js:String) => js.replaceAllLiterally("-","")
	}
	def render:PartialFunction[(KindMarker,_), Any] = {
	  case (jk:JsonKind,s:String) => '"'+s.grouped(4).toList.mkString("-")+'"'
	}
}
class CCNum( val s:String ) extends AnyVal
case class TestTime( period:ISOTime, cc:CCNum, num:Numa )

class Numa( val s:String ) extends AnyVal

class CustomSpec extends FunSpec {
	val sjJS  = ScalaJack()
	val vc_nc_v  = VisitorContext().copy(isCanonical = false, isValidating = true) 
	val vc_c_v   = VisitorContext().copy(isValidating = true) 
	val vc_nc_nv = VisitorContext().copy(isCanonical = false) 
	// No vc = c_nv canonical (c) and non-validating (nv)

	object JSMaster {
		val a = """{"period":"2015-09-18T04:00:00.000Z","cc":"1234-5678-9012-3456","num":"123"}"""
	}

	object ScalaMaster {
		val a = TestTime(new ISOTime( ISODateTimeFormat.dateTime().parseDateTime("2015-09-18T04:00:00.000Z") ), new CCNum("1234567890123456"), new Numa("123"))
	}

	describe("=========================\n| -- Custom VC Tests -- |\n=========================") {
		it("Render Tests - CNV") {
			sjJS.render( ScalaMaster.a ) should be( JSMaster.a )
		}
		it( "Read custom value class - CNV" ) {
			sjJS.read[TestTime]( JSMaster.a ) should be( ScalaMaster.a )
		}
		it("Render Tests - NCNV") {
			sjJS.render( ScalaMaster.a,vc_nc_nv ) should be( JSMaster.a )
		}
		it( "Read custom value class - NCNV" ) {
			sjJS.read[TestTime]( JSMaster.a, vc_nc_nv ) should be( ScalaMaster.a )
		}
		it("Render Tests - CV") {
			sjJS.render( ScalaMaster.a,vc_c_v ) should be( JSMaster.a )
		}
		it( "Read custom value class - CV" ) {
			sjJS.read[TestTime]( JSMaster.a, vc_c_v ) should be( ScalaMaster.a )
		}
		it("Render Tests - NCV") {
			sjJS.render( ScalaMaster.a,vc_nc_v ) should be( JSMaster.a )
		}
		it( "Read custom value class - NCV" ) {
			sjJS.read[TestTime]( JSMaster.a, vc_nc_v ) should be( ScalaMaster.a )
		}
	}
}
