package co.blocke.scalajack
package test.v4

import org.scalatest.{ FunSpec, GivenWhenThen, BeforeAndAfterAll }
import org.scalatest.Matchers._
import scala.language.postfixOps
import scala.util.Try
import org.joda.time.{DateTime,DateTimeZone}
import org.joda.time.format.DateTimeFormat

case class Might(name:String, age:Int=50, stuff:Option[String] = None)
case class Might2(name:String, age:Int=50, stuff:Option[String] = Some("a"))
case class BadOne(name:String, others:List[Int])
case class BadOne2(name:String, others:Option[List[Int]])

class CSVSpec extends FunSpec {
	val sjCSV  = ScalaJack(CSVType())

	object CSVMaster {
		val a = """5,17,false,"hey","you",1.2,1.2,9223372036854775800,Z,null,-14,2,"1e6c2b31-4dfe-4bf6-a0a0-882caaff0e9c",520560000000"""
		val b = """Greg,5"""
		val c = """Mike,false"""
		val d = """"Fred",123,"""
		val e = """"Fred",123,Something"""
		val f = """Todd,,"""
		val g = """Todd,,"""
	}

	object ScalaMaster {
		val pattern = "MM-dd-yy"
		val dt = DateTime.parse("07-01-86", DateTimeFormat.forPattern(pattern).withZoneUTC())
		val a = All(
					5,
					new java.lang.Integer(17),
					false,
					new java.lang.String("hey"),
					"you",
					1.2 toFloat,
					1.2 toDouble,
					9223372036854775800L,
					'Z',
					null,
					-14 toByte,
					2 toShort,
					java.util.UUID.fromString("1e6c2b31-4dfe-4bf6-a0a0-882caaff0e9c"),
					dt
				)
		val b = Ex3[Int]("Greg",5)
		val c = Ex3[Boolean]("Mike",false)
		val d = OneSub1("Fred",123L,None)
		val e = OneSub1("Fred",123L,Some("Something"))
		val f = Might("Todd")
		val g = Might2("Todd")
		val h = Pristine("Sally", 25, None, Address("123 Main", 12345))
	}

	describe("===================\n| -- CSV Tests -- |\n===================") {
		// it("Render Tests") {
		// 	sjJS.render( ScalaMaster.a ) should be( JSMaster.a )
		// }
		it( "Read basic types" ) {
			sjCSV.read[All]( CSVMaster.a ) should equal( ScalaMaster.a )
		}
		it( "Handle parameterized classes" ) {
			sjCSV.read[Ex3[Int]]( CSVMaster.b ) should equal( ScalaMaster.b )
			sjCSV.read[Ex3[Boolean]]( CSVMaster.c ) should equal( ScalaMaster.c )
		}
		it( "Handles optional values" ) {
			sjCSV.read[OneSub1]( CSVMaster.d ) should equal( ScalaMaster.d )
			sjCSV.read[OneSub1]( CSVMaster.e ) should equal( ScalaMaster.e )
		}
		it( "Handles default values" ) {
			sjCSV.read[Might]( CSVMaster.f ) should equal( ScalaMaster.f )
			sjCSV.read[Might2]( CSVMaster.g ) should equal( ScalaMaster.g )
		}
		it( "Flags errors for illegal field types -- field count mismatch" ) {
			val thrown = the [co.blocke.scalajack.csv.CSVParseException] thrownBy sjCSV.read[Pristine]( "" )
			thrown.getMessage should equal ("Number of CSV fields doesn't match the number of case class fields.")
		}
		it( "Flags errors for illegal field types -- classes" ) {
			val thrown = the [co.blocke.scalajack.csv.CSVParseException] thrownBy sjCSV.read[Pristine]( "Sally,25,," )
			thrown.getMessage should equal ("Only primitive fields allowed! (No collections, classes, etc.)")
		}
		it( "Flags errors for illegal field types -- non-option collections" ) {
			val thrown = the [co.blocke.scalajack.csv.CSVParseException] thrownBy sjCSV.read[BadOne]( "Red,1" )
			thrown.getMessage should equal ("Only primitive fields allowed! (No collections, classes, etc.)")
		}
		it( "Flags errors for illegal field types -- option of illegal type" ) {
			val thrown = the [co.blocke.scalajack.csv.CSVParseException] thrownBy sjCSV.read[BadOne2]( "Red,1" )
			thrown.getMessage should equal ("Only primitive fields allowed! (No collections, classes, etc.)")
		}
	}
}
