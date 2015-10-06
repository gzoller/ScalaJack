package co.blocke.scalajack
package test.v4

import org.scalatest.{ FunSpec, GivenWhenThen, BeforeAndAfterAll }
import org.scalatest.Matchers._
import scala.language.postfixOps
import scala.util.Try
import org.joda.time.{DateTime,DateTimeZone}
import org.joda.time.format.DateTimeFormat

case class Hip(name:String, avail:(Int,Boolean))
case class Hop(name:String, avail:Boolean)

class TupleSpec extends FunSpec {
	val sjJS  = ScalaJack()

	type tupA = (String,String)
	type tupB = List[(String,Int)]
	type tupC = Map[String,List[(String,Int)]]
	type tupD = List[(String,Int,Boolean)]
	type tupE = (String,Hop,Int)

	object JSMaster {
		val a = """["Fred","Mary"]"""
		val b = """[["Fred",5],["George",6]]"""
		val c = """{"a":[["Fred",5],["George",6]],"b":[["Mary",1],["Linda",2]]}"""
		val d = """[["Fred",5,false],["George",6,true]]"""
		val e = """["Fred",{"name":"Mike","avail":false},5]"""
		val f = """{"name":"Greg","avail":[5,true]}"""
	}

	object ScalaMaster {
		val a : tupA = ("Fred","Mary")
		val b : tupB = List(("Fred",5),("George",6))
		val c : tupC = Map("a"->List(("Fred",5),("George",6)),"b"->List(("Mary",1),("Linda",2)))
		val d : tupD = List(("Fred",5,false),("George",6,true))
		val e : tupE = ("Fred",Hop("Mike",false),5)
		val f        = Hip("Greg",(5,true))
	}

	describe("===================\n| -- Any Tests -- |\n===================") {
		describe("Render Tests") {
			it( "Naked tuples" ) {
				sjJS.render( ScalaMaster.a ) should be( JSMaster.a )
				sjJS.render( ScalaMaster.b ) should be( JSMaster.b )
				sjJS.render( ScalaMaster.c ) should be( JSMaster.c )
				sjJS.render( ScalaMaster.d ) should be( JSMaster.d )
				sjJS.render( ScalaMaster.e ) should be( JSMaster.e )
				sjJS.render( ScalaMaster.f ) should be( JSMaster.f )
			}
		}
		describe("Read Tests") {
			it( "Naked tuples" ) {
				sjJS.read[tupA]( JSMaster.a ) should be( ScalaMaster.a )
				sjJS.read[tupB]( JSMaster.b ) should be( ScalaMaster.b )
				sjJS.read[tupC]( JSMaster.c ) should be( ScalaMaster.c )
				sjJS.read[tupD]( JSMaster.d ) should be( ScalaMaster.d )
				sjJS.read[tupE]( JSMaster.e ) should be( ScalaMaster.e )
				sjJS.read[Hip] ( JSMaster.f ) should be( ScalaMaster.f )
			}
		}
	}
}
