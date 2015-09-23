package co.blocke.scalajack
package test.v4

import org.scalatest.{ FunSpec, GivenWhenThen, BeforeAndAfterAll }
import org.scalatest.Matchers._
import scala.language.postfixOps
import scala.util.Try
import org.joda.time.{DateTime,DateTimeZone}
import org.joda.time.format.DateTimeFormat

case class Something(
	name:String,
	stuff:Map[String,Any]
	)

class AnySpec extends FunSpec {
	val sjJS  = ScalaJack()

	object JSMaster {
		val a = """{"name":"Fred","stuff":{"a":1,"b":true}}"""
		val b = """{"name":"Fred","stuff":{"a":1,"b":[4,5,6]}}"""
		val c = """{"name":"Fred","stuff":{"a":1,"b":[{"x":"Fido","y":false},{"x":"Cat","y":true}]}}"""
		val d = """{"name":"Fred","stuff":{"a":1,"b":null}}"""
		val e = """{"name":"Fred","stuff":{"a":1,"b":["foo",null,"bar"]}}"""
	}

	object ScalaMaster {
		val a = Something("Fred",Map("a"->1,"b"->true))
		val b = Something("Fred",Map("a"->1,"b"->List(4,5,6)))
		val c = Something("Fred",Map("a"->1,"b"->List(Map("x"->"Fido","y"->false),Map("x"->"Cat","y"->true))))
		val d = Something("Fred",Map("a"->1,"b"->null))
		val e = Something("Fred",Map("a"->1,"b"->List("foo",null,"bar")))
	}

	describe("===================\n| -- Any Tests -- |\n===================") {
		describe("Render Tests") {
			sjJS.render( ScalaMaster.a ) should be( JSMaster.a )
			sjJS.render( ScalaMaster.b ) should be( JSMaster.b )
			sjJS.render( ScalaMaster.c ) should be( JSMaster.c )
			sjJS.render( ScalaMaster.d ) should be( JSMaster.d )
			sjJS.render( ScalaMaster.e ) should be( JSMaster.e )
		}
		describe("Read Tests") {
			sjJS.read[Something](JSMaster.a).stuff should contain allOf (("a" -> 1), ("b" -> true))
			sjJS.read[Something](JSMaster.b).stuff should contain allOf (("a" -> 1), ("b" -> List(4,5,6)))
			val c = sjJS.read[Something](JSMaster.c).stuff.asInstanceOf[Map[String,List[Map[_,_]]]] 
			c("b")(0) should contain allOf (("x" -> "Fido"), ("y" -> false))
			c("b")(1) should contain allOf (("x" -> "Cat"), ("y" -> true))
			sjJS.read[Something](JSMaster.d).stuff should contain allOf (("a" -> 1), ("b" -> null))
			sjJS.read[Something](JSMaster.e).stuff should contain allOf (("a" -> 1), ("b" -> List("foo",null,"bar")))
		}
	}
}
