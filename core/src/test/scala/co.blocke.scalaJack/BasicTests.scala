package co.blocke.scalajack
package test

import org.scalatest.{ FunSpec, GivenWhenThen, BeforeAndAfterAll }
import org.scalatest.Matchers._

case class Foo(name:String, age:Int)

import scala.reflect.runtime.universe._
case class ScalaJack_Custom() extends ScalaJack {
	import formats.Custom._
	protected def _render[T]( graph:SjType, instance:T )(implicit tt:TypeTag[T], buf:StringBuilder) =
		graph match {
		  case g:SjCaseClass => g.render(instance)
		}    
}

class TestSpec extends FunSpec with GivenWhenThen with BeforeAndAfterAll {
	val sjJS  = ScalaJack(Formats.JSON)
	val sjXML = ScalaJack(Formats.XML)
	val sjC   = ScalaJack(Formats.Custom, Some(()=>ScalaJack_Custom()))

	it("Must render JSON") {
		println(sjJS.render(Foo("John",24)))
	}
	it("Must render XML") {
		println(sjXML.render(Foo("John",24)))
	}
	it("Must render custom output") {
		println(sjC.render(Foo("John",24)))		
	}
}
