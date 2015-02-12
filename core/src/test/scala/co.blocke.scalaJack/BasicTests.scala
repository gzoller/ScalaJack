package co.blocke.scalajack
package test

import org.scalatest.{ FunSpec, GivenWhenThen, BeforeAndAfterAll }
import org.scalatest.Matchers._
import scala.language.postfixOps

case class Foo(
	name:String, 
	age:Int)

case class All(
	a:	Int,
	b:	java.lang.Integer,
	c:	Boolean,
	d:	java.lang.String,
	e:	String,
	f:	Float,
	g:	Double,
	h:	Long,
	i:	Char,
	j:	Null,
	k:	Byte,
	l:	Short
	)
case class AllColl(
	a: List[Int],
	b: List[Foo],
	c: Option[Int],
	d: Option[String]
	)

import scala.reflect.runtime.universe._
case class ScalaJack_Custom() extends ScalaJack {
	import formats.Custom._
	protected def _render[T]( graph:SjType, instance:T, buf:StringBuilder )(implicit tt:TypeTag[T]) = renderFarm(graph, instance, buf)
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
	it("Must render all primitives") {
		sjJS.render(All(
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
			2 toShort
		)) should equal("""{"a":5,"b":17,"c":false,"d":"hey","e":"you","f":1.2,"g":1.2,"h":9223372036854775800,"i":"Z","j":null,"k":-14,"l":2}""")
	}
	it("Must render all collections (non-nested)") {
		println(sjJS.render(AllColl(
			List(1,2),
			List(Foo("one",1),Foo("two",2)),
			None,
			Some("Me")
			)))
	}
}
