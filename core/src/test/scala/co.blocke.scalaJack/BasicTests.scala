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
// case class ScalaJack_Custom() extends ScalaJack {
// 	import formats.Custom._
// 	protected def _render[T]( graph:SjType, instance:T, buf:StringBuilder )(implicit tt:TypeTag[T]) = renderFarm(graph, instance, buf)
// }

class TestSpec extends FunSpec with GivenWhenThen with BeforeAndAfterAll {
	val sjJS  = ScalaJack(Formats.JSON)
	val sjXML = ScalaJack(Formats.XML)
	// val sjC   = ScalaJack(Formats.Custom, Some(()=>ScalaJack_Custom()))

	it("Must render JSON and XML") {
		sjJS.render(Foo("John",24)) should equal("""{"name":"John","age":24}""")
		sjXML.render(Foo("John",24)) should equal("""<class type="co.blocke.scalajack.test.Foo"><field name="name">John</field><field name="age">24</field></class>""")
	}
	// it("Must render custom output") {
	// 	println(sjC.render(Foo("John",24)))		
	// }
	it("Must render all primitives") {
		val all = All(
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
		)
		sjJS.render(all) should equal("""{"a":5,"b":17,"c":false,"d":"hey","e":"you","f":1.2,"g":1.2,"h":9223372036854775800,"i":"Z","j":null,"k":-14,"l":2}""")
		sjXML.render(all) should equal("""<class type="co.blocke.scalajack.test.All"><field name="a">5</field><field name="b">17</field><field name="c">false</field><field name="d">hey</field><field name="e">you</field><field name="f">1.2</field><field name="g">1.2</field><field name="h">9223372036854775800</field><field name="i">Z</field><field name="j" xsi:nil="true"/><field name="k">-14</field><field name="l">2</field></class>""")
	}
	it("Must render all collections (non-nested)") {
		val all = AllColl(
			List(1,2),
			List(Foo("one",1),Foo("two",2)),
			None,
			Some("Me")
			)
		sjJS.render(all) should equal("""{"a":[1,2],"b":[{"name":"one","age":1},{"name":"two","age":2}],"d":"Me"}""")
		sjXML.render(all) should equal("""<class type="co.blocke.scalajack.test.AllColl"><field name="a"><list class="scala.collection.immutable.List"><item>1</item><item>2</item></list></field><field name="b"><list class="scala.collection.immutable.List"><item><class type="co.blocke.scalajack.test.Foo"><field name="name">one</field><field name="age">1</field></class></item><item><class type="co.blocke.scalajack.test.Foo"><field name="name">two</field><field name="age">2</field></class></item></list></field><field name="d">Me</field></class>""")
	}
}
