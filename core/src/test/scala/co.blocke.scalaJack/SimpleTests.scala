package co.blocke.scalajack
package test

import org.scalatest.{ FunSpec, GivenWhenThen, BeforeAndAfterAll }
import org.scalatest.Matchers._
import scala.language.postfixOps

class SimpleTestSpec extends FunSpec with GivenWhenThen with BeforeAndAfterAll {
	val sjJS  = ScalaJack(Formats.JSON)

	describe("======================\n| -- Simple Tests -- |\n======================") {
/*
		describe("Render Tests") {
			it("Must render JSON and XML") {
				sjJS.render(Foo("John",24)) should equal("""{"name":"John","age":24}""")
			}
			it("Must render naked collections") {
				sjJS.render(List(1,2,3)) should equal("""[1,2,3]""")
				sjJS.render(Map("a"->false,"b"->true)) should equal("""{"a":false,"b":true}""")
				sjJS.render(Set(1,2,3)) should equal("""[1,2,3]""")
			}
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
					2 toShort,
					java.util.UUID.fromString("1e6c2b31-4dfe-4bf6-a0a0-882caaff0e9c")
				)
				// println(sjJS.render(all))
				sjJS.render(all) should equal("""{"a":5,"b":17,"c":false,"d":"hey","e":"you","f":1.2,"g":1.2,"h":9223372036854775800,"i":"Z","j":null,"k":-14,"l":2,"m":"1e6c2b31-4dfe-4bf6-a0a0-882caaff0e9c"}""")
			}
			it("Must render all collections (non-nested & non-canonical)") {
				val all = AllColl(
					List(1,2),
					List(Foo("one",1),Foo("two",2)),
					None,
					Some("Me"),
					List(Some(1),None,Some(3),None),
					Map("a"->1),
					Map(Foo("a",1) -> Some(WithType(5)),Foo("b",2) -> Some(WithType(7)),Foo("c",3) -> None)
					)
				// println(sjJS.render(all,VisitorContext("_hint",true)))
				sjJS.render(all,VisitorContext("_hint",false)) should equal("""{"a":[1,2],"b":[{"name":"one","age":1},{"name":"two","age":2}],"d":"Me","e":[1,3],"f":{"a":1},"g":{{"name":"a","age":1}:{"me":5},{"name":"b","age":2}:{"me":7}}}""")
			}
			it("Must render traits") {
				val t = Stuff("wow",Foo("me",9))
				sjJS.render(t) should equal("""{"item":"wow","other":{"_hint":"co.blocke.scalajack.test.Foo","name":"me","age":9}}""")
			}
			it("Must render typed classes") {
				val a1 = WithType("hey")
				val a2 = WithType(Foo("boom",9))
				val a3 = WithType(Set(List("a","b"),List("c")))
				sjJS.render(a1) should equal("""{"me":"hey"}""")
				sjJS.render(a2) should equal("""{"me":{"name":"boom","age":9}}""")
				sjJS.render(a3) should equal("""{"me":[["a","b"],["c"]]}""")
			}
			it("Must render Enumerations") {
				val all = EnumExer( Colors.Red, Formats.JSON )
				sjJS.render(all) should equal("""{"a":"Red","b":"JSON"}""")
			}
			it("Must render value classes") {
				val a1 = new Wrapper("test")
				val a2 = new Wrapper2(7)
				val a3 = Wrapped( new Wrapper("foo"),1 )
				val a4 = Wrapped2( new Wrapper2(true), 9 )
				sjJS.render(a1) should equal("\"test\"")
				sjJS.render(a2) should equal("7")
				sjJS.render(a3) should equal("""{"hey":"foo","you":1}""")
				sjJS.render(a4) should equal("""{"hey":true,"you":9}""")
			}
			it("Must support unicode") {
				val a = WithType("następujących")
				sjJS.render(a) should equal("{\"me\":\"nast\\u0119puj\\u0105cych\"}")
			}
		}
		*/

		describe("Read Tests") {
			/*
			it("Must read simple JSON") {
				val sjJS  = ScalaJack(Formats.JSON)
				val js = """{"name":"Fred","age":29,"bogus":false,"addr":{"street":"123 Main","zip":29384}}"""
				val z = sjJS.read[Pristine](js,VisitorContext().copy(isValidating = true))
			 	(z == Pristine("Fred",29,None,Address("123 Main",29384))) should be( true )
			}
			it("Must read all primitive types") {
				val sjJS  = ScalaJack(Formats.JSON)
				val js = """{"a":5,"b":17,"c":false,"d":"hey","e":"you","f":1.2,"g":1.2,"h":9223372036854775800,"i":"Z","j":null,"k":-14,"l":2,"m":"1e6c2b31-4dfe-4bf6-a0a0-882caaff0e9c","n":null}"""
				val z = sjJS.read[All](js,VisitorContext().copy(isValidating = true))
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
					2 toShort,
					java.util.UUID.fromString("1e6c2b31-4dfe-4bf6-a0a0-882caaff0e9c")
				)
				(all == z) should be( true )
			}
			it("Must read naked collections") {
				sjJS.read[List[Int]]("""[1,2,3]""") should equal(List(1,2,3))
				sjJS.read[List[Map[Int,Boolean]]]("""[{5:false,6:true},{10:false,11:true}]""",VisitorContext().copy(isValidating = true, isCanonical = false)) should equal(List(Map(5->false,6->true),Map(11->true,10->false)))
				sjJS.read[Map[String,Boolean]]("""{"a":false,"b":true}""") should equal(Map("a"->false,"b"->true))
				sjJS.read[Set[Int]]("""[1,2,3]""") should equal(Set(1,2,3))
			}
			it("Must read all collection types") {
				val all = AllColl(
					List(1,2),
					List(Foo("one",1),Foo("two",2)),
					None,
					Some("Me"),
					List(Some(1),None,Some(3),None),
					Map("a"->1),
					Map(Foo("a",1) -> Some(WithType(5)),Foo("b",2) -> Some(WithType(7)),Foo("c",3) -> None)
					)
				val js = """{"a":[1,2],"b":[{"name":"one","age":1},{"name":"two","age":2}],"d":"Me","e":[1,3],"f":{"a":1},"g":{{"name":"a","age":1}:{"me":5},{"name":"b","age":2}:{"me":7}}}"""
				val z = sjJS.read[AllColl](js,VisitorContext().copy(isValidating = true, isCanonical = false))
				// filter out the None values for comparison
				(all.copy(e=all.e.filter(_.isDefined),g=all.g.filter(_._2.isDefined)) == z) should be( true )
			}
			it("Must read traits") {
				val js = """{"item":"wow","other":{"name":"me","_hint":"co.blocke.scalajack.test.Foo","age":9}}"""
				val z = sjJS.read[Stuff](js,VisitorContext().copy(isValidating = true))
				val t = Stuff("wow",Foo("me",9))
				(z == t) should equal( true )
			}
			*/
			it("Must read typed (parameterized) classes") {
				val a1 = WithType("hey")
				val a2 = WithType(Foo("boom",9))
				val a3 = WithType(Set(List("a","b"),List("c")))
				val c1 = Case_1("Greg",List(WithType(1),WithType(2)))
				val c2 = Case_2("Greg",List(WithType(1),WithType(2)))
				val c3 = Case_3("Greg",List(Two(4,true),Two(3,true)))
				val c4 = List(List(WithType("foo")))
				val c5 = Case_5("Greg",List(Two(5,"five"),Two(6,"six")))
				val t1 = Ex1("hey",99)

/*
				sjJS.read[WithType[String]]("""{"me":"hey"}""") should equal( a1 )
				sjJS.read[WithType[Foo]]("""{"me":{"name":"boom","age":9}}""") should equal( a2 )
				sjJS.read[WithType[Set[List[String]]]]("""{"me":[["a","b"],["c"]]}""") should equal( a3 )
				// ::: Case 1 -------------------------------- Case class having a defined parameterized type
				val jsC1 = """{"name":"Greg","other":[{"me":1},{"me":2}]}"""
				(sjJS.read[Case_1](jsC1) == c1) should be( true )
				// ::: Case 2 -------------------------------- Parameterized case class having parameterized type
				val jsC2 = """{"name":"Greg","other":[{"me":1},{"me":2}]}"""
				(sjJS.read[Case_2[Int]](jsC2) == c2) should be( true )
				// ::: Case 3 -------------------------------- Parameterized case class having 2 param types--one specified, one not
				val jsC3 = """{"name":"Greg","other":[{"a":4,"b":true},{"a":3,"b":true}]}"""
				(sjJS.read[Case_3[Int]](jsC3) == c3) should be( true )
				// ::: Case 4 -------------------------------- Naked collection (nested) of parameterized type
				val jsC4 = """[[{"me":"foo"}]]"""
				(sjJS.read[List[List[WithType[String]]]](jsC4) == c4) should be( true )
				// ::: Case 5 -------------------------------- Multiple parameters, consumed out-of-order
				val jsC5 = """{"name":"Greg","other":[{"a":5,"b":"five"},{"a":6,"b":"six"}]}"""
				(sjJS.read[Case_5[String,Int]](jsC5) == c5) should be( true )
				// ::: Case 6 -------------------------------- Parameterized trait
				val jsT1 = """{"_hint":"co.blocke.scalajack.test.Ex1","a":"hey","b":99}"""
				(sjJS.read[Excite[Int,String]](jsT1) == t1) should be( true )
				// ::: Case 7 -------------------------------- Nested parameterized trait
*/
//				(pending)
val t2 = Ex2(Slp('a'),100)
val jsT2 = """{"_hint":"co.blocke.scalajack.test.Ex2","a":{"_hint":"co.blocke.scalajack.test.Slp","x":"a"},"b":100}"""
println(sjJS.read[Excite2[Sleep[Char],Int]](jsT2))
// trait Excite[T,U] {
// 	val a:U
// 	val b:T
// }
// trait Sleep[Y] {
// 	val x:Y
// }
// case class Ex1(a:String, b:Int) extends Excite[Int,String]
// case class Slp[Z](x:Z) extends Sleep[Z]
// case class Ex2[Z](a:Slp[Z], b:Int) extends Excite[Int,Sleep[Z]]
				// ::: Case 8 -------------------------------- Nested parameterized trait, partially applied
//				(pending)
			}
			/*
			it("Must read Enumerations") {
				val js = """{"a":"Red","b":"JSON"}"""
				val z = sjJS.read[EnumExer](js,VisitorContext().copy(isValidating = true))
				val all = EnumExer( Colors.Red, Formats.JSON )
				(all == z) should be( true )
			}
			it("Must read value classes") {
				val a1 = new Wrapper("test")
				val a2 = new Wrapper2(7)
				val a3 = Wrapped( new Wrapper("foo"),1 )
				val a4 = Wrapped2( new Wrapper2(true), 9 )
				val js1 = "\"test\""
				val js2 = "7"
				val js3 = """{"hey":"foo","you":1}"""
				val js4 = """{"hey":true,"you":9}"""
				(sjJS.read[Wrapper](js1) == a1) should be( true )
				//(sjJS.read[Wrapper2[Int]](js2) == a2) should be( true )  <-- Can't parse "7"
				(sjJS.read[Wrapped](js3) == a3) should be( true )
				(sjJS.read[Wrapped2[Int]](js4) == a4) should be( true )
			}
			it("Must read unicode") {
				(pending)
			}
			*/
		}
	}
}
