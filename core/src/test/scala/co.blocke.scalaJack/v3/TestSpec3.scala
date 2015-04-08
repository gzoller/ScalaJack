package co.blocke.scalajack
package test.v3

import org.scalatest.{ FunSpec, GivenWhenThen, BeforeAndAfterAll }
import org.scalatest.Matchers._
import scala.util.Try
import java.util.UUID
import org.joda.time.DateTime
import org.joda.time.format.DateTimeFormat

class TestSpec3 extends FunSpec with GivenWhenThen with BeforeAndAfterAll {

	val data = One( "Greg", List("a","b"), List(Two("x",false),Two("y",true)), Two("Nest!",true), Some("wow"), Map("hey"->17,"you"->21), true, 99123986123L, Num.C, 46 )

	describe("=========================\n| -- V3 Tests Part 3 -- |\n=========================") {
		describe("Parameterized Class Support") {
			describe("Basic Parameterized Case Class") {
				it("Simple parameters - Foo[A](x:A) where A -> simple type") {
					val w = Wrap("number",true,15)
					val w2 = Wrap("number",true,"wow")
					val js = ScalaJack.render(w)
					val js2 = ScalaJack.render(w2)
					js should equal("""{"name":"number","data":true,"stuff":15}""")
					js2 should equal("""{"name":"number","data":true,"stuff":"wow"}""")
					ScalaJack.read[Wrap[Boolean,Int]](js) should equal(w)
					ScalaJack.read[Wrap[Boolean,String]](js2) should equal(w2)
				}
				it("Non-parameter case clase as a field member - Foo[A](x:A, b:Bar) where A -> simple type") {
					val w = Truck(false, Two("z",true))
					val js = ScalaJack.render(w)
					js should equal("""{"s":false,"t":{"foo":"z","bar":true}}""")
					ScalaJack.read[Truck[Boolean]](js) should equal(w)
				}
				it("Non-parameter case class as a parameter - Foo[A](x:A) where A -> Bar") {
					val w = Wrap("number",true,Two("a",false))
					val js = ScalaJack.render(w)
					js should equal("""{"name":"number","data":true,"stuff":{"foo":"a","bar":false}}""")
					ScalaJack.read[Wrap[Boolean,Two]](js) should equal(w)
				}
			}
			describe("Advanced Parameterized Case Class") {
				it("Parameterized case class as parameter - Foo[A](x:A) where A -> Bar[Int]") {
					val w = Carry("Bob", Wrap("Mary",3,"Available"))
					val x = Carry("Mary", Wrap("Greg",false,"Done"))
					val y = Carry("Fred", Wrap("Mike",Two("Steam",true),"OK"))
					val js = ScalaJack.render(w)
					val js2 = ScalaJack.render(x)
					val js3 = ScalaJack.render(y)
					js should equal("""{"s":"Bob","w":{"name":"Mary","data":3,"stuff":"Available"}}""")
					js2 should equal("""{"s":"Mary","w":{"name":"Greg","data":false,"stuff":"Done"}}""")
					js3 should equal("""{"s":"Fred","w":{"name":"Mike","data":{"foo":"Steam","bar":true},"stuff":"OK"}}""")
					ScalaJack.read[Carry[Int]](js) should equal(w)
					ScalaJack.read[Carry[Boolean]](js2) should equal(x)
					ScalaJack.read[Carry[Two]](js3) should equal(y)
				}
				it("Case class having value class parameter - Foo[A](x:A) where A -> value class") {
					val w = Carry("Mike", Wrap("Sally", new Wrapper(15), "Fine"))
					val js = ScalaJack.render(w)
					js should equal("""{"s":"Mike","w":{"name":"Sally","data":15,"stuff":"Fine"}}""")
					ScalaJack.read[Carry[Wrapper]](js) should equal(w)
				}
				it("Case class having parameterized case class as a parameter: Foo[A](x:A) where A -> Bar[Blah[Long]]") {
					val w = Carry("Bill", Wrap("Betty", Zoo("dog",false),"ok"))
					val js = ScalaJack.render(w)
					js should equal("""{"s":"Bill","w":{"name":"Betty","data":{"name":"dog","z":false},"stuff":"ok"}}""")
					ScalaJack.read[Carry[Zoo[Boolean]]](js) should equal(w)
				}
			}
			describe("Basic Collection Support") {
				it("Case class having List parameter - Foo[A](x:A) where A -> List of simple type") {
					val w = Carry("Trey", Wrap("Hobbies", List(true,true,false), "all"))
					val js = ScalaJack.render(w)
					js should equal("""{"s":"Trey","w":{"name":"Hobbies","data":[true,true,false],"stuff":"all"}}""")
					ScalaJack.read[Carry[List[Boolean]]](js) should equal(w)
				}
				it("Case class having Map parameter - Foo[A](x:A) where A -> Map of simple type") {
					val w = Carry("Troy", Wrap("Articles", Map("OK"->59), "all"))
					val js = ScalaJack.render(w)
					js should equal("""{"s":"Troy","w":{"name":"Articles","data":{"OK":59},"stuff":"all"}}""")
					ScalaJack.read[Carry[Map[String,Int]]](js) should equal(w)
				}
				it("Case class having Option parameter - Foo[A](x:A) where A -> Option of simple type") {
					val w = Carry("Terri", Wrap("Hobbies", Some(17), "all"))
					val x = Carry[Option[Int]]("Terry", Wrap("Hobbies", None, "all"))
					val js = ScalaJack.render[Carry[Option[Int]]](w)
					val js2 = ScalaJack.render[Carry[Option[Int]]](x)
					js should equal("""{"s":"Terri","w":{"name":"Hobbies","data":17,"stuff":"all"}}""")
					js2 should equal("""{"s":"Terry","w":{"name":"Hobbies","stuff":"all"}}""")
					ScalaJack.read[Carry[Option[Int]]](js) should equal(w)
					ScalaJack.read[Carry[Option[Int]]](js2) should equal(x)
				}
				it("Case class having List of parameterized value - Foo[A](x:List[A]) - where A is a simple type") {
					val w = BagList("list",List(1,2,3))
					val js = ScalaJack.render(w)
					js should equal("""{"s":"list","many":[1,2,3]}""")
					ScalaJack.read[BagList[Int]](js) should equal(w)
				}
				it("Case class having Map of parameterized value - Foo[A,B](x:Map[A,B]) - where A,B are simple types") {
					val w = BagMap(5, Map("one"->true,"two"->false))
					val js = ScalaJack.render(w)
					js should equal("""{"i":5,"items":{"one":true,"two":false}}""")
					ScalaJack.read[BagMap[Boolean]](js) should equal(w)
				}
				it("Case class having Option of parameterized value - Foo[A](x:Option[A]) - where A is a simple type") {
					val w = BagOpt(1, Some("ok"))
					val x = BagOpt[String](1,None)
					val js = ScalaJack.render(w)
					val js2 = ScalaJack.render(x)
					js should equal("""{"i":1,"maybe":"ok"}""")
					js2 should equal("""{"i":1}""")
					ScalaJack.read[BagOpt[String]](js) should equal(w)
					ScalaJack.read[BagOpt[String]](js2) should equal(x)
				}
			}
			describe("Advanced Collection Support - collections of parameterized case class") {
				it("Case class having List parameter - Foo[A](x:A) where A -> List of Bar[Int]") {
					val w = Carry("Trey", Wrap("Hobbies", List(Zoo("one",1),Zoo("two",2)), "all"))
					val js = ScalaJack.render(w)
					js should equal("""{"s":"Trey","w":{"name":"Hobbies","data":[{"name":"one","z":1},{"name":"two","z":2}],"stuff":"all"}}""")
					ScalaJack.read[Carry[List[Zoo[Int]]]](js) should equal(w)
				}
				it("Case class having Map parameter - Foo[A](x:A) where A -> Map of Bar[Int,String]") {
					val w = Carry("Troy", Wrap("Articles", Map("OK"->Zoo("q",false)), "all"))
					val js = ScalaJack.render(w)
					js should equal("""{"s":"Troy","w":{"name":"Articles","data":{"OK":{"name":"q","z":false}},"stuff":"all"}}""")
					ScalaJack.read[Carry[Map[String,Zoo[Boolean]]]](js) should equal(w)
				}
				it("Case class having Option parameter - Foo[A](x:A) where A -> Option of Bar[Int]") {
					val w = Carry("Terri", Wrap("Hobbies", Some(Zoo("a","b")), "all"))
					val x = Carry[Option[Int]]("Terry", Wrap("Hobbies", None, "all"))
					val js = ScalaJack.render[Carry[Option[Zoo[String]]]](w)
					val js2 = ScalaJack.render[Carry[Option[Int]]](x)
					js should equal("""{"s":"Terri","w":{"name":"Hobbies","data":{"name":"a","z":"b"},"stuff":"all"}}""")
					js2 should equal("""{"s":"Terry","w":{"name":"Hobbies","stuff":"all"}}""")
					ScalaJack.read[Carry[Option[Zoo[String]]]](js) should equal(w)
					ScalaJack.read[Carry[Option[Int]]](js2) should equal(x)
				}
				it("Case class having List parameter - Foo[A](x:A) where A -> List of value class") {
					val w = Carry("Trey", Wrap("Hobbies", List(new Wrapper(99),new Wrapper(100)), "all"))
					val js = ScalaJack.render(w)
					js should equal("""{"s":"Trey","w":{"name":"Hobbies","data":[99,100],"stuff":"all"}}""")
					ScalaJack.read[Carry[List[Wrapper]]](js) should equal(w)
				}
				it("Case class having Map parameter - Foo[A](x:A) where A -> Map of Bar[String,value class]") {
					val w = Carry("Troy", Wrap("Articles", Map("OK"->new Wrapper(2)), "all"))
					val js = ScalaJack.render(w)
					js should equal("""{"s":"Troy","w":{"name":"Articles","data":{"OK":2},"stuff":"all"}}""")
					ScalaJack.read[Carry[Map[String,Wrapper]]](js) should equal(w)
				}
				it("Case class having Option parameter - Foo[A](x:A) where A -> Option of value class") {
					val w = Carry("Terri", Wrap("Hobbies", Some(new Wrapper(-2)), "all"))
					val x = Carry[Option[Wrapper]]("Terry", Wrap("Hobbies", None, "all"))
					val js = ScalaJack.render[Carry[Option[Wrapper]]](w)
					val js2 = ScalaJack.render[Carry[Option[Wrapper]]](x)
					js should equal("""{"s":"Terri","w":{"name":"Hobbies","data":-2,"stuff":"all"}}""")
					js2 should equal("""{"s":"Terry","w":{"name":"Hobbies","stuff":"all"}}""")
					ScalaJack.read[Carry[Option[Wrapper]]](js) should equal(w)
					ScalaJack.read[Carry[Option[Wrapper]]](js2) should equal(x)
				}
				it("Case class having List of parameterized value - Foo[A](x:List[A]) - where A -> Bar[Int]") {
					val w = BagList("list",List(Zoo("a",1),Zoo("b",2)))
					val js = ScalaJack.render(w)
					js should equal("""{"s":"list","many":[{"name":"a","z":1},{"name":"b","z":2}]}""")
					ScalaJack.read[BagList[Zoo[Int]]](js) should equal(w)
				}
				it("Case class having Map of parameterized value - Foo[A,B](x:Map[A,B]) - where A,B -> String,Bar[Int]") {
					val w = BagMap(5, Map("one"->Zoo("a",1),"two"->Zoo("b",2)))
					val js = ScalaJack.render(w)
					js should equal("""{"i":5,"items":{"one":{"name":"a","z":1},"two":{"name":"b","z":2}}}""")
					ScalaJack.read[BagMap[Zoo[Int]]](js) should equal(w)
				}
				it("Case class having Option of parameterized value - Foo[A](x:Option[A]) - where A -> Bar[Int]") {
					val w = Carry("Terri", Wrap("Hobbies", Some(Truck(false,Two("aaa",true))), "all"))
					val x = Carry[Option[Truck[Boolean]]]("Terry", Wrap("Hobbies", None, "all"))
					val js = ScalaJack.render[Carry[Option[Truck[Boolean]]]](w)
					val js2 = ScalaJack.render[Carry[Option[Truck[Boolean]]]](x)
					js should equal("""{"s":"Terri","w":{"name":"Hobbies","data":{"s":false,"t":{"foo":"aaa","bar":true}},"stuff":"all"}}""")
					js2 should equal("""{"s":"Terry","w":{"name":"Hobbies","stuff":"all"}}""")
					ScalaJack.read[Carry[Option[Truck[Boolean]]]](js) should equal(w)
					ScalaJack.read[Carry[Option[Truck[Boolean]]]](js2) should equal(x)
				}
				it("Case class having List of parameterized value - Foo[A](x:List[A]) - where A -> value class") {
					val w = BagList("list",List(Zoo("a",new Wrapper(1)),Zoo("b",new Wrapper(2))))
					val js = ScalaJack.render(w)
					js should equal("""{"s":"list","many":[{"name":"a","z":1},{"name":"b","z":2}]}""")
					ScalaJack.read[BagList[Zoo[Wrapper]]](js) should equal(w)
				}
				it("Case class having Map of parameterized value - Foo[A,B](x:Map[A,B]) - where A,B -> String,value class") {
					val w = BagMap(5, Map("one"->Zoo("a",new Wrapper(1)),"two"->Zoo("b",new Wrapper(2))))
					val js = ScalaJack.render(w)
					js should equal("""{"i":5,"items":{"one":{"name":"a","z":1},"two":{"name":"b","z":2}}}""")
					ScalaJack.read[BagMap[Zoo[Wrapper]]](js) should equal(w)
				}
				it("Case class having Option of parameterized value - Foo[A](x:Option[A]) - where A -> value class") {
					val w = Carry("Terri", Wrap("Hobbies", Some(Zoo("a",new Wrapper(12))), "all"))
					val x = Carry[Option[Truck[Boolean]]]("Terry", Wrap("Hobbies", None, "all"))
					val js = ScalaJack.render[Carry[Option[Zoo[Wrapper]]]](w)
					val js2 = ScalaJack.render[Carry[Option[Truck[Boolean]]]](x)
					js should equal("""{"s":"Terri","w":{"name":"Hobbies","data":{"name":"a","z":12},"stuff":"all"}}""")
					js2 should equal("""{"s":"Terry","w":{"name":"Hobbies","stuff":"all"}}""")
					ScalaJack.read[Carry[Option[Zoo[Wrapper]]]](js) should equal(w)
					ScalaJack.read[Carry[Option[Truck[Boolean]]]](js2) should equal(x)
				}
			}
			describe("Basic trait support") {
				it("Parameter is a simple trait") {
					val w = Carry[Pop]("Surprise", Wrap("Yellow", Wow2("three",3), "Done"))
					val js = ScalaJack.render(w)
					js should equal("""{"s":"Surprise","w":{"name":"Yellow","data":{"_hint":"co.blocke.scalajack.test.v3.Wow2","x":"three","y":3},"stuff":"Done"}}""")
					ScalaJack.read[Carry[Pop]](js) should equal(w)
				}
				it("Parameter is List of trait") {
					val w = Carry[List[Pop]]("Surprise", Wrap("Yellow", List(Wow1("four",4),Wow2("three",3)), "Done"))
					val js = ScalaJack.render(w)
					js should equal("""{"s":"Surprise","w":{"name":"Yellow","data":[{"_hint":"co.blocke.scalajack.test.v3.Wow1","a":"four","b":4},{"_hint":"co.blocke.scalajack.test.v3.Wow2","x":"three","y":3}],"stuff":"Done"}}""")
					ScalaJack.read[Carry[List[Pop]]](js) should equal(w)
				}
				it("Parameter is Map of String->trait") {
					val w = Carry[Map[String,Pop]]("Surprise", Wrap("Yellow", Map("a"->Wow1("four",4),"b"->Wow2("three",3)), "Done"))
					val js = ScalaJack.render(w)
					js should equal("""{"s":"Surprise","w":{"name":"Yellow","data":{"a":{"_hint":"co.blocke.scalajack.test.v3.Wow1","a":"four","b":4},"b":{"_hint":"co.blocke.scalajack.test.v3.Wow2","x":"three","y":3}},"stuff":"Done"}}""")
					ScalaJack.read[Carry[Map[String,Pop]]](js) should equal(w)
				}
				it("Parameter is an Option of trait") {
					val w = Carry[Option[Pop]]("Terri", Wrap("Hobbies", Some(Wow1("ok",-99)), "all"))
					val x = Carry[Option[Pop]]("Terry", Wrap("Hobbies", None, "all"))
					val js = ScalaJack.render(w)
					val js2 = ScalaJack.render(x)
					js should equal("""{"s":"Terri","w":{"name":"Hobbies","data":{"_hint":"co.blocke.scalajack.test.v3.Wow1","a":"ok","b":-99},"stuff":"all"}}""")
					js2 should equal("""{"s":"Terry","w":{"name":"Hobbies","stuff":"all"}}""")
					ScalaJack.read[Carry[Option[Pop]]](js) should equal(w)
					ScalaJack.read[Carry[Option[Pop]]](js2) should equal(x)
				}
				it("List of parameter, where parameter is a trait") {
					val w = BagList[Pop]("list",List(Wow1("A",1),Wow1("B",2)))
					val js = ScalaJack.render(w)
					js should equal("""{"s":"list","many":[{"_hint":"co.blocke.scalajack.test.v3.Wow1","a":"A","b":1},{"_hint":"co.blocke.scalajack.test.v3.Wow1","a":"B","b":2}]}""")
					ScalaJack.read[BagList[Pop]](js) should equal(w)
				}
				it("Map of String->parameter, where parameter is a trait") {
					val w = BagMap[Pop](5, Map("one"->Wow2("q",7),"two"->Wow1("r",3)))
					val js = ScalaJack.render(w)
					js should equal("""{"i":5,"items":{"one":{"_hint":"co.blocke.scalajack.test.v3.Wow2","x":"q","y":7},"two":{"_hint":"co.blocke.scalajack.test.v3.Wow1","a":"r","b":3}}}""")
					ScalaJack.read[BagMap[Pop]](js) should equal(w)
				}
				it("Option of parameter, where parameter is a trait") {
					val w = Carry[Option[Pop]]("Terri", Wrap("Hobbies", Some(Wow2("finite",1000)), "all"))
					val x = Carry[Option[Pop]]("Terry", Wrap("Hobbies", None, "all"))
					val js = ScalaJack.render(w)
					val js2 = ScalaJack.render(x)
					js should equal("""{"s":"Terri","w":{"name":"Hobbies","data":{"_hint":"co.blocke.scalajack.test.v3.Wow2","x":"finite","y":1000},"stuff":"all"}}""")
					js2 should equal("""{"s":"Terry","w":{"name":"Hobbies","stuff":"all"}}""")
					ScalaJack.read[Carry[Option[Pop]]](js) should equal(w)
					ScalaJack.read[Carry[Option[Pop]]](js2) should equal(x)
				}
			}
			describe("Advanced trait support -- parameters are traits, themselves having parameters") {
				it("Case class having an embedded parameterized trait") {
					val w = Breakfast(true, Toast(7,"Burnt"))
					val js = ScalaJack.render(w)
					js should equal("""{"y":true,"bread":{"_hint":"co.blocke.scalajack.test.v3.Toast","g":7,"yum":"Burnt"}}""")
					ScalaJack.read[Breakfast[String]](js) should equal(w)
				}
				it("Case class having an embedded parameterized trait, with the trait's parameter another case class") {
					val w = Breakfast(true, Toast(7,Two("two",true)))
					val js = ScalaJack.render(w)
					js should equal("""{"y":true,"bread":{"_hint":"co.blocke.scalajack.test.v3.Toast","g":7,"yum":{"foo":"two","bar":true}}}""")
					ScalaJack.read[Breakfast[Two]](js) should equal(w)
				}
				it("Case class having an embedded parameterized trait, with the trait's parameter a value class") {
					val w = Breakfast(true, Toast(7,new Wrapper(-100)))
					val js = ScalaJack.render(w)
					js should equal("""{"y":true,"bread":{"_hint":"co.blocke.scalajack.test.v3.Toast","g":7,"yum":-100}}""")
					ScalaJack.read[Breakfast[Wrapper]](js) should equal(w)
				}
				it("Parameter is a parameterized trait") { // I can't believe this one worked!
					val w = Carry[Tart[Soup[String]]]("Bill", Wrap("Betty", Bun(3,Cruton(8,"eight")),"ok"))
					val js = ScalaJack.render(w)
					js should equal("""{"s":"Bill","w":{"name":"Betty","data":{"_hint":"co.blocke.scalajack.test.v3.Bun","g":3,"yum":{"_hint":"co.blocke.scalajack.test.v3.Cruton","i":8,"sweet":"eight"}},"stuff":"ok"}}""")
					ScalaJack.read[Carry[Tart[Soup[String]]]](js) should equal(w)
				}
				it("Parameter is List of parameterized trait") {
					val w = Carry[List[Tart[Boolean]]]("Trey", Wrap("Hobbies", List(Bun(1,false),Toast(2,true)), "all"))
					val js = ScalaJack.render(w)
					js should equal("""{"s":"Trey","w":{"name":"Hobbies","data":[{"_hint":"co.blocke.scalajack.test.v3.Bun","g":1,"yum":false},{"_hint":"co.blocke.scalajack.test.v3.Toast","g":2,"yum":true}],"stuff":"all"}}""")
					ScalaJack.read[Carry[List[Tart[Boolean]]]](js) should equal(w)
				}
				it("Parameter is Map of String->parameterized trait") {
					val w = Carry[Map[String,Tart[String]]]("Troy", Wrap("Articles", Map("OK"->Bun(27,"Hot")), "all"))
					val js = ScalaJack.render(w)
					js should equal("""{"s":"Troy","w":{"name":"Articles","data":{"OK":{"_hint":"co.blocke.scalajack.test.v3.Bun","g":27,"yum":"Hot"}},"stuff":"all"}}""")
					ScalaJack.read[Carry[Map[String,Tart[String]]]](js) should equal(w)
				}
				it("Parameter is an Option of parameterized trait") {
					val w = Carry[Option[Tart[Int]]]("Terri", Wrap("Hobbies", Some(Toast(11,12)), "all"))
					val x = Carry[Option[Tart[Int]]]("Terry", Wrap("Hobbies", None, "all"))
					val js = ScalaJack.render(w)
					val js2 = ScalaJack.render(x)
					js should equal("""{"s":"Terri","w":{"name":"Hobbies","data":{"_hint":"co.blocke.scalajack.test.v3.Toast","g":11,"yum":12},"stuff":"all"}}""")
					js2 should equal("""{"s":"Terry","w":{"name":"Hobbies","stuff":"all"}}""")
					ScalaJack.read[Carry[Option[Tart[Int]]]](js) should equal(w)
					ScalaJack.read[Carry[Option[Tart[Int]]]](js2) should equal(x)
				}
				it("List of parameter, where parameter is a parameterized trait") {
					val w = BagList[Tart[Boolean]]("list",List(Toast(1,true),Bun(2,false)))
					val js = ScalaJack.render(w)
					js should equal("""{"s":"list","many":[{"_hint":"co.blocke.scalajack.test.v3.Toast","g":1,"yum":true},{"_hint":"co.blocke.scalajack.test.v3.Bun","g":2,"yum":false}]}""")
					ScalaJack.read[BagList[Tart[Boolean]]](js) should equal(w)
				}
				it("Map of String->parameter, where parameter is a parameterized trait") {
					val w = BagMap[Tart[Boolean]](5, Map("one"->Bun(1,true),"two"->Toast(2,false)))
					val js = ScalaJack.render(w)
					js should equal("""{"i":5,"items":{"one":{"_hint":"co.blocke.scalajack.test.v3.Bun","g":1,"yum":true},"two":{"_hint":"co.blocke.scalajack.test.v3.Toast","g":2,"yum":false}}}""")
					ScalaJack.read[BagMap[Tart[Boolean]]](js) should equal(w)
				}
				it("Option of parameter, where parameter is a parameterized trait") {
					val w = BagOpt[Tart[String]](1, Some(Bun(6,"ok")))
					val x = BagOpt[Tart[String]](1,None)
					val js = ScalaJack.render(w)
					val js2 = ScalaJack.render(x)
					js should equal("""{"i":1,"maybe":{"_hint":"co.blocke.scalajack.test.v3.Bun","g":6,"yum":"ok"}}""")
					js2 should equal("""{"i":1}""")
					ScalaJack.read[BagOpt[Tart[String]]](js) should equal(w)
					ScalaJack.read[BagOpt[Tart[String]]](js2) should equal(x)
				}
			}
		}
	}
}
