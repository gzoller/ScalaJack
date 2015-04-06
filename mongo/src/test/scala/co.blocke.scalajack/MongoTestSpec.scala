package co.blocke.scalajack
package test

import mongo._
import org.scalatest.{ FunSpec, GivenWhenThen, BeforeAndAfterAll }
import org.scalatest.Matchers._
import org.bson.types.ObjectId
import com.mongodb.casbah.Imports._
import scala.util.Try
import java.util.UUID
import org.joda.time.DateTime
import org.joda.time.format.DateTimeFormat

class MongoTestSpec extends FunSpec with GivenWhenThen with BeforeAndAfterAll {

	val data = One( "Greg", List("a","b"), List(Two("x",false),Two("y",true)), Two("Nest!",true), Some("wow"), Map("hey"->17,"you"->21), true, 99123986123L, Num.C, 46 )

	val sjM = ScalaJack(MongoType())

	describe("=====================\n| -- Mongo Tests -- |\n=====================") {
		describe("Basic Render/Read") {
			it( "Serialize simple object to JSON -- all supported data types" ) {
				val js = ScalaJack.render(data)
				js should equal( """{"name":"Greg","stuff":["a","b"],"more":[{"foo":"x","bar":false},{"foo":"y","bar":true}],"nest":{"foo":"Nest!","bar":true},"maybe":"wow","mymap":{"hey":17,"you":21},"flipflop":true,"big":99123986123,"num":"C","age":46}""" )
			}
			it( "De-serialize simple object from JSON -- all supported data types" ) {
				val js = ScalaJack.render(data)
				js should equal( """{"name":"Greg","stuff":["a","b"],"more":[{"foo":"x","bar":false},{"foo":"y","bar":true}],"nest":{"foo":"Nest!","bar":true},"maybe":"wow","mymap":{"hey":17,"you":21},"flipflop":true,"big":99123986123,"num":"C","age":46}""" )
				val b = ScalaJack.read[One](js)
				b should equal( data )
			}
			it("Should handle UUID types") {
				val thing = UuidThing("Foo",UUID.fromString("1e6c2b31-4dfe-4bf6-a0a0-882caaff0e9c"),List(UUID.fromString("1e6c2b31-4dfe-4bf6-a0a0-882caaff0e9c"),UUID.fromString("1e6c2b31-4dfe-4bf6-a0a0-882caaff0e9c")),Some(UUID.fromString("1e6c2b31-4dfe-4bf6-a0a0-882caaff0e9c")))
				val js = ScalaJack.render( thing )
				js should equal("""{"name":"Foo","uuid":"1e6c2b31-4dfe-4bf6-a0a0-882caaff0e9c","many":["1e6c2b31-4dfe-4bf6-a0a0-882caaff0e9c","1e6c2b31-4dfe-4bf6-a0a0-882caaff0e9c"],"maybe":"1e6c2b31-4dfe-4bf6-a0a0-882caaff0e9c"}""")
				val b = ScalaJack.read[UuidThing](js)
				b should equal( thing )
			}
			it("Should handle DateTime types") {
				val pattern = "dd-MM-yy"
				val t = DateTime.parse("07-01-86", DateTimeFormat.forPattern(pattern))
				val thing = JodaThing("Foo",t,List(t,t),Some(t))
				val js = ScalaJack.render( thing )
				js should equal("""{"name":"Foo","dt":505440000000,"many":[505440000000,505440000000],"maybe":505440000000}""")
				val b = ScalaJack.read[JodaThing](js)
				b should equal( thing )
			}
			it( "Should ignore extra fields in the inbound JSON" ) {
				val js = """{"foo":"hey","bogus":19,"bar":true}"""
				ScalaJack.read[Two](js) should equal( Two("hey",true) )
			}
			it( "Should ignore extra fields in the inbound database objects" ) {
				val dbo = MongoDBObject( "foo"->"hey", "bogus"->19, "bar"-> true )
				sjM.read[Two](dbo) should equal( Two("hey",true) )
			}
			it( "Handle empty Lists & Maps") {
				val four = Four(List[String](), Map[String,Int]())
				val js = ScalaJack.render(four)
				js should equal( """{"stuff":[],"things":{}}""" )
				ScalaJack.read[Four](js) should equal( four )
			}
			it("Primitive Lists") {
				val pl = PrimitiveLists(List(1,2,3), List(3L,4L), List(true,false), List('a','b','c'), List(1.2,3.4))
				val js = ScalaJack.render(pl)
				js should equal("""{"ints":[1,2,3],"longs":[3,4],"bools":[true,false],"chars":["a","b","c"],"doubles":[1.2,3.4]}""")
				ScalaJack.read[PrimitiveLists](js) should equal( pl )
			}
			it( "Naked Lists of string" ) {
				val stuff = List( "a","b","c" )
				val js = ScalaJack.render(stuff)
				js should equal( """["a","b","c"]""" )
				ScalaJack.read[List[String]](js) should equal( stuff )
			}
			it( "Naked Lists of UUID" ) {
				val stuff = List(UUID.fromString("1e6c2b31-4dfe-4bf6-a0a0-882caaff0e9c"),UUID.fromString("1e6c2b31-4dfe-4bf6-a0a0-882caaff0e9c") )
				val js = ScalaJack.render(stuff)
				js should equal( """["1e6c2b31-4dfe-4bf6-a0a0-882caaff0e9c","1e6c2b31-4dfe-4bf6-a0a0-882caaff0e9c"]""" )
				ScalaJack.read[List[UUID]](js) should equal( stuff )
			}
			it( "Naked Lists of Joda" ) {
				val pattern = "dd-MM-yy"
				val t = DateTime.parse("07-01-86", DateTimeFormat.forPattern(pattern))
				val stuff = List( t, t )
				val js = ScalaJack.render(stuff)
				js should equal( """[505440000000,505440000000]""" )
				ScalaJack.read[List[DateTime]](js) should equal( stuff )
			}
			it( "Naked Lists of objects" ) {
				val stuff = List( Three("three",Num.A,Wow1("foo",17)), Three("four",Num.B,Wow1("bar",18)) )
				val js = ScalaJack.render(stuff)
				js should equal( """[{"name":"three","two":"A","pp":{"_hint":"co.blocke.scalajack.test.Wow1","a":"foo","b":17}},{"name":"four","two":"B","pp":{"_hint":"co.blocke.scalajack.test.Wow1","a":"bar","b":18}}]""" )
				ScalaJack.read[List[Three]](js) should equal( stuff )
			}
			it( "Naked Lists of Boolean" ) {
				val stuff = List(true,false) // int, boolean, long, double, char
				val js = ScalaJack.render(stuff)
				js should equal("""[true,false]""")
				ScalaJack.read[List[Boolean]](js) should equal( stuff )
			}
			it( "Naked Lists of Int" ) {
				val stuff = List(5,6) // int, boolean, long, double, char
				val js = ScalaJack.render(stuff)
				js should equal("""[5,6]""")
				ScalaJack.read[List[Int]](js) should equal( stuff )
			}
			it( "Naked Lists of Long" ) {
				val stuff = List(5L,6L) // int, boolean, long, double, char
				val js = ScalaJack.render(stuff)
				js should equal("""[5,6]""")
				ScalaJack.read[List[Long]](js) should equal( stuff )
			}
			it( "Naked Lists of Double" ) {
				val stuff = List(5.1,6.2) // int, boolean, long, double, char
				val js = ScalaJack.render(stuff)
				js should equal("""[5.1,6.2]""")
				ScalaJack.read[List[Double]](js) should equal( stuff )
			}
			it( "Naked Lists of Char" ) {
				val stuff = List('a','b') // int, boolean, long, double, char
				val js = ScalaJack.render(stuff)
				js should equal("""["a","b"]""")
				ScalaJack.read[List[Char]](js) should equal( stuff )
			}
			it( "Naked Maps of string" ) {
				val stuff = Map( "a"->"b","c"->"d" )
				val js = ScalaJack.render(stuff)
				js should equal( """{"a":"b","c":"d"}""" )
				ScalaJack.read[Map[String,String]](js) should equal( stuff )
			}
			it( "Naked Maps of objects" ) {
				val stuff = Map( "a"->Three("three",Num.A,Wow1("foo",17)), "b"->Three("four",Num.B,Wow1("bar",18)) )
				val js = ScalaJack.render(stuff)
				js should equal( """{"a":{"name":"three","two":"A","pp":{"_hint":"co.blocke.scalajack.test.Wow1","a":"foo","b":17}},"b":{"name":"four","two":"B","pp":{"_hint":"co.blocke.scalajack.test.Wow1","a":"bar","b":18}}}""" )
				ScalaJack.read[Map[String,Three]](js) should equal( stuff )
			}
			it( "Naked Maps of Boolean" ) {
				val stuff = Map("a"->true,"b"->false) // int, boolean, long, double, char
				val js = ScalaJack.render(stuff)
				js should equal("""{"a":true,"b":false}""")
				ScalaJack.read[Map[String,Boolean]](js) should equal( stuff )
			}
			it( "Naked Maps of Int" ) {
				val stuff = Map("a"->5,"b"->6) // int, boolean, long, double, char
				val js = ScalaJack.render(stuff)
				js should equal("""{"a":5,"b":6}""")
				ScalaJack.read[Map[String,Int]](js) should equal( stuff )
			}
			it( "Naked Maps of Long" ) {
				val stuff = Map("a"->5L,"b"->6L) // int, boolean, long, double, char
				val js = ScalaJack.render(stuff)
				js should equal("""{"a":5,"b":6}""")
				ScalaJack.read[Map[String,Long]](js) should equal( stuff )
			}
			it( "Naked Maps of Double" ) {
				val stuff = Map("a"->5.1,"b"->6.2) // int, boolean, long, double, char
				val js = ScalaJack.render(stuff)
				js should equal("""{"a":5.1,"b":6.2}""")
				ScalaJack.read[Map[String,Double]](js) should equal( stuff )
			}
			it( "Naked Maps of Char" ) {
				val stuff = Map("a"->'b',"c"->'d') // int, boolean, long, double, char
				val js = ScalaJack.render(stuff)
				js should equal( """{"a":"b","c":"d"}""" )
				ScalaJack.read[Map[String,Char]](js) should equal( stuff )
			}
			it( "Renders and reads strings with embedded chars (newlines, quotes, etc.)" ) {
				val w = Two("This is a test\nOf the \"Emergency Broadcast \tSystem\"",true)
				val js = ScalaJack.render(w)
				js should equal("""{"foo":"This is a test\nOf the \"Emergency Broadcast \tSystem\"","bar":true}""")
				ScalaJack.read[Two](js) should equal(w)
			}
			it( "Handles a case class of all-optional values, that happen to be None") {
				val ao = AllOpt(None,None,None)
				val js = ScalaJack.render(ao)
				js should equal("""{}""")
				ScalaJack.read[AllOpt](js) should equal(ao)
			}
			it( "Handles null values - Double" ) {
				val js = """{"a":5.1,"b":null}"""
				val o = ScalaJack.read[Map[String,Double]](js)
				o should equal( Map("a"->5.1,"b"->null) )
			}
			it( "Handles null values - Boolean" ) {
				val js = """{"a":true,"b":null}"""
				val o = ScalaJack.read[Map[String,Boolean]](js)
				o should equal( Map("a"->true,"b"->null) )
			}
			it( "Handles null values - String" ) {
				val js = """{"a":"wow","b":null}"""
				val o = ScalaJack.read[Map[String,String]](js)
				o should equal( Map("a"->"wow","b"->null) )
			}
			it( "Handles null values - Int" ) {
				val js = """{"a":5,"b":null}"""
				val o = ScalaJack.read[Map[String,Int]](js)
				o should equal( Map("a"->5,"b"->null) )
			}
			it( "Handles null values - Long" ) {
				val js = """{"a":5,"b":null}"""
				val o = ScalaJack.read[Map[String,Long]](js)
				o should equal( Map("a"->5L,"b"->null) )
			}
			it( "Handles null values - UUID" ) {
				val js = """{"a":"1e6c2b31-4dfe-4bf6-a0a0-882caaff0e9c","b":null}"""
				val o = ScalaJack.read[Map[String,UUID]](js)
				o should equal( Map("a"->UUID.fromString("1e6c2b31-4dfe-4bf6-a0a0-882caaff0e9c"),"b"->null) )
			}
			it( "Handles null values - DateTime" ) {
				val js = """{"a":505440000000,"b":null}"""
				val o = ScalaJack.read[Map[String,DateTime]](js)
				val pattern = "dd-MM-yy"
				val t = DateTime.parse("07-01-86", DateTimeFormat.forPattern(pattern))
				o should equal( Map("a"->t,"b"->null) )
			}
		}
		describe("Trait Support") {
			it( "Traits with subclasses" ) {
				val t = Three("three",Num.A,Wow1("foo",17))
				val js2 = ScalaJack.render(t)
				js2 should equal( """{"name":"three","two":"A","pp":{"_hint":"co.blocke.scalajack.test.Wow1","a":"foo","b":17}}""" )
				// Change order so hint isn't first in list
				val js3 = """{"name":"three","two":"A","pp":{"a":"foo","_hint":"co.blocke.scalajack.test.Wow1","b":17}}"""
				val u = ScalaJack.read[Three](js3)
				u should equal( t )
			}
			it( "Support changing type hint" ) {
				val t = Three("three",Num.A,Wow1("foo",17))
				val js2 = ScalaJack.render(t,"hey")
				js2 should equal( """{"name":"three","two":"A","pp":{"hey":"co.blocke.scalajack.test.Wow1","a":"foo","b":17}}""" )
				val u = ScalaJack.read[Three](js2,"hey")
				u should equal( t )
			}
			it("Top-level trait") {
				val w = Wow1("hey",99)
				val js = ScalaJack.render[Pop](w)
				js should equal("""{"_hint":"co.blocke.scalajack.test.Wow1","a":"hey","b":99}""")
				ScalaJack.read[Pop](js) should equal(w)
			}
		}
		describe("Value Classes") {
			describe("Without custom JSON support") {
				it( "Simple value class support" ) {
					val stuff = ValSupport("foo", new Wrapper(42), false)
					val js = ScalaJack.render(stuff)
					js should equal( """{"name":"foo","wrap":42,"more":false}""" )
					ScalaJack.read[ValSupport](js) should equal( stuff )
				}
				it( "List of value class without custom JSON support" ) {
					val stuff = ListValSupport("bar", List(new Wrapper(99),new Wrapper(100)), true)
					val js = ScalaJack.render(stuff)
					js should equal("""{"name":"bar","wrap":[99,100],"more":true}""")
					ScalaJack.read[ListValSupport](js) should equal( stuff )
				}
				it( "Option of value class without custom JSON support" ) {
					val stuff = OptValSupport("hey", Some(new Wrapper(2)))
					val stuff2 = OptValSupport("hey", None)
					val js1 = ScalaJack.render(stuff)
					val js2 = ScalaJack.render(stuff2)
					js1 should equal("""{"name":"hey","wrap":2}""")
					js2 should equal("""{"name":"hey"}""")
					ScalaJack.read[OptValSupport](js1) should equal( stuff )
					ScalaJack.read[OptValSupport](js2) should equal( stuff2 )
				}
				it( "Map of value class without custom JSON support" ) {
					val stuff = MapValSupport("hey", Map("blah"->new Wrapper(2),"wow"->new Wrapper(3)))
					val js2 = ScalaJack.render(stuff)
					js2 should equal("""{"name":"hey","wrap":{"blah":2,"wow":3}}""")
					ScalaJack.read[MapValSupport](js2) should equal( stuff )
				}
			}
		/*
			describe("With custom JSON support") {
				it( "Simple value custom JSON support for Value class" ) {
					val stuff = ValSupport("foo", new Wrapper(99), true)
					val js = ScalaJack.render(stuff,ScalaJack.HINT,true)
					js should equal("""{"name":"foo","wrap":{"num":99,"hey":"you"},"more":true}""")
					ScalaJack.read[ValSupport](js,ScalaJack.HINT,true) should equal( stuff )
				}
				it( "List of value class with custom JSON support" ) {
					val stuff = ListValSupport("bar", List(new Wrapper(99),new Wrapper(100)), true)
					val js = ScalaJack.render(stuff,ScalaJack.HINT,true)
					js should equal("""{"name":"bar","wrap":[{"num":99,"hey":"you"},{"num":100,"hey":"you"}],"more":true}""")
					ScalaJack.read[ListValSupport](js,ScalaJack.HINT,true) should equal( stuff )
				}
				it( "Option of value class with custom JSON support" ) {
					val stuff = OptValSupport("hey", Some(new Wrapper(2)))
					val stuff2 = OptValSupport("hey", None)
					val js1 = ScalaJack.render(stuff,ScalaJack.HINT,true)
					val js2 = ScalaJack.render(stuff2,ScalaJack.HINT,true)
					js1 should equal("""{"name":"hey","wrap":{"num":2,"hey":"you"}}""")
					js2 should equal("""{"name":"hey"}""")
					ScalaJack.read[OptValSupport](js1,ScalaJack.HINT,true) should equal( stuff )
					ScalaJack.read[OptValSupport](js2,ScalaJack.HINT,true) should equal( stuff2 )
				}
				it( "Map of value class without custom JSON support" ) {
					val stuff = MapValSupport("hey", Map("blah"->new Wrapper(2),"wow"->new Wrapper(3)))
					val js1 = ScalaJack.render(stuff,ScalaJack.HINT,true)
					js1 should equal("""{"name":"hey","wrap":{"blah":{"num":2,"hey":"you"},"wow":{"num":3,"hey":"you"}}}""")
					ScalaJack.read[MapValSupport](js1,ScalaJack.HINT,true) should equal( stuff )
				}
			}
			*/
		}
		describe("Nested Constructs") {
			describe("With Lists") {
				it( "List of lists of case classes" ) {
					val ln = ListList("Fred", List(List(Animal("mouse", 4), Animal("bug", 6)), List(Animal("whale", 0), Animal("elephant", 4))))
					val js = ScalaJack.render(ln)
					js should equal( """{"name":"Fred","stuff":[[{"name":"mouse","legs":4},{"name":"bug","legs":6}],[{"name":"whale","legs":0},{"name":"elephant","legs":4}]]}""" )
					ScalaJack.read[ListList](js) should equal( ln )
				}
				it( "List of lists of lists of case classes" ) {
					val ln = ListListList("Fred",
						List(
							List(
								List(
									Animal("mouse", 4),
									Animal("bug", 6)),
								List(
									Animal("whale", 0),
									Animal("elephant", 4))),
							List(
								List(
									Animal("millipede", 1000),
									Animal("slug", 0)),
								List(
									Animal("bird", 2),
									Animal("tiger", 4)))))
					val js = ScalaJack.render(ln)
					js should equal( """{"name":"Fred","stuff":[[[{"name":"mouse","legs":4},{"name":"bug","legs":6}],[{"name":"whale","legs":0},{"name":"elephant","legs":4}]],[[{"name":"millipede","legs":1000},{"name":"slug","legs":0}],[{"name":"bird","legs":2},{"name":"tiger","legs":4}]]]}""" )
					ScalaJack.read[ListListList](js) should equal( ln )
				}
				// NOTE: If your list has a None it it, this will be lost upon re-marshal from JSON as JSON has no representation
				//       for a None (it's simply missing from the list).
				it( "List of option of case classes" ) {
					val lop = ListOpt("Jenny", List(Some(Animal("mouse", 4)), None, Some(Animal("whale", 0))))
					val js = ScalaJack.render(lop)
					js should equal( """{"name":"Jenny","stuff":[{"name":"mouse","legs":4},{"name":"whale","legs":0}]}""" )
					ScalaJack.read[ListOpt](js) should equal( lop.copy(stuff = lop.stuff.filter(_.isDefined) ) )
				}
				it( "List of map of case classes" ) {
					val lm = ListMap("Jenny", List(Map("a" -> Animal("mouse", 4)), Map("b" -> Animal("whale", 0))))
					val js = ScalaJack.render(lm)
					js should equal( """{"name":"Jenny","stuff":[{"a":{"name":"mouse","legs":4}},{"b":{"name":"whale","legs":0}}]}""" )
					ScalaJack.read[ListMap](js) should equal( lm )
				}
			}
			describe("With Option") {
				it( "Option of list of case classes" ) {
					val oln = OpList("Wow", Some(List(Animal("mouse", 4), Animal("bug", 6))))
					val js = ScalaJack.render(oln)
					js should equal( """{"name":"Wow","opList":[{"name":"mouse","legs":4},{"name":"bug","legs":6}]}""" )
					ScalaJack.read[OpList](js) should equal( oln )
				}
				it( "Option of nested list of case classes" ) {
					val oln = OpListList("Yay", Some(List(List(Animal("mouse", 4), Animal("bug", 6)), List(Animal("whale", 0), Animal("elephant", 4)))))
					val js = ScalaJack.render(oln)
					js should equal( """{"name":"Yay","opListList":[[{"name":"mouse","legs":4},{"name":"bug","legs":6}],[{"name":"whale","legs":0},{"name":"elephant","legs":4}]]}""" )
					ScalaJack.read[OpListList](js) should equal( oln )
				}
				it( "Option of map of case classes" ) {
					val om = OpMap("Wow", Some(Map("hello" -> (Animal("mouse", 4)))))
					val js = ScalaJack.render(om)
					js should equal( """{"name":"Wow","opMap":{"hello":{"name":"mouse","legs":4}}}""" )
					ScalaJack.read[OpMap](js) should equal( om )
					val om2 = OpMap("Wow", None)
					val js2 = ScalaJack.render(om2)
					js2 should equal( """{"name":"Wow"}""" )
					ScalaJack.read[OpMap](js2) should equal( om2 )
				}
				it( "Nested Option of case classes" ) {
					val oop = OpOp("Oops", Some(Some(Animal("mouse", 4))))
					val js = ScalaJack.render(oop)
					js should equal( """{"name":"Oops","opts":{"name":"mouse","legs":4}}""" )
					ScalaJack.read[OpOp](js) should equal( oop )
					val oop2 = OpOp("Oops", None)
					val js2 = ScalaJack.render(oop2)
					js2 should equal( """{"name":"Oops"}""" )
					ScalaJack.read[OpOp](js2) should equal( oop2 )
				}
			}
			describe("With Map") {
				it( "Map of list of case classes" ) {
					val mln = MapList("Bob", Map("Mike" -> List(Animal("mouse", 4), Animal("bug", 6)), "Sally" -> List(Animal("whale", 0), Animal("elephant", 4))))
					val js = ScalaJack.render(mln)
					js should equal( """{"name":"Bob","mapList":{"Mike":[{"name":"mouse","legs":4},{"name":"bug","legs":6}],"Sally":[{"name":"whale","legs":0},{"name":"elephant","legs":4}]}}""" )
					ScalaJack.read[MapList](js) should equal( mln )
				}
				it( "Map of nested lists of case classes" ) {
					val mln = MapListList("Bob", Map("Everyone" -> List(List(Animal("mouse", 4), Animal("bug", 6)), List(Animal("whale", 0), Animal("elephant", 4)))))
					val js = ScalaJack.render(mln)
					js should equal( """{"name":"Bob","mapList":{"Everyone":[[{"name":"mouse","legs":4},{"name":"bug","legs":6}],[{"name":"whale","legs":0},{"name":"elephant","legs":4}]]}}""" )
					ScalaJack.read[MapListList](js) should equal( mln )
				}
				it( "Map of option of case classes" ) {
					val a: Option[Animal] = None
					val mln = MapOpt("Bob", Map("things" -> Some(Animal("mouse", 4)), "otherthings" -> a))
					val js = ScalaJack.render(mln)
					js should equal( """{"name":"Bob","mapOpt":{"things":{"name":"mouse","legs":4}}}""" )
					ScalaJack.read[MapOpt](js) should equal( mln.copy(mapOpt = mln.mapOpt.filter({ case (k, v) => v.isDefined })) )
				}
				it( "Map of map of case classes" ) {
					val mm = MapMap("Bob", Map("things" -> Map("a" -> Animal("mouse", 4), "b" -> Animal("horse", 4)), "stuff" -> Map("c" -> Animal("sloth", 2))))
					val js = ScalaJack.render(mm)
					js should equal( """{"name":"Bob","mapmap":{"things":{"a":{"name":"mouse","legs":4},"b":{"name":"horse","legs":4}},"stuff":{"c":{"name":"sloth","legs":2}}}}""" )
					ScalaJack.read[MapMap](js) should equal( mm )
				}
			}
		}
		describe("MongoDB/Casbah Support") {
			it( "MongoKey Annotation (_id field generation) - single key" ) {
				val five = Five("Fred",Two("blah",true))
				val dbo = sjM.render(five)
				dbo.toString should equal( """{ "_id" : "Fred" , "two" : { "foo" : "blah" , "bar" : true}}""" )
				sjM.read[Five](dbo) should equal( five )
			}
			it( "MongoKey Annotation (_id field generation) - compound key" ) {
				val six = Six("Fred",12,Two("blah",true))
				val dbo = sjM.render(six)
				dbo.toString should equal( """{ "_id" : { "name" : "Fred" , "num" : 12} , "two" : { "foo" : "blah" , "bar" : true}}""" )
				sjM.read[Six](dbo) should equal( six )
			}
			/*
			it("ObjectId support") {
				val oid = new ObjectId
				val seven = Seven(oid,Two("blah",true))
				val dbo = sjM.render(seven)
				dbo.toString should equal( """{ "_id" : { "$oid" : """"+oid+""""} , "two" : { "foo" : "blah" , "bar" : true}}""" )
				sjM.read[Seven](dbo) should equal( seven )
				val js = ScalaJack.render(seven)
				ScalaJack.read[Seven](js) should equal( seven )
			}
			*/
			it("Naked Map support") {
				val li = Map("a"->1,"b"->2,"c"->3)
				val dbo = sjM.render(li)
				dbo.toString should equal( """{ "a" : 1 , "b" : 2 , "c" : 3}""" )
				sjM.read[Map[String,Int]](dbo) should equal( li )
			}
			it("UUID support") {
				val thing = UuidThing("Foo",UUID.fromString("1e6c2b31-4dfe-4bf6-a0a0-882caaff0e9c"),List(UUID.fromString("1e6c2b31-4dfe-4bf6-a0a0-882caaff0e9c"),UUID.fromString("1e6c2b31-4dfe-4bf6-a0a0-882caaff0e9c")),Some(UUID.fromString("1e6c2b31-4dfe-4bf6-a0a0-882caaff0e9c")))
				val dbo = sjM.render( thing )
				dbo.toString should equal("""{ "name" : "Foo" , "uuid" : "1e6c2b31-4dfe-4bf6-a0a0-882caaff0e9c" , "many" : [ "1e6c2b31-4dfe-4bf6-a0a0-882caaff0e9c" , "1e6c2b31-4dfe-4bf6-a0a0-882caaff0e9c"] , "maybe" : "1e6c2b31-4dfe-4bf6-a0a0-882caaff0e9c"}""")
				val b = sjM.read[UuidThing](dbo)
				b should equal( thing )
			}
			it("DateTime support") {
				val pattern = "dd-MM-yy"
				val t = DateTime.parse("07-01-86", DateTimeFormat.forPattern(pattern))
				val thing = JodaThing("Foo",t,List(t,t),Some(t))
				val dbo = sjM.render( thing )
				dbo.toString should equal("""{ "name" : "Foo" , "dt" : 505440000000 , "many" : [ 505440000000 , 505440000000] , "maybe" : 505440000000}""")
				val b = sjM.read[JodaThing](dbo)
				b should equal( thing )
			}
		}
		describe("Parameterized Class Support") {
			describe("Basic Parameterized Case Class") {
				it("Simple parameters - Foo[A](x:A) where A -> simple type") {
					val w = Wrap("number",true,15)
					val w2 = Wrap("number",true,"wow")
					val js = ScalaJack.render(w)
					val js2 = ScalaJack.render(w2)
					val db = sjM.render(w)
					val db2 = sjM.render(w2)
					js should equal("""{"name":"number","data":true,"stuff":15}""")
					js2 should equal("""{"name":"number","data":true,"stuff":"wow"}""")
					db.toString should equal("""{ "name" : "number" , "data" : true , "stuff" : 15}""")
					db2.toString should equal("""{ "name" : "number" , "data" : true , "stuff" : "wow"}""")
					ScalaJack.read[Wrap[Boolean,Int]](js) should equal(w)
					ScalaJack.read[Wrap[Boolean,String]](js2) should equal(w2)
					sjM.read[Wrap[Boolean,Int]](db) should equal(w)
					sjM.read[Wrap[Boolean,String]](db2) should equal(w2)
				}
				it("Non-parameter case clase as a field member - Foo[A](x:A, b:Bar) where A -> simple type") {
					val w = Truck(false, Two("z",true))
					val js = ScalaJack.render(w)
					val dbo = sjM.render(w)
					js should equal("""{"s":false,"t":{"foo":"z","bar":true}}""")
					dbo.toString should equal("""{ "s" : false , "t" : { "foo" : "z" , "bar" : true}}""")
					ScalaJack.read[Truck[Boolean]](js) should equal(w)
					sjM.read[Truck[Boolean]](dbo) should equal(w)
				}
				it("Non-parameter case class as a parameter - Foo[A](x:A) where A -> Bar") {
					val w = Wrap("number",true,Two("a",false))
					val js = ScalaJack.render(w)
					js should equal("""{"name":"number","data":true,"stuff":{"foo":"a","bar":false}}""")
					val db = sjM.render(w)
					db.toString should equal("""{ "name" : "number" , "data" : true , "stuff" : { "foo" : "a" , "bar" : false}}""")
					ScalaJack.read[Wrap[Boolean,Two]](js) should equal(w)
					sjM.read[Wrap[Boolean,Two]](db) should equal(w)
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
					val db = sjM.render(w)
					val db2 = sjM.render(x)
					val db3 = sjM.render(y)
					js should equal("""{"s":"Bob","w":{"name":"Mary","data":3,"stuff":"Available"}}""")
					js2 should equal("""{"s":"Mary","w":{"name":"Greg","data":false,"stuff":"Done"}}""")
					js3 should equal("""{"s":"Fred","w":{"name":"Mike","data":{"foo":"Steam","bar":true},"stuff":"OK"}}""")
					db.toString should equal("""{ "s" : "Bob" , "w" : { "name" : "Mary" , "data" : 3 , "stuff" : "Available"}}""")
					db2.toString should equal("""{ "s" : "Mary" , "w" : { "name" : "Greg" , "data" : false , "stuff" : "Done"}}""")
					db3.toString should equal("""{ "s" : "Fred" , "w" : { "name" : "Mike" , "data" : { "foo" : "Steam" , "bar" : true} , "stuff" : "OK"}}""")
					ScalaJack.read[Carry[Int]](js) should equal(w)
					ScalaJack.read[Carry[Boolean]](js2) should equal(x)
					ScalaJack.read[Carry[Two]](js3) should equal(y)
					sjM.read[Carry[Int]](db) should equal(w)
					sjM.read[Carry[Boolean]](db2) should equal(x)
					sjM.read[Carry[Two]](db3) should equal(y)
				}
				it("Case class having value class parameter - Foo[A](x:A) where A -> value class") {
					val w = Carry("Mike", Wrap("Sally", new Wrapper(15), "Fine"))
					val js = ScalaJack.render(w)
					val db = sjM.render(w)
					js should equal("""{"s":"Mike","w":{"name":"Sally","data":15,"stuff":"Fine"}}""")
					db.toString should equal("""{ "s" : "Mike" , "w" : { "name" : "Sally" , "data" : 15 , "stuff" : "Fine"}}""")
					ScalaJack.read[Carry[Wrapper]](js) should equal(w)
					sjM.read[Carry[Wrapper]](db) should equal(w)
				}
				it("Case class having parameterized case class as a parameter: Foo[A](x:A) where A -> Bar[Blah[Long]]") {
					val w = Carry("Bill", Wrap("Betty", Zoo("dog",false),"ok"))
					val js = ScalaJack.render(w)
					val db = sjM.render(w)
					js should equal("""{"s":"Bill","w":{"name":"Betty","data":{"name":"dog","z":false},"stuff":"ok"}}""")
					db.toString should equal("""{ "s" : "Bill" , "w" : { "name" : "Betty" , "data" : { "name" : "dog" , "z" : false} , "stuff" : "ok"}}""")
					ScalaJack.read[Carry[Zoo[Boolean]]](js) should equal(w)
					sjM.read[Carry[Zoo[Boolean]]](db) should equal(w)
				}
			}
			describe("Basic Collection Support") {
				it("Case class having List parameter - Foo[A](x:A) where A -> List of simple type") {
					val w = Carry("Trey", Wrap("Hobbies", List(true,true,false), "all"))
					val js = ScalaJack.render(w)
					val db = sjM.render(w)
					js should equal("""{"s":"Trey","w":{"name":"Hobbies","data":[true,true,false],"stuff":"all"}}""")
					db.toString should equal("""{ "s" : "Trey" , "w" : { "name" : "Hobbies" , "data" : [ true , true , false] , "stuff" : "all"}}""")
					ScalaJack.read[Carry[List[Boolean]]](js) should equal(w)
					sjM.read[Carry[List[Boolean]]](db) should equal(w)
				}
				it("Case class having Map parameter - Foo[A](x:A) where A -> Map of simple type") {
					val w = Carry("Troy", Wrap("Articles", Map("OK"->59), "all"))
					val js = ScalaJack.render(w)
					val db = sjM.render(w)
					js should equal("""{"s":"Troy","w":{"name":"Articles","data":{"OK":59},"stuff":"all"}}""")
					db.toString should equal("""{ "s" : "Troy" , "w" : { "name" : "Articles" , "data" : { "OK" : 59} , "stuff" : "all"}}""")
					ScalaJack.read[Carry[Map[String,Int]]](js) should equal(w)
					sjM.read[Carry[Map[String,Int]]](db) should equal(w)
				}
				it("Case class having Option parameter - Foo[A](x:A) where A -> Option of simple type") {
					val w = Carry("Terri", Wrap("Hobbies", Some(17).asInstanceOf[Option[Int]], "all"))
					val x = Carry[Option[Int]]("Terry", Wrap("Hobbies", None, "all"))
					val js = ScalaJack.render(w)
					val js2 = ScalaJack.render(x)
					val db = sjM.render(w)
					val db2 = sjM.render(x)
					js should equal("""{"s":"Terri","w":{"name":"Hobbies","data":17,"stuff":"all"}}""")
					js2 should equal("""{"s":"Terry","w":{"name":"Hobbies","stuff":"all"}}""")
					db.toString should equal("""{ "s" : "Terri" , "w" : { "name" : "Hobbies" , "data" : 17 , "stuff" : "all"}}""")
					db2.toString should equal("""{ "s" : "Terry" , "w" : { "name" : "Hobbies" , "stuff" : "all"}}""")
					ScalaJack.read[Carry[Option[Int]]](js) should equal(w)
					ScalaJack.read[Carry[Option[Int]]](js2) should equal(x)
					sjM.read[Carry[Option[Int]]](db) should equal(w)
					sjM.read[Carry[Option[Int]]](db2) should equal(x)
				}
				it("Case class having List of parameterized value - Foo[A](x:List[A]) - where A is a simple type") {
					val w = BagList("list",List(1,2,3))
					val js = ScalaJack.render(w)
					val db = sjM.render(w)
					js should equal("""{"s":"list","many":[1,2,3]}""")
					db.toString should equal("""{ "s" : "list" , "many" : [ 1 , 2 , 3]}""")
					ScalaJack.read[BagList[Int]](js) should equal(w)
					sjM.read[BagList[Int]](db) should equal(w)
				}
				it("Case class having Map of parameterized value - Foo[A,B](x:Map[A,B]) - where A,B are simple types") {
					val w = BagMap(5, Map("one"->true,"two"->false))
					val js = ScalaJack.render(w)
					val db = sjM.render(w)
					js should equal("""{"i":5,"items":{"one":true,"two":false}}""")
					db.toString should equal("""{ "i" : 5 , "items" : { "one" : true , "two" : false}}""")
					ScalaJack.read[BagMap[Boolean]](js) should equal(w)
					sjM.read[BagMap[Boolean]](db) should equal(w)
				}
				it("Case class having Option of parameterized value - Foo[A](x:Option[A]) - where A is a simple type") {
					val w = BagOpt(1, Some("ok"))
					val x = BagOpt[String](1,None)
					val js = ScalaJack.render(w)
					val js2 = ScalaJack.render(x)
					val db = sjM.render(w)
					val db2 = sjM.render(x)
					js should equal("""{"i":1,"maybe":"ok"}""")
					js2 should equal("""{"i":1}""")
					db.toString should equal("""{ "i" : 1 , "maybe" : "ok"}""")
					db2.toString should equal("""{ "i" : 1}""")
					ScalaJack.read[BagOpt[String]](js) should equal(w)
					ScalaJack.read[BagOpt[String]](js2) should equal(x)
					sjM.read[BagOpt[String]](db) should equal(w)
					sjM.read[BagOpt[String]](db2) should equal(x)
				}
			}
			describe("Advanced Collection Support - collections of parameterized case class") {
				it("Case class having List parameter - Foo[A](x:A) where A -> List of Bar[Int]") {
					val w = Carry("Trey", Wrap("Hobbies", List(Zoo("one",1),Zoo("two",2)), "all"))
					val js = ScalaJack.render(w)
					val db = sjM.render(w)
					js should equal("""{"s":"Trey","w":{"name":"Hobbies","data":[{"name":"one","z":1},{"name":"two","z":2}],"stuff":"all"}}""")
					db.toString should equal("""{ "s" : "Trey" , "w" : { "name" : "Hobbies" , "data" : [ { "name" : "one" , "z" : 1} , { "name" : "two" , "z" : 2}] , "stuff" : "all"}}""")
					ScalaJack.read[Carry[List[Zoo[Int]]]](js) should equal(w)
					sjM.read[Carry[List[Zoo[Int]]]](db) should equal(w)
				}
				it("Case class having Map parameter - Foo[A](x:A) where A -> Map of Bar[Int,String]") {
					val w = Carry("Troy", Wrap("Articles", Map("OK"->Zoo("q",false)), "all"))
					val js = ScalaJack.render(w)
					val db = sjM.render(w)
					js should equal("""{"s":"Troy","w":{"name":"Articles","data":{"OK":{"name":"q","z":false}},"stuff":"all"}}""")
					db.toString should equal("""{ "s" : "Troy" , "w" : { "name" : "Articles" , "data" : { "OK" : { "name" : "q" , "z" : false}} , "stuff" : "all"}}""")
					ScalaJack.read[Carry[Map[String,Zoo[Boolean]]]](js) should equal(w)
					sjM.read[Carry[Map[String,Zoo[Boolean]]]](db) should equal(w)
				}
				it("Case class having Option parameter - Foo[A](x:A) where A -> Option of Bar[Int]") {
					val w = Carry("Terri", Wrap("Hobbies", Some(Zoo("a","b")).asInstanceOf[Option[Zoo[String]]], "all"))
					val x = Carry[Option[Int]]("Terry", Wrap("Hobbies", None, "all"))
					val js = ScalaJack.render(w)
					val js2 = ScalaJack.render(x)
					val db = sjM.render(w)
					val db2 = sjM.render(x)
					js should equal("""{"s":"Terri","w":{"name":"Hobbies","data":{"name":"a","z":"b"},"stuff":"all"}}""")
					js2 should equal("""{"s":"Terry","w":{"name":"Hobbies","stuff":"all"}}""")
					db.toString should equal("""{ "s" : "Terri" , "w" : { "name" : "Hobbies" , "data" : { "name" : "a" , "z" : "b"} , "stuff" : "all"}}""")
					db2.toString should equal("""{ "s" : "Terry" , "w" : { "name" : "Hobbies" , "stuff" : "all"}}""")
					ScalaJack.read[Carry[Option[Zoo[String]]]](js) should equal(w)
					ScalaJack.read[Carry[Option[Zoo[String]]]](js2) should equal(x)
					sjM.read[Carry[Option[Zoo[String]]]](db) should equal(w)
					sjM.read[Carry[Option[Zoo[String]]]](db2) should equal(x)
				}
				it("Case class having List parameter - Foo[A](x:A) where A -> List of value class") {
					val w = Carry("Trey", Wrap("Hobbies", List(new Wrapper(99),new Wrapper(100)), "all"))
					val js = ScalaJack.render(w)
					val db = sjM.render(w)
					js should equal("""{"s":"Trey","w":{"name":"Hobbies","data":[99,100],"stuff":"all"}}""")
					db.toString should equal("""{ "s" : "Trey" , "w" : { "name" : "Hobbies" , "data" : [ 99 , 100] , "stuff" : "all"}}""")
					ScalaJack.read[Carry[List[Wrapper]]](js) should equal(w)
					sjM.read[Carry[List[Wrapper]]](db) should equal(w)
				}
				it("Case class having Map parameter - Foo[A](x:A) where A -> Map of Bar[String,value class]") {
					val w = Carry("Troy", Wrap("Articles", Map("OK"->new Wrapper(2)), "all"))
					val js = ScalaJack.render(w)
					val db = sjM.render(w)
					js should equal("""{"s":"Troy","w":{"name":"Articles","data":{"OK":2},"stuff":"all"}}""")
					db.toString should equal("""{ "s" : "Troy" , "w" : { "name" : "Articles" , "data" : { "OK" : 2} , "stuff" : "all"}}""")
					ScalaJack.read[Carry[Map[String,Wrapper]]](js) should equal(w)
					sjM.read[Carry[Map[String,Wrapper]]](db) should equal(w)
				}
				it("Case class having Option parameter - Foo[A](x:A) where A -> Option of value class") {
					val w = Carry("Terri", Wrap("Hobbies", Some(new Wrapper(-2)).asInstanceOf[Option[Wrapper]], "all"))
					val x = Carry[Option[Wrapper]]("Terry", Wrap("Hobbies", None, "all"))
					val js = ScalaJack.render(w)
					val js2 = ScalaJack.render(x)
					val db = sjM.render(w)
					val db2 = sjM.render(x)
					js should equal("""{"s":"Terri","w":{"name":"Hobbies","data":-2,"stuff":"all"}}""")
					js2 should equal("""{"s":"Terry","w":{"name":"Hobbies","stuff":"all"}}""")
					db.toString should equal("""{ "s" : "Terri" , "w" : { "name" : "Hobbies" , "data" : -2 , "stuff" : "all"}}""")
					db2.toString should equal("""{ "s" : "Terry" , "w" : { "name" : "Hobbies" , "stuff" : "all"}}""")
					ScalaJack.read[Carry[Option[Wrapper]]](js) should equal(w)
					ScalaJack.read[Carry[Option[Wrapper]]](js2) should equal(x)
					sjM.read[Carry[Option[Wrapper]]](db) should equal(w)
					sjM.read[Carry[Option[Wrapper]]](db2) should equal(x)
				}
				it("Case class having List of parameterized value - Foo[A](x:List[A]) - where A -> Bar[Int]") {
					val w = BagList("list",List(Zoo("a",1),Zoo("b",2)))
					val js = ScalaJack.render(w)
					val db = sjM.render(w)
					js should equal("""{"s":"list","many":[{"name":"a","z":1},{"name":"b","z":2}]}""")
					db.toString should equal("""{ "s" : "list" , "many" : [ { "name" : "a" , "z" : 1} , { "name" : "b" , "z" : 2}]}""")
					ScalaJack.read[BagList[Zoo[Int]]](js) should equal(w)
					sjM.read[BagList[Zoo[Int]]](db) should equal(w)
				}
				it("Case class having Map of parameterized value - Foo[A,B](x:Map[A,B]) - where A,B -> String,Bar[Int]") {
					val w = BagMap(5, Map("one"->Zoo("a",1),"two"->Zoo("b",2)))
					val js = ScalaJack.render(w)
					val db = sjM.render(w)
					js should equal("""{"i":5,"items":{"one":{"name":"a","z":1},"two":{"name":"b","z":2}}}""")
					db.toString should equal("""{ "i" : 5 , "items" : { "one" : { "name" : "a" , "z" : 1} , "two" : { "name" : "b" , "z" : 2}}}""")
					ScalaJack.read[BagMap[Zoo[Int]]](js) should equal(w)
					sjM.read[BagMap[Zoo[Int]]](db) should equal(w)
				}
				it("Case class having Option of parameterized value - Foo[A](x:Option[A]) - where A -> Bar[Int]") {
					val w = Carry("Terri", Wrap("Hobbies", Some(Truck(false,Two("aaa",true))).asInstanceOf[Option[Truck[Boolean]]], "all"))
					val x = Carry[Option[Truck[Boolean]]]("Terry", Wrap("Hobbies", None, "all"))
					val js = ScalaJack.render(w)
					val js2 = ScalaJack.render(x)
					val db = sjM.render(w)
					val db2 = sjM.render(x)
					js should equal("""{"s":"Terri","w":{"name":"Hobbies","data":{"s":false,"t":{"foo":"aaa","bar":true}},"stuff":"all"}}""")
					js2 should equal("""{"s":"Terry","w":{"name":"Hobbies","stuff":"all"}}""")
					db.toString should equal("""{ "s" : "Terri" , "w" : { "name" : "Hobbies" , "data" : { "s" : false , "t" : { "foo" : "aaa" , "bar" : true}} , "stuff" : "all"}}""")
					db2.toString should equal("""{ "s" : "Terry" , "w" : { "name" : "Hobbies" , "stuff" : "all"}}""")
					ScalaJack.read[Carry[Option[Truck[Boolean]]]](js) should equal(w)
					ScalaJack.read[Carry[Option[Truck[Boolean]]]](js2) should equal(x)
					sjM.read[Carry[Option[Truck[Boolean]]]](db) should equal(w)
					sjM.read[Carry[Option[Truck[Boolean]]]](db2) should equal(x)
				}
				it("Case class having List of parameterized value - Foo[A](x:List[A]) - where A -> value class") {
					val w = BagList("list",List(Zoo("a",new Wrapper(1)),Zoo("b",new Wrapper(2))))
					val js = ScalaJack.render(w)
					val db = sjM.render(w)
					js should equal("""{"s":"list","many":[{"name":"a","z":1},{"name":"b","z":2}]}""")
					db.toString should equal("""{ "s" : "list" , "many" : [ { "name" : "a" , "z" : 1} , { "name" : "b" , "z" : 2}]}""")
					ScalaJack.read[BagList[Zoo[Wrapper]]](js) should equal(w)
					sjM.read[BagList[Zoo[Wrapper]]](db) should equal(w)
				}
				it("Case class having Map of parameterized value - Foo[A,B](x:Map[A,B]) - where A,B -> String,value class") {
					val w = BagMap(5, Map("one"->Zoo("a",new Wrapper(1)),"two"->Zoo("b",new Wrapper(2))))
					val js = ScalaJack.render(w)
					val db = sjM.render(w)
					js should equal("""{"i":5,"items":{"one":{"name":"a","z":1},"two":{"name":"b","z":2}}}""")
					db.toString should equal("""{ "i" : 5 , "items" : { "one" : { "name" : "a" , "z" : 1} , "two" : { "name" : "b" , "z" : 2}}}""")
					ScalaJack.read[BagMap[Zoo[Wrapper]]](js) should equal(w)
					sjM.read[BagMap[Zoo[Wrapper]]](db) should equal(w)
				}
				it("Case class having Option of parameterized value - Foo[A](x:Option[A]) - where A -> value class") {
					val w = Carry("Terri", Wrap("Hobbies", Some(Zoo("a",new Wrapper(12))).asInstanceOf[Option[Zoo[Wrapper]]], "all"))
					val x = Carry[Option[Truck[Boolean]]]("Terry", Wrap("Hobbies", None, "all"))
					val js = ScalaJack.render(w)
					val js2 = ScalaJack.render(x)
					val db = sjM.render(w)
					val db2 = sjM.render(x)
					js should equal("""{"s":"Terri","w":{"name":"Hobbies","data":{"name":"a","z":12},"stuff":"all"}}""")
					js2 should equal("""{"s":"Terry","w":{"name":"Hobbies","stuff":"all"}}""")
					db.toString should equal("""{ "s" : "Terri" , "w" : { "name" : "Hobbies" , "data" : { "name" : "a" , "z" : 12} , "stuff" : "all"}}""")
					db2.toString should equal("""{ "s" : "Terry" , "w" : { "name" : "Hobbies" , "stuff" : "all"}}""")
					ScalaJack.read[Carry[Option[Zoo[Wrapper]]]](js) should equal(w)
					ScalaJack.read[Carry[Option[Zoo[Wrapper]]]](js2) should equal(x)
					sjM.read[Carry[Option[Zoo[Wrapper]]]](db) should equal(w)
					sjM.read[Carry[Option[Zoo[Wrapper]]]](db2) should equal(x)
				}
			}
			describe("Basic trait support") {
				it("Parameter is a simple trait") {
					val w = Carry[Pop]("Surprise", Wrap("Yellow", Wow2("three",3), "Done"))
					val js = ScalaJack.render(w)
					val db = sjM.render(w)
					js should equal("""{"s":"Surprise","w":{"name":"Yellow","data":{"_hint":"co.blocke.scalajack.test.Wow2","x":"three","y":3},"stuff":"Done"}}""")
					db.toString should equal("""{ "s" : "Surprise" , "w" : { "name" : "Yellow" , "data" : { "_hint" : "co.blocke.scalajack.test.Wow2" , "x" : "three" , "y" : 3} , "stuff" : "Done"}}""")
					ScalaJack.read[Carry[Pop]](js) should equal(w)
					sjM.read[Carry[Pop]](db) should equal(w)
				}
				it("Parameter is List of trait") {
					val w = Carry[List[Pop]]("Surprise", Wrap("Yellow", List(Wow1("four",4),Wow2("three",3)), "Done"))
					val js = ScalaJack.render(w)
					val db = sjM.render(w)
					js should equal("""{"s":"Surprise","w":{"name":"Yellow","data":[{"_hint":"co.blocke.scalajack.test.Wow1","a":"four","b":4},{"_hint":"co.blocke.scalajack.test.Wow2","x":"three","y":3}],"stuff":"Done"}}""")
					db.toString should equal("""{ "s" : "Surprise" , "w" : { "name" : "Yellow" , "data" : [ { "_hint" : "co.blocke.scalajack.test.Wow1" , "a" : "four" , "b" : 4} , { "_hint" : "co.blocke.scalajack.test.Wow2" , "x" : "three" , "y" : 3}] , "stuff" : "Done"}}""")
					ScalaJack.read[Carry[List[Pop]]](js) should equal(w)
					sjM.read[Carry[List[Pop]]](db) should equal(w)
				}
				it("Parameter is Map of String->trait") {
					val w = Carry[Map[String,Pop]]("Surprise", Wrap("Yellow", Map("a"->Wow1("four",4),"b"->Wow2("three",3)), "Done"))
					val js = ScalaJack.render(w)
					val db = sjM.render(w)
					js should equal("""{"s":"Surprise","w":{"name":"Yellow","data":{"a":{"_hint":"co.blocke.scalajack.test.Wow1","a":"four","b":4},"b":{"_hint":"co.blocke.scalajack.test.Wow2","x":"three","y":3}},"stuff":"Done"}}""")
					db.toString should equal("""{ "s" : "Surprise" , "w" : { "name" : "Yellow" , "data" : { "a" : { "_hint" : "co.blocke.scalajack.test.Wow1" , "a" : "four" , "b" : 4} , "b" : { "_hint" : "co.blocke.scalajack.test.Wow2" , "x" : "three" , "y" : 3}} , "stuff" : "Done"}}""")
					ScalaJack.read[Carry[Map[String,Pop]]](js) should equal(w)
					sjM.read[Carry[Map[String,Pop]]](db) should equal(w)
				}
				it("Parameter is an Option of trait") {
					val w = Carry[Option[Pop]]("Terri", Wrap("Hobbies", Some(Wow1("ok",-99)), "all"))
					val x = Carry[Option[Pop]]("Terry", Wrap("Hobbies", None, "all"))
					val js = ScalaJack.render(w)
					val js2 = ScalaJack.render(x)
					val db = sjM.render(w)
					val db2 = sjM.render(x)
					js should equal("""{"s":"Terri","w":{"name":"Hobbies","data":{"_hint":"co.blocke.scalajack.test.Wow1","a":"ok","b":-99},"stuff":"all"}}""")
					js2 should equal("""{"s":"Terry","w":{"name":"Hobbies","stuff":"all"}}""")
					db.toString should equal("""{ "s" : "Terri" , "w" : { "name" : "Hobbies" , "data" : { "_hint" : "co.blocke.scalajack.test.Wow1" , "a" : "ok" , "b" : -99} , "stuff" : "all"}}""")
					db2.toString should equal("""{ "s" : "Terry" , "w" : { "name" : "Hobbies" , "stuff" : "all"}}""")
					ScalaJack.read[Carry[Option[Pop]]](js) should equal(w)
					ScalaJack.read[Carry[Option[Pop]]](js2) should equal(x)
					sjM.read[Carry[Option[Pop]]](db) should equal(w)
					sjM.read[Carry[Option[Pop]]](db2) should equal(x)
				}
				it("List of parameter, where parameter is a trait") {
					val w = BagList[Pop]("list",List(Wow1("A",1),Wow1("B",2)))
					val js = ScalaJack.render(w)
					val db = sjM.render(w)
					js should equal("""{"s":"list","many":[{"_hint":"co.blocke.scalajack.test.Wow1","a":"A","b":1},{"_hint":"co.blocke.scalajack.test.Wow1","a":"B","b":2}]}""")
					db.toString should equal("""{ "s" : "list" , "many" : [ { "_hint" : "co.blocke.scalajack.test.Wow1" , "a" : "A" , "b" : 1} , { "_hint" : "co.blocke.scalajack.test.Wow1" , "a" : "B" , "b" : 2}]}""")
					ScalaJack.read[BagList[Pop]](js) should equal(w)
					sjM.read[BagList[Pop]](db) should equal(w)
				}
				it("Map of String->parameter, where parameter is a trait") {
					val w = BagMap[Pop](5, Map("one"->Wow2("q",7),"two"->Wow1("r",3)))
					val js = ScalaJack.render(w)
					val db = sjM.render(w)
					js should equal("""{"i":5,"items":{"one":{"_hint":"co.blocke.scalajack.test.Wow2","x":"q","y":7},"two":{"_hint":"co.blocke.scalajack.test.Wow1","a":"r","b":3}}}""")
					db.toString should equal("""{ "i" : 5 , "items" : { "one" : { "_hint" : "co.blocke.scalajack.test.Wow2" , "x" : "q" , "y" : 7} , "two" : { "_hint" : "co.blocke.scalajack.test.Wow1" , "a" : "r" , "b" : 3}}}""")
					ScalaJack.read[BagMap[Pop]](js) should equal(w)
					sjM.read[BagMap[Pop]](db) should equal(w)
				}
				it("Option of parameter, where parameter is a trait") {
					val w = Carry[Option[Pop]]("Terri", Wrap("Hobbies", Some(Wow2("finite",1000)), "all"))
					val x = Carry[Option[Pop]]("Terry", Wrap("Hobbies", None, "all"))
					val js = ScalaJack.render(w)
					val js2 = ScalaJack.render(x)
					val db = sjM.render(w)
					val db2 = sjM.render(x)
					js should equal("""{"s":"Terri","w":{"name":"Hobbies","data":{"_hint":"co.blocke.scalajack.test.Wow2","x":"finite","y":1000},"stuff":"all"}}""")
					js2 should equal("""{"s":"Terry","w":{"name":"Hobbies","stuff":"all"}}""")
					db.toString should equal("""{ "s" : "Terri" , "w" : { "name" : "Hobbies" , "data" : { "_hint" : "co.blocke.scalajack.test.Wow2" , "x" : "finite" , "y" : 1000} , "stuff" : "all"}}""")
					db2.toString should equal("""{ "s" : "Terry" , "w" : { "name" : "Hobbies" , "stuff" : "all"}}""")
					ScalaJack.read[Carry[Option[Pop]]](js) should equal(w)
					ScalaJack.read[Carry[Option[Pop]]](js2) should equal(x)
					sjM.read[Carry[Option[Pop]]](db) should equal(w)
					sjM.read[Carry[Option[Pop]]](db2) should equal(x)
				}
			}
			describe("Advanced trait support -- parameters are traits, themselves having parameters") {
				it("Case class having an embedded parameterized trait") {
					val w = Breakfast(true, Toast(7,"Burnt"))
					val js = ScalaJack.render(w)
					val db = sjM.render(w)
					js should equal("""{"y":true,"bread":{"_hint":"co.blocke.scalajack.test.Toast","g":7,"yum":"Burnt"}}""")
					db.toString should equal("""{ "y" : true , "bread" : { "_hint" : "co.blocke.scalajack.test.Toast" , "g" : 7 , "yum" : "Burnt"}}""")
					ScalaJack.read[Breakfast[String]](js) should equal(w)
					sjM.read[Breakfast[String]](db) should equal(w)
				}
				it("Case class having an embedded parameterized trait, with the trait's parameter another case class") {
					val w = Breakfast(true, Toast(7,Two("two",true)))
					val js = ScalaJack.render(w)
					val db = sjM.render(w)
					js should equal("""{"y":true,"bread":{"_hint":"co.blocke.scalajack.test.Toast","g":7,"yum":{"foo":"two","bar":true}}}""")
					db.toString should equal("""{ "y" : true , "bread" : { "_hint" : "co.blocke.scalajack.test.Toast" , "g" : 7 , "yum" : { "foo" : "two" , "bar" : true}}}""")
					ScalaJack.read[Breakfast[Two]](js) should equal(w)
					sjM.read[Breakfast[Two]](db) should equal(w)
				}
				it("Case class having an embedded parameterized trait, with the trait's parameter a value class") {
					val w = Breakfast(true, Toast(7,new Wrapper(-100)))
					val js = ScalaJack.render(w)
					val db = sjM.render(w)
					js should equal("""{"y":true,"bread":{"_hint":"co.blocke.scalajack.test.Toast","g":7,"yum":-100}}""")
					db.toString should equal("""{ "y" : true , "bread" : { "_hint" : "co.blocke.scalajack.test.Toast" , "g" : 7 , "yum" : -100}}""")
					ScalaJack.read[Breakfast[Wrapper]](js) should equal(w)
					sjM.read[Breakfast[Wrapper]](db) should equal(w)
				}
				it("Parameter is a parameterized trait") { // I can't believe this one worked!
					val w = Carry[Tart[Soup[String]]]("Bill", Wrap("Betty", Bun(3,Cruton(8,"eight")),"ok"))
					val js = ScalaJack.render(w)
					val db = sjM.render(w)
					js should equal("""{"s":"Bill","w":{"name":"Betty","data":{"_hint":"co.blocke.scalajack.test.Bun","g":3,"yum":{"_hint":"co.blocke.scalajack.test.Cruton","i":8,"sweet":"eight"}},"stuff":"ok"}}""")
					db.toString should equal("""{ "s" : "Bill" , "w" : { "name" : "Betty" , "data" : { "_hint" : "co.blocke.scalajack.test.Bun" , "g" : 3 , "yum" : { "_hint" : "co.blocke.scalajack.test.Cruton" , "i" : 8 , "sweet" : "eight"}} , "stuff" : "ok"}}""")
					ScalaJack.read[Carry[Tart[Soup[String]]]](js) should equal(w)
					sjM.read[Carry[Tart[Soup[String]]]](db) should equal(w)
				}
				it("Parameter is List of parameterized trait") {
					val w = Carry[List[Tart[Boolean]]]("Trey", Wrap("Hobbies", List(Bun(1,false),Toast(2,true)), "all"))
					val js = ScalaJack.render(w)
					val db = sjM.render(w)
					js should equal("""{"s":"Trey","w":{"name":"Hobbies","data":[{"_hint":"co.blocke.scalajack.test.Bun","g":1,"yum":false},{"_hint":"co.blocke.scalajack.test.Toast","g":2,"yum":true}],"stuff":"all"}}""")
					db.toString should equal("""{ "s" : "Trey" , "w" : { "name" : "Hobbies" , "data" : [ { "_hint" : "co.blocke.scalajack.test.Bun" , "g" : 1 , "yum" : false} , { "_hint" : "co.blocke.scalajack.test.Toast" , "g" : 2 , "yum" : true}] , "stuff" : "all"}}""")
					ScalaJack.read[Carry[List[Tart[Boolean]]]](js) should equal(w)
					sjM.read[Carry[List[Tart[Boolean]]]](db) should equal(w)
				}
				it("Parameter is Map of String->parameterized trait") {
					val w = Carry[Map[String,Tart[String]]]("Troy", Wrap("Articles", Map("OK"->Bun(27,"Hot")), "all"))
					val js = ScalaJack.render(w)
					val db = sjM.render(w)
					js should equal("""{"s":"Troy","w":{"name":"Articles","data":{"OK":{"_hint":"co.blocke.scalajack.test.Bun","g":27,"yum":"Hot"}},"stuff":"all"}}""")
					db.toString should equal("""{ "s" : "Troy" , "w" : { "name" : "Articles" , "data" : { "OK" : { "_hint" : "co.blocke.scalajack.test.Bun" , "g" : 27 , "yum" : "Hot"}} , "stuff" : "all"}}""")
					ScalaJack.read[Carry[Map[String,Tart[String]]]](js) should equal(w)
					sjM.read[Carry[Map[String,Tart[String]]]](db) should equal(w)
				}
				it("Parameter is an Option of parameterized trait") {
					val w = Carry[Option[Tart[Int]]]("Terri", Wrap("Hobbies", Some(Toast(11,12)), "all"))
					val x = Carry[Option[Tart[Int]]]("Terry", Wrap("Hobbies", None, "all"))
					val js = ScalaJack.render(w)
					val js2 = ScalaJack.render(x)
					val db = sjM.render(w)
					val db2 = sjM.render(x)
					js should equal("""{"s":"Terri","w":{"name":"Hobbies","data":{"_hint":"co.blocke.scalajack.test.Toast","g":11,"yum":12},"stuff":"all"}}""")
					js2 should equal("""{"s":"Terry","w":{"name":"Hobbies","stuff":"all"}}""")
					db.toString should equal("""{ "s" : "Terri" , "w" : { "name" : "Hobbies" , "data" : { "_hint" : "co.blocke.scalajack.test.Toast" , "g" : 11 , "yum" : 12} , "stuff" : "all"}}""")
					db2.toString should equal("""{ "s" : "Terry" , "w" : { "name" : "Hobbies" , "stuff" : "all"}}""")
					ScalaJack.read[Carry[Option[Tart[Int]]]](js) should equal(w)
					ScalaJack.read[Carry[Option[Tart[Int]]]](js2) should equal(x)
					sjM.read[Carry[Option[Tart[Int]]]](db) should equal(w)
					sjM.read[Carry[Option[Tart[Int]]]](db2) should equal(x)
				}
				it("List of parameter, where parameter is a parameterized trait") {
					val w = BagList[Tart[Boolean]]("list",List(Toast(1,true),Bun(2,false)))
					val js = ScalaJack.render(w)
					val db = sjM.render(w)
					js should equal("""{"s":"list","many":[{"_hint":"co.blocke.scalajack.test.Toast","g":1,"yum":true},{"_hint":"co.blocke.scalajack.test.Bun","g":2,"yum":false}]}""")
					db.toString should equal("""{ "s" : "list" , "many" : [ { "_hint" : "co.blocke.scalajack.test.Toast" , "g" : 1 , "yum" : true} , { "_hint" : "co.blocke.scalajack.test.Bun" , "g" : 2 , "yum" : false}]}""")
					ScalaJack.read[BagList[Tart[Boolean]]](js) should equal(w)
					sjM.read[BagList[Tart[Boolean]]](db) should equal(w)
				}
				it("Map of String->parameter, where parameter is a parameterized trait") {
					val w = BagMap[Tart[Boolean]](5, Map("one"->Bun(1,true),"two"->Toast(2,false)))
					val js = ScalaJack.render(w)
					val db = sjM.render(w)
					js should equal("""{"i":5,"items":{"one":{"_hint":"co.blocke.scalajack.test.Bun","g":1,"yum":true},"two":{"_hint":"co.blocke.scalajack.test.Toast","g":2,"yum":false}}}""")
					db.toString should equal("""{ "i" : 5 , "items" : { "one" : { "_hint" : "co.blocke.scalajack.test.Bun" , "g" : 1 , "yum" : true} , "two" : { "_hint" : "co.blocke.scalajack.test.Toast" , "g" : 2 , "yum" : false}}}""")
					ScalaJack.read[BagMap[Tart[Boolean]]](js) should equal(w)
					sjM.read[BagMap[Tart[Boolean]]](db) should equal(w)
				}
				it("Option of parameter, where parameter is a parameterized trait") {
					val w = BagOpt[Tart[String]](1, Some(Bun(6,"ok")))
					val x = BagOpt[Tart[String]](1,None)
					val js = ScalaJack.render(w)
					val js2 = ScalaJack.render(x)
					val db = sjM.render(w)
					val db2 = sjM.render(x)
					js should equal("""{"i":1,"maybe":{"_hint":"co.blocke.scalajack.test.Bun","g":6,"yum":"ok"}}""")
					js2 should equal("""{"i":1}""")
					db.toString should equal("""{ "i" : 1 , "maybe" : { "_hint" : "co.blocke.scalajack.test.Bun" , "g" : 6 , "yum" : "ok"}}""")
					db2.toString should equal("""{ "i" : 1}""")
					ScalaJack.read[BagOpt[Tart[String]]](js) should equal(w)
					ScalaJack.read[BagOpt[Tart[String]]](js2) should equal(x)
					sjM.read[BagOpt[Tart[String]]](db) should equal(w)
					sjM.read[BagOpt[Tart[String]]](db2) should equal(x)
				}
			}
		}
		/*
		describe("Improved Error Reporting") {
			describe("Object") {
				it("Must provide useful errors - simple case class") {
					val js = """{"a":"Foo","b":"Bar"}"""
					Try( ScalaJack.read[Wow1](js) ).failed.get.getMessage should be( "Class co.blocke.scalajack.test.Wow1 field b Expected VALUE_NUMBER_INT and saw VALUE_STRING" )
				}
				it("Must provide useful errors - list member of class") {
					val js = """{"stuff":[5],"things":{"a":5}}"""
					Try( ScalaJack.read[Four](js) ).failed.get.getMessage should be( "Class co.blocke.scalajack.test.Four field stuff Expected VALUE_STRING and saw VALUE_NUMBER_INT" )
				}
				it("Must provide useful errors - map member of class") {
					val js = """{"stuff":["hey"],"things":{"a":true}}"""
					Try( ScalaJack.read[Four](js) ).failed.get.getMessage should be( "Class co.blocke.scalajack.test.Four field things Expected VALUE_NUMBER_INT and saw VALUE_TRUE" )
				}
				it("Must provide useful errors - list of case class member of class") {
					val js = """{"s":"hey","many":[{"age":33},{"age":"old"}]}"""
					Try( ScalaJack.read[BagList[Hey]](js) ).failed.get.getMessage should be( "Class co.blocke.scalajack.test.Hey field age Expected VALUE_NUMBER_INT and saw VALUE_STRING" )
				}
				it("Must provide useful errors - map of case class member of class") {
					val js = """{"i":5,"items":{"one":{"age":33},"two":{"age":"old"}}}"""
					Try( ScalaJack.read[BagMap[Hey]](js) ).failed.get.getMessage should be( "Class co.blocke.scalajack.test.Hey field age Expected VALUE_NUMBER_INT and saw VALUE_STRING" )
				}
				it("Must provide useful errors - Option member of class") {
					val js = """{"name":"Bob","big":5,"maybe":false}"""
					Try( ScalaJack.read[OneSub1](js) ).failed.get.getMessage should be( "Class co.blocke.scalajack.test.OneSub1 field maybe Expected VALUE_STRING and saw VALUE_FALSE" )
				}
				it("Must provide useful errors - Option of calue class") {
					val js = """{"name":"Bob","wrap":false}"""
					Try( ScalaJack.read[OptValSupport](js) ).failed.get.getMessage should be( "Class co.blocke.scalajack.test.OptValSupport field wrap Expected VALUE_NUMBER_INT and saw VALUE_FALSE" )
				}
				it("Must provide useful errors - Option of case class member of class") {
					val js = """{"i":5,"maybe":{"age":"boom"}}"""
					Try( ScalaJack.read[BagOpt[Hey]](js) ).failed.get.getMessage should be( "Class co.blocke.scalajack.test.Hey field age Expected VALUE_NUMBER_INT and saw VALUE_STRING" )
				}
				it("Must provide useful errors - Enumeration member of class (wrong type)") {
					val js = """{"age":5, "num":true}"""
					Try( ScalaJack.read[Numy](js) ).failed.get.getMessage should be( "Class co.blocke.scalajack.test.Numy field num Expected VALUE_STRING (enum) and saw VALUE_TRUE" )
				}
				it("Must provide useful errors - Enumeration member of class (right type, but not enum value") {
					val js = """{"age":5, "num":"P"}"""
					Try( ScalaJack.read[Numy](js) ).failed.get.getMessage should be( "Class co.blocke.scalajack.test.Numy field num Given value of P is not valid for this enum field." )
				}
				it("Must provide useful errors - Naked list") {
					val js = """["a","b",5]"""
					Try( ScalaJack.read[List[String]](js) ).failed.get.getMessage should be( "Class scala.collection.immutable.List field  Expected VALUE_STRING and saw VALUE_NUMBER_INT" )
				}
				it("Must provide useful errors - Naked list of case class") {
					val js = """[{"age":55},{"age":"bar"}]"""
					Try( ScalaJack.read[List[Hey]](js) ).failed.get.getMessage should be( "Class co.blocke.scalajack.test.Hey field age Expected VALUE_NUMBER_INT and saw VALUE_STRING" )
				}
				it("Must provide useful errors - Nested case class") {
					val js = """{"_hint":"co.blocke.scalajack.test.Cruton","i":5,"sweet":{"age":false}}"""
					Try( ScalaJack.read[Soup[Hey]](js) ).failed.get.getMessage should be( "Class co.blocke.scalajack.test.Hey field age Expected VALUE_NUMBER_INT and saw VALUE_FALSE" )
				}
			}
			/*zzz
			describe("DB Object") {
				it("Must provide useful errors - simple case class") {
					val js = Map("a"->"Foo","b"->"Bar")
					Try( sjM.read[Wow1](js) ).failed.get.getMessage should be( "Class co.blocke.scalajack.test.Wow1 field b Expected VALUE_NUMBER_INT and saw java.lang.String" )
				}
				it("Must provide useful errors - list member of class") {
					val js = Map("stuff"->List(5),"things"->Map("a"->5))
					Try( sjM.read[Four](js) ).failed.get.getMessage should be( "Class co.blocke.scalajack.test.Four field stuff Expected VALUE_STRING and saw VALUE_NUMBER_INT" )
				}
				it("Must provide useful errors - map member of class") {
					val js = """{"stuff":["hey"],"things":{"a":true}}"""
					Try( sjM.read[Four](js) ).failed.get.getMessage should be( "Class co.blocke.scalajack.test.Four field things Expected VALUE_NUMBER_INT and saw VALUE_TRUE" )
				}
				it("Must provide useful errors - list of case class member of class") {
					val js = """{"s":"hey","many":[{"age":33},{"age":"old"}]}"""
					Try( sjM.read[BagList[Hey]](js) ).failed.get.getMessage should be( "Class co.blocke.scalajack.test.Hey field age Expected VALUE_NUMBER_INT and saw VALUE_STRING" )
				}
				it("Must provide useful errors - map of case class member of class") {
					val js = """{"i":5,"items":{"one":{"age":33},"two":{"age":"old"}}}"""
					Try( sjM.read[BagMap[Hey]](js) ).failed.get.getMessage should be( "Class co.blocke.scalajack.test.Hey field age Expected VALUE_NUMBER_INT and saw VALUE_STRING" )
				}
				it("Must provide useful errors - Option member of class") {
					val js = """{"name":"Bob","big":5,"maybe":false}"""
					Try( sjM.read[OneSub1](js) ).failed.get.getMessage should be( "Class co.blocke.scalajack.test.OneSub1 field maybe Expected VALUE_STRING and saw VALUE_FALSE" )
				}
				it("Must provide useful errors - Option of calue class") {
					val js = """{"name":"Bob","wrap":false}"""
					Try( sjM.read[OptValSupport](js) ).failed.get.getMessage should be( "Class co.blocke.scalajack.test.OptValSupport field wrap Expected VALUE_NUMBER_INT and saw VALUE_FALSE" )
				}
				it("Must provide useful errors - Option of case class member of class") {
					val js = """{"i":5,"maybe":{"age":"boom"}}"""
					Try( sjM.read[BagOpt[Hey]](js) ).failed.get.getMessage should be( "Class co.blocke.scalajack.test.Hey field age Expected VALUE_NUMBER_INT and saw VALUE_STRING" )
				}
				it("Must provide useful errors - Enumeration member of class (wrong type)") {
					val js = """{"age":5, "num":true}"""
					Try( sjM.read[Numy](js) ).failed.get.getMessage should be( "Class co.blocke.scalajack.test.Numy field num Expected VALUE_STRING (enum) and saw VALUE_TRUE" )
				}
				it("Must provide useful errors - Enumeration member of class (right type, but not enum value") {
					val js = """{"age":5, "num":"P"}"""
					Try( sjM.read[Numy](js) ).failed.get.getMessage should be( "Class co.blocke.scalajack.test.Numy field num Given value of P is not valid for this enum field." )
				}
				it("Must provide useful errors - Naked list") {
					val js = """["a","b",5]"""
					Try( ScalaJack.readListDB[String](js) ).failed.get.getMessage should be( "Class scala.collection.List field  Expected VALUE_STRING and saw VALUE_NUMBER_INT" )
				}
				it("Must provide useful errors - Naked list of case class") {
					val js = """[{"age":55},{"age":"bar"}]"""
					Try( ScalaJack.readListDB[Hey](js) ).failed.get.getMessage should be( "Class co.blocke.scalajack.test.Hey field age Expected VALUE_NUMBER_INT and saw VALUE_STRING" )
				}
				it("Must provide useful errors - Nested case class") {
					val js = """{"_hint":"co.blocke.scalajack.test.Cruton","i":5,"sweet":{"age":false}}"""
					Try( sjM.read[Soup[Hey]](js) ).failed.get.getMessage should be( "Class co.blocke.scalajack.test.Hey field age Expected VALUE_NUMBER_INT and saw VALUE_FALSE" )
				}
			}
			zzz*/
		}
		*/
		describe("Thread Safety Test") {
			it("Should not crash when multiple threads access Analyzer (Scala 2.10.x reflection bug)") {
				import scala.concurrent.{Future, Await}
				import scala.concurrent.ExecutionContext.Implicits.global
				import scala.concurrent.duration._
				import scala.language.postfixOps
				val doit = () =>
					Try {
						val js = ScalaJack.render( Foo("Greg",List("a","b","c")) )
						ScalaJack.read[Foo](js)
						}.toOption.isDefined
				val z = List(
					Future (doit()),
					Future (doit()),
					Future (doit()),
					Future (doit())
					)
				val res = Await.result(Future.sequence(z), 3 seconds).reduce((a,b) => a && b)
				res should be( true )
			}
		}
	}
}
