package co.nubilus.scalajack
package test

import org.scalatest.{ FunSpec, GivenWhenThen, BeforeAndAfterAll }
import org.scalatest.matchers.MustMatchers

class TestSpec extends FunSpec with MustMatchers with GivenWhenThen with BeforeAndAfterAll {

	val data = Map("mymap"->Map("hey"->17,"you"->21),"nest"->Two("Nest!",true),"num"->"B","maybe"->Some("wow"),"name"->"Greg","flipflop"->true,"big"->99123986123L,"more"->List(Two("x",false),Two("y",true)),"stuff"->List("a","b"),"num"->Num.C,"age"->46)

	describe("====================\n| -- JSON Tests -- |\n====================") {
		it( "Serialize simple object to JSON -- all supported data types" ) {
			val a = ScalaJack.poof[One]( data )
			val js = ScalaJack.render(a)
			js must equal( """{"name":"Greg","stuff":["a","b"],"more":[{"foo":"x","bar":false},{"foo":"y","bar":true}],"nest":{"foo":"Nest!","bar":true},"maybe":"wow","mymap":{"hey":17,"you":21},"flipflop":true,"big":99123986123,"num":"C","age":46}""" )
		}
		it( "De-serialize simple object from JSON -- all supported data types" ) {
			val a = ScalaJack.poof[One]( data )
			val js = ScalaJack.render(a)
			js must equal( """{"name":"Greg","stuff":["a","b"],"more":[{"foo":"x","bar":false},{"foo":"y","bar":true}],"nest":{"foo":"Nest!","bar":true},"maybe":"wow","mymap":{"hey":17,"you":21},"flipflop":true,"big":99123986123,"num":"C","age":46}""" )
			val b = ScalaJack.read[One](js)
			b must equal( a )
		}
		it( "Handle empty Lists & Maps") {
			val four = Four(List[String](), Map[String,Int]())
			val js = ScalaJack.render(four)
			js must equal( """{"stuff":[],"things":{}}""" )
			ScalaJack.read[Four](js) must equal( four )
		}
		it( "Traits" ) {
			val t = Three("three",Num.A,Wow1("foo",17))
			val js2 = ScalaJack.render(t)
			js2 must equal( """{"name":"three","two":"A","pp":{"_hint":"co.nubilus.scalajack.test.Wow1","a":"foo","b":17}}""" )
			val u = ScalaJack.read[Three](js2)
			u must equal( t )
		}
		it( "MongoKey Annotation (_id field generation) - switch on" ) {
			val five = Five("Fred",Two("blah",true))
			val js = ScalaJack.render(five,true)
			js must equal( """{"_id":"Fred","two":{"foo":"blah","bar":true}}""" )
			ScalaJack.read[Five](js) must equal( five )
		}
		it( "MongoKey Annotation (_id field generation) - switch off" ) {
			val five = Five("Fred",Two("blah",true))
			val js = ScalaJack.render(five)
			js must equal( """{"name":"Fred","two":{"foo":"blah","bar":true}}""" )
			ScalaJack.read[Five](js) must equal( five )
		}

		// ------------- Nested Combinations
		it( "Serialize list of lists of case classes" ) {
			val ln = ListList("Fred", List(List(Animal("mouse", 4), Animal("bug", 6)), List(Animal("whale", 0), Animal("elephant", 4))))
			val js = ScalaJack.render(ln)
			js must equal( """{"name":"Fred","stuff":[[{"name":"mouse","legs":4},{"name":"bug","legs":6}],[{"name":"whale","legs":0},{"name":"elephant","legs":4}]]}""" )
			ScalaJack.read[ListList](js) must equal( ln )
		}
		it( "Serialize list of lists of lists of case classes" ) {
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
			js must equal( """{"name":"Fred","stuff":[[[{"name":"mouse","legs":4},{"name":"bug","legs":6}],[{"name":"whale","legs":0},{"name":"elephant","legs":4}]],[[{"name":"millipede","legs":1000},{"name":"slug","legs":0}],[{"name":"bird","legs":2},{"name":"tiger","legs":4}]]]}""" )
			ScalaJack.read[ListListList](js) must equal( ln ) 
		}
		// NOTE: If your list has a None it it, this will be lost upon re-marshal from JSON as JSON has no representation
		//       for a None (it's simply missing from the list).
		it( "Serialize list of option of case classes" ) {
			val lop = ListOpt("Jenny", List(Some(Animal("mouse", 4)), None, Some(Animal("whale", 0))))
			val js = ScalaJack.render(lop)
			js must equal( """{"name":"Jenny","stuff":[{"name":"mouse","legs":4},{"name":"whale","legs":0}]}""" )
			ScalaJack.read[ListOpt](js) must equal( lop.copy(stuff = lop.stuff.filter(_.isDefined) ) )
		}
		it( "Serialize list of map of case classes" ) {
			val lm = ListMap("Jenny", List(Map("a" -> Animal("mouse", 4)), Map("b" -> Animal("whale", 0))))
			val js = ScalaJack.render(lm)
			js must equal( """{"name":"Jenny","stuff":[{"a":{"name":"mouse","legs":4}},{"b":{"name":"whale","legs":0}}]}""" )
			ScalaJack.read[ListMap](js) must equal( lm )
		}
		it( "Serialize an option of list of case classes" ) {
			val oln = OpList("Wow", Some(List(Animal("mouse", 4), Animal("bug", 6))))
			val js = ScalaJack.render(oln)
			js must equal( """{"name":"Wow","opList":[{"name":"mouse","legs":4},{"name":"bug","legs":6}]}""" )
			ScalaJack.read[OpList](js) must equal( oln )
		}
		it( "Serialize an option of nested list of case classes" ) {
			val oln = OpListList("Yay", Some(List(List(Animal("mouse", 4), Animal("bug", 6)), List(Animal("whale", 0), Animal("elephant", 4)))))
			val js = ScalaJack.render(oln)
			js must equal( """{"name":"Yay","opListList":[[{"name":"mouse","legs":4},{"name":"bug","legs":6}],[{"name":"whale","legs":0},{"name":"elephant","legs":4}]]}""" )
			ScalaJack.read[OpListList](js) must equal( oln )
		}
		it( "Serialize an option of map of case classes" ) {
			val om = OpMap("Wow", Some(Map("hello" -> (Animal("mouse", 4)))))
			val js = ScalaJack.render(om)
			js must equal( """{"name":"Wow","opMap":{"hello":{"name":"mouse","legs":4}}}""" )
			ScalaJack.read[OpMap](js) must equal( om )
			val om2 = OpMap("Wow", None)
			val js2 = ScalaJack.render(om2)
			js2 must equal( """{"name":"Wow"}""" )
			ScalaJack.read[OpMap](js2) must equal( om2 )
		}
		it( "Serialize a nested option of case classes" ) {
			val oop = OpOp("Oops", Some(Some(Animal("mouse", 4))))
			val js = ScalaJack.render(oop)
			js must equal( """{"name":"Oops","opts":{"name":"mouse","legs":4}}""" )
			ScalaJack.read[OpOp](js) must equal( oop )
			val oop2 = OpOp("Oops", None)
			val js2 = ScalaJack.render(oop2)
			js2 must equal( """{"name":"Oops"}""" )
			ScalaJack.read[OpOp](js2) must equal( oop2 )
		}
		it( "Serialize a map of list of case classes" ) {
			val mln = MapList("Bob", Map("Mike" -> List(Animal("mouse", 4), Animal("bug", 6)), "Sally" -> List(Animal("whale", 0), Animal("elephant", 4))))
			val js = ScalaJack.render(mln)
			js must equal( """{"name":"Bob","mapList":{"Mike":[{"name":"mouse","legs":4},{"name":"bug","legs":6}],"Sally":[{"name":"whale","legs":0},{"name":"elephant","legs":4}]}}""" )
			ScalaJack.read[MapList](js) must equal( mln )
		}
		it( "Serialize a map of nested lists of case classes" ) {
			val mln = MapListList("Bob", Map("Everyone" -> List(List(Animal("mouse", 4), Animal("bug", 6)), List(Animal("whale", 0), Animal("elephant", 4)))))
			val js = ScalaJack.render(mln)
			js must equal( """{"name":"Bob","mapList":{"Everyone":[[{"name":"mouse","legs":4},{"name":"bug","legs":6}],[{"name":"whale","legs":0},{"name":"elephant","legs":4}]]}}""" )
			ScalaJack.read[MapListList](js) must equal( mln )
		}
		it( "Serialize a map of option of case classes" ) {
			val a: Option[Animal] = None
			val mln = MapOpt("Bob", Map("things" -> Some(Animal("mouse", 4)), "otherthings" -> a))
			val js = ScalaJack.render(mln)
			println(js)
			js must equal( """{"name":"Bob","mapOpt":{"things":{"name":"mouse","legs":4}}}""" )
			ScalaJack.read[MapOpt](js) must equal( mln.copy(mapOpt = mln.mapOpt.filter({ case (k, v) => v.isDefined })) )
		}
		it( "Serialize a map of map of case classes" ) {
			val mm = MapMap("Bob", Map("things" -> Map("a" -> Animal("mouse", 4), "b" -> Animal("horse", 4)), "stuff" -> Map("c" -> Animal("sloth", 2))))
			val js = ScalaJack.render(mm)
			js must equal( """{"name":"Bob","mapmap":{"things":{"a":{"name":"mouse","legs":4},"b":{"name":"horse","legs":4}},"stuff":{"c":{"name":"sloth","legs":2}}}}""" )
			ScalaJack.read[MapMap](js) must equal( mm )
		}
	}
}