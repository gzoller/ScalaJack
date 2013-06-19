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
		it( "MongoKey Annotation (_id field generation)" ) {
			val five = Five("Fred",Two("blah",true))
			val js = ScalaJack.render(five)
			js must equal( """{"_id":"Fred","two":{"foo":"blah","bar":true}}""" )
			ScalaJack.read[Five](js) must equal( five )
		}

		// ------------- Nested Combinations
		it( "Serialize lists of case classes" ) {
			val ln = ListList("Fred", List(List(Animal("mouse", 4), Animal("bug", 6)), List(Animal("whale", 0), Animal("elephant", 4))))
			val js = ScalaJack.render(ln)
			println(js)
			js must equal( """{"name":"Fred","stuff":[[{"name":"mouse","legs":4},{"name":"bug","legs":6}],[{"name":"whale","legs":0},{"name":"elephant","legs":4}]]}""" )
			ScalaJack.read[ListList](js) must equal( ln )
		}

		/*
      "serialize nested lists of case classes" in {
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
        val js = grater[ListListList].toCompactJSON(ln)
        js must_== """{"name":"Fred","stuff":[[[{"name":"mouse","legs":4},{"name":"bug","legs":6}],[{"name":"whale","legs":0},{"name":"elephant","legs":4}]],[[{"name":"millipede","legs":1000},{"name":"slug","legs":0}],[{"name":"bird","legs":2},{"name":"tiger","legs":4}]]]}"""
        grater[ListListList].fromJSON(js) must_== ln
      }
      // NOTE: If your list has a None it it, this will be lost upon re-marshal from JSON as JSON has no representation
      //       for a None (it's simply missing from the list).
      "serialize a list of option of case classe" in {
        val lop = ListOpt("Jenny", List(Some(Animal("mouse", 4)), None, Some(Animal("whale", 0))))
        val js = grater[ListOpt].toCompactJSON(lop)
        js must_== """{"name":"Jenny","stuff":[{"name":"mouse","legs":4},{"name":"whale","legs":0}]}"""
        grater[ListOpt].fromJSON(js) must_== lop.copy(stuff = lop.stuff.filter(_.isDefined))
      }
      "serialize a list of map of case classe" in {
        val lm = ListMap("Jenny", List(Map("a" -> Animal("mouse", 4)), Map("b" -> Animal("whale", 0))))
        val js = grater[ListMap].toCompactJSON(lm)
        js must_== """{"name":"Jenny","stuff":[{"a":{"name":"mouse","legs":4}},{"b":{"name":"whale","legs":0}}]}"""
        grater[ListMap].fromJSON(js) must_== lm
      }
      "serialize an option of list of case classes" in {
        val oln = OpList("Wow", Some(List(Animal("mouse", 4), Animal("bug", 6))))
        val js = grater[OpList].toCompactJSON(oln)
        js must_== """{"name":"Wow","opList":[{"name":"mouse","legs":4},{"name":"bug","legs":6}]}"""
        grater[OpList].fromJSON(js) must_== oln
      }
      "serailize an option of nested lists of case classes" in {
        val oln = OpListList("Yay", Some(List(List(Animal("mouse", 4), Animal("bug", 6)), List(Animal("whale", 0), Animal("elephant", 4)))))
        val js = grater[OpListList].toCompactJSON(oln)
        js must_== """{"name":"Yay","opListList":[[{"name":"mouse","legs":4},{"name":"bug","legs":6}],[{"name":"whale","legs":0},{"name":"elephant","legs":4}]]}"""
        grater[OpListList].fromJSON(js) must_== oln
      }
      "serailize an option of map of case classes" in {
        val om = OpMap("Wow", Some(Map("hello" -> (Animal("mouse", 4)))))
        val js = grater[OpMap].toCompactJSON(om)
        js must_== """{"name":"Wow","opMap":{"hello":{"name":"mouse","legs":4}}}"""
        grater[OpMap].fromJSON(js) must_== om
        val om2 = OpMap("Wow", None)
        val js2 = grater[OpMap].toCompactJSON(om2)
        js2 must_== """{"name":"Wow"}"""
        grater[OpMap].fromJSON(js2) must_== om2
      }
      "serailize an nested option of case class" in {
        val oop = OpOp("Oops", Some(Some(Animal("mouse", 4))))
        val js = grater[OpOp].toCompactJSON(oop)
        js must_== """{"name":"Oops","opts":{"name":"mouse","legs":4}}"""
        grater[OpOp].fromJSON(js) must_== oop
        val oop2 = OpOp("Oops", None)
        val js2 = grater[OpOp].toCompactJSON(oop2)
        js2 must_== """{"name":"Oops"}"""
        grater[OpOp].fromJSON(js2) must_== oop2
      }
      "serialize a map of list of case classes" in {
        val mln = MapList("Bob", Map("Mike" -> List(Animal("mouse", 4), Animal("bug", 6)), "Sally" -> List(Animal("whale", 0), Animal("elephant", 4))))
        val js = grater[MapList].toCompactJSON(mln)
        js must_== """{"name":"Bob","mapList":{"Mike":[{"name":"mouse","legs":4},{"name":"bug","legs":6}],"Sally":[{"name":"whale","legs":0},{"name":"elephant","legs":4}]}}"""
        grater[MapList].fromJSON(js) must_== mln
      }
      "serialize a map of nested lists of case classes" in {
        val mln = MapListList("Bob", Map("Everyone" -> List(List(Animal("mouse", 4), Animal("bug", 6)), List(Animal("whale", 0), Animal("elephant", 4)))))
        val js = grater[MapListList].toCompactJSON(mln)
        js must_== """{"name":"Bob","mapList":{"Everyone":[[{"name":"mouse","legs":4},{"name":"bug","legs":6}],[{"name":"whale","legs":0},{"name":"elephant","legs":4}]]}}"""
        grater[MapListList].fromJSON(js) must_== mln
      }
      "serialize a map of option of case classe" in {
        val a: Option[Animal] = None
        val mln = MapOpt("Bob", Map("things" -> Some(Animal("mouse", 4)), "otherthings" -> a))
        val js = grater[MapOpt].toCompactJSON(mln)
        js must_== """{"name":"Bob","mapOpt":{"things":{"name":"mouse","legs":4}}}"""
        grater[MapOpt].fromJSON(js) must_== mln.copy(mapOpt = mln.mapOpt.filter({ case (k, v) => v.isDefined }))
      }
      "serialize a map of map of case class" in {
        val mm = MapMap("Bob", Map("things" -> Map("a" -> Animal("mouse", 4), "b" -> Animal("horse", 4)), "stuff" -> Map("c" -> Animal("sloth", 2))))
        val js = grater[MapMap].toCompactJSON(mm)
        js must_== """{"name":"Bob","mapmap":{"things":{"a":{"name":"mouse","legs":4},"b":{"name":"horse","legs":4}},"stuff":{"c":{"name":"sloth","legs":2}}}}"""
        grater[MapMap].fromJSON(js) must_== mm
      }
		*/
	}
}