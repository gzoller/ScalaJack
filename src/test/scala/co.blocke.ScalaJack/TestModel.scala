package co.blocke.scalajack
package test

object Num extends Enumeration {
	val A,B,C = Value
}

case class One( 
	name     : String, 
	stuff    : List[String],
	more     : List[Two],
	nest     : Two,
	maybe    : Option[String],
	mymap    : Map[String,Int],
	flipflop : Boolean,
	big      : Long,
	num      : Num.Value,
	age      : Int
    )

case class Two(
	foo      : String,
	bar      : Boolean
	)

case class Three( 
	name:String, 
	two:Num.Value,
	pp:Pop 
	)

case class Four(
	stuff : List[String],
	things : Map[String,Int]
	)

case class Five( 
	@MongoKey name:String, 
	two : Two
	)
	
trait Pop {
	def go
}

case class Wow1( a:String, b:Int) extends Pop {
	def go { println("--1--") }
}
case class Wow2( x:String, y:Int) extends Pop {
	def go { println("--2--") }
}

case class Animal(val name: String, val legs: Int)

// Value class support
class Wrapper(val underlying: Int) extends AnyVal
case class ValSupport( name:String, wrap:Wrapper )

// Test Lists
case class ListList(val name: String, val stuff: List[List[Animal]])
case class ListListList(val name: String, val stuff: List[List[List[Animal]]])
case class ListOpt(val name: String, val stuff: List[Option[Animal]])
case class ListMap(val name: String, val stuff: List[Map[String, Animal]])

// Test nested Options+Variants w/other collections
case class OpOp(val name: String, val opts: Option[Option[Animal]])
case class OpList(val name: String, val opList: Option[List[Animal]])
case class OpListList(val name: String, val opListList: Option[List[List[Animal]]])
case class OpMap(val name: String, val opMap: Option[Map[String, Animal]])

// Test nested Maps+Variants w/other collections
case class MapList(val name: String, val mapList: Map[String, List[Animal]])
case class MapListList(val name: String, val mapList: Map[String, List[List[Animal]]])
case class MapOpt(val name: String, val mapOpt: Map[String, Option[Animal]])
case class MapMap(val name: String, val mapmap: Map[String, Map[String, Animal]])
