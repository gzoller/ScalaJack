package co.blocke.scalajack
package test

import com.fasterxml.jackson.core._
import org.bson.types.ObjectId

object Num extends Enumeration {
	val A,B,C = Value
}

case class Wrap[T,U] (
		name:String,
		data:T,
		stuff:U
		)

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

case class Six( 
	@MongoKey name:String, 
	@MongoKey num:Int,
	two : Two
	)

case class Seven( 
	_id:ObjectId, 
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

// Value class support w/custom rendering
class Wrapper(val underlying: Int) extends AnyVal
case class ValSupport( name:String, wrap:Wrapper, more:Boolean )
object Wrapper extends ExtJson {
	override def toJson( obj:Any ) : String = "{\"num\":"+obj.asInstanceOf[Int]+",\"hey\":\"you\"}"
	override def fromJson( valueType:Field, jp:JsonParser, ext:Boolean, hint:String ) : Any = {
		jp.nextToken // consume '{'
		jp.getCurrentName // consume 'num' label
		jp.nextToken // scan to value
		val v = jp.getValueAsInt // consume 'num' value
		while( jp.getCurrentToken != JsonToken.END_OBJECT ) {
			jp.nextToken
		}
		jp.nextToken // consume '}'
		v
	}
}
case class ListValSupport( name:String, wrap:List[Wrapper], more:Boolean )
case class OptValSupport( name:String, wrap:Option[Wrapper] )
case class MapValSupport( name:String, wrap:Map[String,Wrapper])

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
