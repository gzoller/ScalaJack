package co.blocke.scalajack
package mongo
package test

import java.time.{ OffsetDateTime, YearMonth }

object Num extends Enumeration {
  val A, B, C = Value
}

case class Bar[A, B](a: A, b: B)
case class Zoo[U](name: String, z: U) //stuff:Bar[U,String])
case class Hey(age: Int)

case class Wrap[T, U](
  name:  String,
  data:  T,
  stuff: U
)
case class Carry[V](s: String, w: Wrap[V, String])
case class CarryList[V](li: List[String], w: Wrap[V, String])
case class CarryOpt[V](li: List[String], w: Wrap[V, String])
case class BagList[Y](s: String, many: List[Y])
case class BagMap[Y](i: Int, items: Map[String, Y])
case class BagOpt[Y](i: Int, maybe: Option[Y])
case class Truck[Z](s: Z, t: Two)

case class AllOpt(
  one:   Option[String],
  two:   Option[String],
  three: Option[String]
)

case class PrimitiveLists(
  ints:    List[Int],
  longs:   List[Long],
  bools:   List[Boolean],
  chars:   List[Char],
  doubles: List[Double]
)

case class One(
    name:     String,
    stuff:    List[String],
    more:     List[Two],
    nest:     Two,
    maybe:    Option[String],
    mymap:    Map[String, Int],
    flipflop: Boolean,
    big:      Long,
    num:      Num.Value,
    age:      Int
) {
  val foo: String = "yikes!"
}

case class OneSub1(
  name:  String,
  big:   Long,
  maybe: Option[String]
)

case class OneSub2(
  name:     String,
  flipflop: Boolean,
  mymap:    Map[String, Int]
)

case class Two(
  foo: String,
  bar: Boolean
)

case class Three(
  name: String,
  two:  Num.Value,
  pp:   Pop
)

case class Four(
  stuff:  List[String],
  things: Map[String, Int]
)

case class Five(
  @DBKey name: String,
  two:         Two
)

case class Six(
  @DBKey name: String,
  @DBKey num:  Int,
  two:         Two
)

case class Seven(
  @DBKey _id: co.blocke.scalajack.ObjectId,
  two:        Two
)

case class Numy(
  age: Int,
  num: Num.Value
)

case class UuidThing(
  name:  String,
  uuid:  java.util.UUID,
  many:  List[java.util.UUID],
  maybe: Option[java.util.UUID]
)

case class JodaThing(
  name:  String,
  dt:    OffsetDateTime,
  many:  List[OffsetDateTime],
  maybe: Option[OffsetDateTime]
)

trait Pop {
  def go(): Unit
}
trait Tart[T] {
  val yum: T
}
trait Soup[A] {
  val sweet: A
}

case class Wow1(a: String, b: Int) extends Pop {
  def go { println("--1--") }
}
case class Wow2(x: String, y: Int) extends Pop {
  def go { println("--2--") }
}
case class Cruton[U](i: Int, val sweet: U) extends Soup[U]
case class Toast[D](g: Int, val yum: D) extends Tart[D]
case class Bun[R](g: Int, val yum: R) extends Tart[R]
case class Breakfast[K](y: Boolean, bread: Tart[K])

case class Animal(val name: String, val legs: Int)

// Value class support w/custom rendering
class Wrapper(val underlying: Int) extends AnyVal
case class ValSupport(name: String, wrap: Wrapper, more: Boolean)
/*
object Wrapper extends ExtJson {
	override def toJson( obj:Any ) : String = "{\"num\":"+obj.asInstanceOf[Int]+",\"hey\":\"you\"}"
	override def fromJson( valueType:Field, jp:JsonEmitter, ext:Boolean, hint:String ) : Any = {
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
*/
case class ListValSupport(name: String, wrap: List[Wrapper], more: Boolean)
case class OptValSupport(name: String, wrap: Option[Wrapper])
case class MapValSupport(name: String, wrap: Map[String, Wrapper])

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

case class Foo(
  name:  String,
  stuff: List[String]
)

trait PetAnimal {
  val name: String
}
case class Dog(name: String) extends PetAnimal
case class Cat(name: String) extends PetAnimal
trait Pet {
  val kind: PetAnimal
  val food: String
}
case class NicePet(kind: PetAnimal, food: String) extends Pet
case class GrumpyPet(kind: PetAnimal, food: String) extends Pet
case class WithDefaults(
  name:     String,
  age:      Int             = 50,
  num:      Option[Int],
  hasStuff: Option[Boolean] = Some(true),
  pet:      Pet             = NicePet(Dog("Fido"), "bones")
)

object CustomVC {

  //  val typeAdapter = StringTypeAdapter andThen

  //  def stringToDateTime
  //
  //	def read:PartialFunction[(KindMarker,_), Any] = {
  //	  case (jk:JsonKind,js:String) => DateTimeFormat.forPattern("MMMM, yyyy").parseDateTime(js)
  //	  case (mk:MongoKind,bdt:BsonDateTime) => new DateTime(bdt.getValue)
  //	}
  //	def render:PartialFunction[(KindMarker,_), Any] = {
  //	  case (jk:JsonKind,dt:DateTime) => '"'+DateTimeFormat.forPattern("MMMM, yyyy").print(dt)+'"'
  //	  case (mk:MongoKind,dt:DateTime) => BsonDateTime(dt.toDate)
  //	}
}

class CustomVC(val underlying: YearMonth) extends AnyVal {
  override def toString = s"CustomVC($underlying)"
}
case class SomethingSpecial(what: String, when: CustomVC)
