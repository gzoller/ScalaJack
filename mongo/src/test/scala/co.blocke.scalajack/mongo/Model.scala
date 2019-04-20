package co.blocke.scalajack
package mongo

import java.time._

import model.{ Reader, Stringish, TypeAdapter, Writer }
import util.Path
import org.bson.types.ObjectId

import scala.collection.mutable.Builder
import scala.util.Try

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
    @DBKey _id: ObjectId,
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
  def go(): Unit = println("--1--")
}
case class Wow2(x: String, y: Int) extends Pop {
  def go(): Unit = println("--2--")
}
case class Cruton[U](i: Int, sweet: U) extends Soup[U]
case class Toast[D](g: Int, yum: D) extends Tart[D]
case class Bun[R](g: Int, yum: R) extends Tart[R]
case class Breakfast[K](y: Boolean, bread: Tart[K])

case class Animal(name: String, legs: Int)

// Value class support w/custom rendering
class Wrapper(val underlying: Int) extends AnyVal
case class ValSupport(name: String, wrap: Wrapper, more: Boolean)

case class ListValSupport(name: String, wrap: List[Wrapper], more: Boolean)
case class OptValSupport(name: String, wrap: Option[Wrapper])
case class MapValSupport(name: String, wrap: Map[String, Wrapper])

// Test Lists
case class ListList(name: String, stuff: List[List[Animal]])
case class ListListList(name: String, stuff: List[List[List[Animal]]])
case class ListOpt(name: String, stuff: List[Option[Animal]])
case class ListMap(name: String, stuff: List[Map[String, Animal]])

// Test nested Options+Variants w/other collections
case class OpOp(name: String, opts: Option[Option[Animal]])
case class OpList(name: String, opList: Option[List[Animal]])
case class OpListList(name: String, opListList: Option[List[List[Animal]]])
case class OpMap(name: String, opMap: Option[Map[String, Animal]])

// Test nested Maps+Variants w/other collections
case class MapList(name: String, mapList: Map[String, List[Animal]])
case class MapListList(name: String, mapList: Map[String, List[List[Animal]]])
case class MapOpt(name: String, mapOpt: Map[String, Option[Animal]])
case class MapMap(name: String, mapmap: Map[String, Map[String, Animal]])

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

case class SampleZonedDateTime(o1: ZonedDateTime, o2: ZonedDateTime)

trait Address { val postalCode: String }
case class USAddress(street: String, city: String, state: String, postalCode: String) extends Address
case class CanadaAddress(street: String, city: String, province: String, postalCode: String) extends Address
case class DefaultAddress(postalCode: String) extends Address
trait Demographic { val address: Address }
case class USDemographic(@DBKey age: String, address: Address) extends Demographic

object MyTypes {
  type Phone = String
}
import MyTypes._

object PhoneAdapter extends TypeAdapter.===[Phone] with Stringish {
  def read[WIRE](path: Path, reader: Reader[WIRE]): Phone =
    reader.readString(path) match {
      case s: String => s.replaceAll("-", "")
      case null      => null
    }

  def write[WIRE](t: Phone, writer: Writer[WIRE], out: Builder[WIRE, WIRE], isMapKey: Boolean): Unit = t match {
    case null => writer.writeNull(out)
    case _    => writer.writeString("%s-%s-%s".format(t.substring(0, 3), t.substring(3, 6), t.substring(6)), out)
  }
}

case class Person(@DBKey name: String, phone: Phone)

case class Loose(a: Char, b: Float, c: Short, d: Byte)

case class MapFactor(
    @Change(name = "foo_bar") fooBar:String,
    @Change(name = "a_b") thingy:   Long,
    count:                          Int,
    @Change(name = "big_mac") bigMac:String
)
case class MapFactorId(
    @DBKey @Change(name = "foo_bar") fooBar:String,
    @Change(name = "a_b") thingy:         Long,
    count:                                Int,
    @Change(name = "big_mac") bigMac:     String
)
case class MapFactorId2(
    @DBKey @Change(name = "foo_bar") fooBar:String,
    @DBKey @Change(name = "a_b") thingy:  Long,
    @DBKey hey:                           Int,
    count:                                Int,
    @Change(name = "big_mac") bigMac:     String
)

case class PersonCapture(id: ObjectId, name: String, age: Int, stuff: Map[Int, Int]) extends SJCapture
case class Tuple(t: (String, Int))

trait Strange
case class StrangeWrapper(num: Int, s: Strange)
case class StrangeHint(_hint: Int, size: Int) extends Strange

trait Body
case class FancyBody(message: String) extends Body
case class Envelope[T <: Body](id: String, body: T) {
  type Giraffe = T
}

case class Times(offset: OffsetDateTime, zoned: ZonedDateTime)

case class Embed(stuff: List[String], num: Int)
case class Boom(
    name:  String,
    other: Try[Embed])
