package co.blocke.scalajack
package test.v4

import json.JsonKind
import json.{Reader, TokenType, Writer}
import json.typeadapter.BasicTypeAdapter

import org.joda.time.DateTime
import org.joda.time.format.DateTimeFormat

trait Blah
case class Foo(
  name: String,
  age:  Int
) extends Blah
case class Stuff(
  item:  String,
  other: Blah
)

@Collection(name = "mystuff")
case class Decorated(
  @DBKey one: String,
  two:        Int,
  three:      Boolean
)

// Parameterized classes/traits
case class Case_1(name: String, other: List[WithType[Int]])
case class Case_2[T](name: String, other: List[WithType[T]])
case class Case_3[T](name: String, other: List[TwoP[T, Boolean]])
case class Case_5[U, T](name: String, other: List[TwoP[T, U]])
case class WithType[T](me: T)
case class TwoP[T, U](a: T, b: U)
trait Excite[T, U] {
  val a: U
  val b: T
}
trait PairOfValues[T, U] {
  val a: T
  val b: U
}
trait Meal[D] {
  val drink: D
}
case class Ex1(a: String, b: Int) extends Excite[Int, String]
case class Lunch[Z](drink: Z) extends Meal[Z]
case class PairOfMeals[Z, X](a: Meal[Z], b: Meal[X]) extends PairOfValues[Meal[Z], Meal[X]]
case class Ex3[R](a: String, b: R) extends PairOfValues[String, R]

case class All(
  a: Int,
  b: java.lang.Integer,
  c: Boolean,
  d: java.lang.String,
  e: String,
  f: Float,
  g: Double,
  h: Long,
  i: Char,
  j: String, // set to null
  k: Byte,
  l: Short,
  m: java.util.UUID,
  n: DateTime
)
case class AllColl(
  a: List[Int],
  b: List[Foo],
  c: Option[Int],
  d: Option[String],
  e: List[Option[Int]],
  f: Map[String, Int],
  g: Map[Foo, Option[WithType[Int]]] // test sloppy
)

object Colors extends Enumeration {
  val Red, Amber, Green = Value
}
object Formats extends Enumeration {
  type Format = Value
  val JSON = Value // add your values here, e.g. XML
}
import Formats._ // try alternate Ennumeration form
case class EnumExer(a: Colors.Value, b: Format)

// Value classes
class Wrapper(val underlying: String) extends AnyVal
class Wrapper2[T](val underlying: T) extends AnyVal
case class Wrapped(hey: Wrapper, you: Int)
case class Wrapped2[T](hey: Wrapper2[T], you: Int)

case class Address(street: String, zip: Int)
case class Pristine(name: String, age: Int, stuff: Option[Boolean], addr: Address)

class CustomVC(val underlying: DateTime) extends AnyVal
/*
object CustomVC extends ValueClassCustom {
  def read: PartialFunction[(KindMarker, _), Any] = {
    case (jk: JsonKind, js: String) => DateTimeFormat.forPattern("MMMM, yyyy").parseDateTime(js)
  }
  def render: PartialFunction[(KindMarker, _), Any] = {
    case (jk: JsonKind, dt: DateTime) => '"' + DateTimeFormat.forPattern("MMMM, yyyy").print(dt) + '"'
  }
}
*/
object SpecialAdapter extends BasicTypeAdapter[DateTime] {
  override def read(reader: Reader): DateTime = {
    reader.peek match {
      case TokenType.String ⇒
        DateTimeFormat.forPattern("MMMM, yyyy").parseDateTime(reader.readString())
      case TokenType.Null ⇒
        reader.readNull()
    }
  }

  override def write(value: DateTime, writer: Writer): Unit =
    if (value == null) {
      writer.writeNull()
    } else {
      writer.writeString(DateTimeFormat.forPattern("MMMM, yyyy").print(value))
    }
}
case class SomethingSpecial(what: String, when: CustomVC)

// Test view/splice
object Num extends Enumeration {
  val A, B, C = Value
}
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
case class Two(
  foo: String,
  bar: Boolean
)
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

trait Animal {
  val name: String
}
case class Dog(name: String) extends Animal
case class Cat(name: String) extends Animal
trait Pet {
  val kind: Animal
  val food: String
}
case class NicePet(kind: Animal, food: String) extends Pet
case class GrumpyPet(kind: Animal, food: String) extends Pet

object WithDefaults {
  def apply(s: String): WithDefaults = WithDefaults("a", 3, None)
}
case class WithDefaults(
  name:     String,
  age:      Int             = 50,
  num:      Option[Int],
  hasStuff: Option[Boolean] = Some(true),
  pet:      Pet             = NicePet(Dog("Fido"), "bones")
)