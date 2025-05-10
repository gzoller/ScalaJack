package co.blocke.scalajack
package json
package misc

import java.util.Optional
import scala.util.*
import neotype.*

case class Person(name: String, age: Int)

case class OptionHolder[T](
    a: Option[T], // straight Option
    b: (Option[T], String), // tuple w/Option
    c: List[Option[T]], // Seq of Option
    d: Map[Int, Option[T]], // Map of Option
    e: T | Option[T], // Union of Option (R)
    f: Option[T] | T, // Union of Option (L)
    g: Option[Option[T]], // Nested Option
    h: Option[Person], // Option of Class
    i: Either[T, Option[T]], // Either of Option (R)
    j: Either[Option[T], T] // Either of Option (L)
)

case class OptionalHolder[T](
    a: Optional[T], // straight Optional
    b: (Optional[T], String), // tuple w/Optional
    c: List[Optional[T]], // Seq of Optional
    d: Map[Int, Optional[T]], // Map of Optional
    e: T | Optional[T], // Union of Optional (R)
    f: Optional[T] | T, // Union of Optional (L)
    g: Optional[Optional[T]], // Nested Optional
    h: Optional[Person], // Optional of Class
    i: Either[T, Optional[T]], // Either of Optional (R)
    j: Either[Optional[T], T] // Either of Optional (L)
)

case class SimpleOptionHolder[T](a: Option[T])
case class TryHolder[T](a: Try[T])
case class TryHolder2[T](a: Seq[Try[T]], b: (Try[T], Try[T]))

case class LRUnionHolder[T, U](a: Seq[T | U], b: (T | U, T | U))
case class LRUnionHolder2[T, U](a: Seq[Boolean | Int], b: (T | U, T | U))
case class EitherHolder[T](a: Either[T, String], b: Either[String, T])

case class ComplexEither[T](a: Option[Either[String, Option[T]]])
case class EitherRecipe[T](a: Either[Boolean, Either[Option[T], String]])
case class EitherRecipeJ[T](a: Either[Boolean, Either[Optional[T], String]])

case class AliasHolder[T](a: T, b: List[T], c: Map[T, String], d: Map[String, T])
case class AliasHolder2[T](a: T, b: List[T], c: Map[String, T])

case class StringHolder(a: String)

case class MapHolder[T, V](a: Map[T, V])

type NonEmptyString = NonEmptyString.Type
given NonEmptyString: Newtype[String] with
  override inline def validate(input: String): Boolean =
    input.nonEmpty

type XList = XList.Type
given XList: Newtype[List[String]] with
  override inline def validate(input: List[String]): Boolean =
    input.nonEmpty && input(0) == "x"

type EmptyString = EmptyString.Type
given EmptyString: Newtype[String] with
  override inline def validate(input: String): Boolean =
    input.isEmpty

case class Validated(name: NonEmptyString, xspot: XList, nada: List[EmptyString])

case class AnyHolder(
    maybe: Any, // Option[List[String]] <- Some
    maybeNot: Any, // None
    itried: Any, // TryHolder[Int] <- class test
    itried2: Any, // Try[Int] (Success)
    ifailed: Any, // Try[Int] (Failure)
    anymap: Any, // Map[String,Int]
    whichOneR: Any, // Either[String,Int] <- right
    whichOneL: Any, // Either[String,Int] <- left
    bunch: Any // (Some('a'),None,Some('b'))
)

object Size extends Enumeration {
  val Small, Medium, Large = Value
}
object SizeWithType extends Enumeration {
  type SizeWithType = Value
  val Little, Grand = Value
}
import SizeWithType.*

object Permissions extends Enumeration {
  type Permissions = Value
  val READ, WRITE, EXEC, NONE = Value
}

// case class SampleEnum(e1: Enumeration#Value, e2: Enumeration#Value, e3: Enumeration#Value, e4: Enumeration#Value, e5: Enumeration#Value, e6: SizeWithType)
case class SampleEnum(e1: Size.Value, e2: Size.Value, e3: Size.Value, e4: Size.Value, e5: Size.Value, e6: SizeWithType)

enum Color {
  case Red, Blue, Green
}
case class TVColors(color1: Color, color2: Color)

sealed trait Flavor
case object Vanilla extends Flavor
case object Chocolate extends Flavor
case object Bourbon extends Flavor
case class FlavorHolder(f: Flavor, f2: Flavor, f3: Map[Flavor, String], f4: Map[String, Flavor])

sealed trait Vehicle
case class Truck(numberOfWheels: Int) extends Vehicle
case class Car(numberOfWheels: Int, color: String) extends Vehicle
case class Plane(numberOfEngines: Int) extends Vehicle
case class VehicleHolder(f: Vehicle, f2: Vehicle, f4: Map[String, Vehicle])

case class PhoneNumber(countryCode: Int, areaCode: Int, prefix: Int, rest: Int)
case class PhoneHolder(me: String, phone: PhoneNumber)

/*

TODO: FUTURE

To implement Intersection types we must be able to support non-sealed traits.
To do that, we need a full runtime mirror of everything else in ScalaJack that
happens at compile-time (everything, basically).  Not sure if/when that will
happen, so for now... no Intersection type support.

trait Scissors:
  val isSharp: Boolean
  def cut: Unit
trait Needle:
  val bobbins: Int
  def sew: Unit
case class DressFixer(isSharp: Boolean, bobbins: Int) extends Scissors, Needle:
  override def cut = println("Cut")
  override def sew = println("Sew")
case class LRInterHolder[T, U](a: Seq[T | U], b: (T | U, T | U))
 */
