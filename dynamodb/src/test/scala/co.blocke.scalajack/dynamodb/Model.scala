package co.blocke.scalajack
package dynamodb

object MyTypes {
  type Phone = String
}
import MyTypes._

import model.{ Reader, Stringish, TypeAdapter, Writer }
import util.Path

import scala.collection.mutable.Builder

// Override just Phone
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

trait Human { val name: String; val age: Int }
case class Misc(wow: Double, bing: String)

@Collection(name = "people")
case class Person(
    @DBKey(index = 1) name:String,
    @DBKey(index = 0) age:Int,
    likes:                List[String],
    stuff:                Misc,
    foo:                  Option[Boolean] = None) extends Human

@Collection(name = "people2")
case class PersonOneKey(
    @DBKey(index = 0) name:String,
    age:                  Int,
    likes:                List[String],
    stuff:                Misc,
    foo:                  Option[Boolean] = None) extends Human

@Collection(name = "bogus")
case class ErrorNoKey(
    name:  String,
    age:   Int,
    likes: List[String],
    stuff: Misc,
    foo:   Option[Boolean] = None) extends Human

case class ErrorNoTable(
    @DBKey(index = 0) name:String,
    age:                  Int,
    likes:                List[String],
    stuff:                Misc,
    foo:                  Option[Boolean] = None) extends Human

case class PersonWithPhone(name: String, phone: Phone)
trait Address { val postalCode: String }
case class DefaultAddress(postalCode: String) extends Address
case class USAddress(street: String, city: String, state: String, postalCode: String) extends Address

@Collection(name = "people")
class PersonPlain1(
    @DBKey(index = 1) val name:String,
    @DBKey(index = 0) val age:Int,
    val likes:                List[String],
    val stuff:                Misc,
    val foo:                  Option[Boolean] = None)

@Collection(name = "people")
class PersonPlain2() {
  @DBKey(index = 1) var name: String = ""
  @DBKey(index = 0) var age: Int = 0
  var likes: List[String] = List.empty[String]
  var stuff: Misc = null
  var foo: Option[Boolean] = None
}

trait Body
case class FancyBody(message: String) extends Body

case class Envelope[T <: Body](id: String, body: T) {
  type Giraffe = T
}