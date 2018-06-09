package co.blocke.scalajack
package dynamodb
package test

import typeadapter.BasicTypeAdapter

object MyTypes {
  type Phone = String
}
import MyTypes._

// Override just Phone
object PhoneAdapter extends BasicTypeAdapter[Phone] {
  override def read(reader: Reader): Phone = {
    reader.peek match {
      case TokenType.String =>
        val raw = reader.readString()
        raw.replaceAll("-", "").asInstanceOf[Phone]
      // "%s-%s-%s".format(raw.substring(0, 3), raw.substring(3, 6), raw.substring(6)).asInstanceOf[Phone]
      case TokenType.Null =>
        reader.readNull()
    }
  }

  override def write(value: Phone, writer: Writer): Unit =
    if (value == null) {
      writer.writeNull()
    } else {
      writer.writeString("%s-%s-%s".format(value.substring(0, 3), value.substring(3, 6), value.substring(6)))
      // writer.writeString(value.replaceAll("-", ""))
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
