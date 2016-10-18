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
      case TokenType.String ⇒
        val raw = reader.readString()
        raw.replaceAll("-", "").asInstanceOf[Phone]
      // "%s-%s-%s".format(raw.substring(0, 3), raw.substring(3, 6), raw.substring(6)).asInstanceOf[Phone]
      case TokenType.Null ⇒
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
case class Person(name: String, age: Int, likes: List[String], stuff: Misc, foo: Option[Boolean] = None) extends Human
case class PersonWithPhone(name: String, phone: Phone)
trait Address { val postalCode: String }
case class DefaultAddress(postalCode: String) extends Address
case class USAddress(street: String, city: String, state: String, postalCode: String) extends Address
