package co.blocke.scalajack
package json.test.custom

import typeadapter.{ BasicTypeAdapter, SimpleTypeAdapter }

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

// Override Phone...and its parents (String)!
object OopsPhoneAdapter extends SimpleTypeAdapter[Phone] {
  override def read(reader: Reader): Phone = {
    reader.peek match {
      case TokenType.String ⇒
        val raw = reader.readString()
        raw.replaceAll("-", "").asInstanceOf[Phone]
      case TokenType.Null ⇒
        reader.readNull()
    }
  }

  override def write(value: Phone, writer: Writer): Unit =
    if (value == null) {
      writer.writeNull()
    } else {
      if (value.length > 6)
        writer.writeString("%s-%s-%s".format(value.substring(0, 3), value.substring(3, 6), value.substring(6)))
      else
        writer.writeString(value)
    }
}

case class Person(name: String, phone: Phone)

trait Address { val postalCode: String }
case class USAddress(street: String, city: String, state: String, postalCode: String) extends Address
case class CanadaAddress(street: String, city: String, province: String, postalCode: String) extends Address
case class DefaultAddress(postalCode: String) extends Address
trait Demographic { val address: Address }
case class USDemographic(age: Int, address: Address) extends Demographic
