package co.blocke.scalajack
package json.test.custom

object MyTypes {
  type Phone = String
}
import MyTypes._
import util.Path
import model.{ Stringish, Transceiver, TypeAdapter }

import scala.collection.mutable.Builder

// Override just Phone
object PhoneAdapter extends TypeAdapter.===[Phone] with Stringish {
  def read[WIRE](path: Path, reader: Transceiver[WIRE]): Phone =
    reader.readString(path) match {
      case s: String => s.replaceAll("-", "")
      case null      => null
    }

  def write[WIRE](t: Phone, writer: Transceiver[WIRE], out: Builder[Any, WIRE], isMapKey: Boolean): Unit = t match {
    case null => writer.writeNull(out)
    case _    => writer.writeString("%s-%s-%s".format(t.substring(0, 3), t.substring(3, 6), t.substring(6)), out)
  }
}

// Override Phone...and its parents (String)!
object OopsPhoneAdapter extends TypeAdapter.=:=[Phone] with Stringish {
  def read[WIRE](path: Path, reader: Transceiver[WIRE]): Phone =
    reader.readString(path) match {
      case s: String => s.replaceAll("-", "")
      case null      => null
    }

  def write[WIRE](t: Phone, writer: Transceiver[WIRE], out: Builder[Any, WIRE], isMapKey: Boolean): Unit = t match {
    case null => writer.writeNull(out)
    case _    => writer.writeString("%s-%s-%s".format(t.substring(0, 3), t.substring(3, 6), t.substring(6)), out)
  }
}

case class Person(name: String, phone: Phone)

trait Address { val postalCode: String }
case class USAddress(street: String, city: String, state: String, postalCode: String) extends Address
case class CanadaAddress(street: String, city: String, province: String, postalCode: String) extends Address
case class DefaultAddress(postalCode: String) extends Address
trait Demographic { val address: Address }
case class USDemographic(age: Int, address: Address) extends Demographic
