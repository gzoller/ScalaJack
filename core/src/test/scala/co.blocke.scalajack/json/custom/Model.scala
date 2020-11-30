package co.blocke.scalajack
package json.custom

import co.blocke.scala_reflection.impl.Clazzes._
import co.blocke.scala_reflection.info.AliasInfo
import co.blocke.scala_reflection._
import co.blocke.scalajack.model._

opaque type Phone >: Null = String

import scala.collection.mutable

// Override just Phone
object PhoneAdapter extends TypeAdapterFactory with TypeAdapter[Phone]:
  def matches(concrete: RType): Boolean = 
    concrete match {
      case a: AliasInfo if a.name == "Phone" => true
      case _ => false
    }
  def makeTypeAdapter(concrete: RType)(implicit taCache: TypeAdapterCache): TypeAdapter[Phone] = this
  val info = RType.of[Phone]
  override def isStringish: Boolean = true
  
  def read(parser: Parser): Phone =
    parser.expectString() match {
      case null      => null.asInstanceOf[Phone]
      case s: String => s.replaceAll("-", "").asInstanceOf[Phone]
    }

  def write[WIRE](
      t:      Phone,
      writer: Writer[WIRE],
      out:    mutable.Builder[WIRE, WIRE]): Unit = t match {
    case null => writer.writeNull(out)
    case _ =>
      writer.writeString(
        "%s-%s-%s".format(t.toString.substring(0, 3), t.toString.substring(3, 6), t.toString.substring(6)),
        out
      )
  }

case class Person(name: String, phone: Phone)

trait Address { val postalCode: String }
case class USAddress(
    street:     String,
    city:       String,
    state:      String,
    postalCode: String)
  extends Address
case class CanadaAddress(
    street:     String,
    city:       String,
    province:   String,
    postalCode: String)
  extends Address
case class DefaultAddress(postalCode: String) extends Address
trait Demographic { val address: Address }
case class USDemographic(age: Int, address: Address) extends Demographic
