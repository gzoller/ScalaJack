package co.blocke.scalajack
package json4s

import java.util.UUID
import co.blocke.scalajack.model._
import co.blocke.scala_reflection.RType
import co.blocke.scala_reflection.info.AliasInfo
import scala.collection.mutable
import co.blocke.scalajack.SJCapture

// === Scala
case class SampleBigDecimal(
    bd1: BigDecimal,
    bd2: BigDecimal,
    bd3: BigDecimal,
    bd4: BigDecimal,
    bd5: BigDecimal,
    bd6: BigDecimal)
case class SampleBigInt(bi1: BigInt, bi2: BigInt, bi3: BigInt, bi4: BigInt)
case class SampleBinary(b1: Array[Byte], b2: Array[Byte])
case class SampleBoolean(bool1: Boolean, bool2: Boolean)
case class SampleByte(b1: Byte, b2: Byte, b3: Byte, b4: Byte)
case class SampleChar(c1: Char, c2: Char, c3: Char)
case class SampleDouble(d1: Double, d2: Double, d3: Double, d4: Double)

object Size extends Enumeration {
  val Small, Medium, Large = Value
}
case class SampleEnum(
    e1: Size.Value,
    e2: Size.Value,
    e3: Size.Value,
    e4: Size.Value,
    e5: Size.Value)

case class SampleFloat(f1: Float, f2: Float, f3: Float, f4: Float)
case class SampleInt(i1: Int, i2: Int, i3: Int, i4: Int)
case class SampleLong(l1: Long, l2: Long, l3: Long, l4: Long)
case class SampleShort(s1: Short, s2: Short, s3: Short, s4: Short)
case class SampleString(s1: String, s2: String, s3: String)

case class SampleUUID(u1: UUID, u2: UUID)

case class Player(name: String, age: Int)
case class PlayerCapture(name: String, age: Int) extends SJCapture

case class OptionBigInt(o: Option[BigInt])
case class OptionClass(name: String, age: Option[Int])
case class OptionTuple(foo: Int, t: (Boolean, Option[String], Int))
trait Person { val name: String }
case class SomeClass(name: String, age: Int) extends Person
trait Thing[A, B] { val a: A; val b: B }
case class AThing[Y, X](a: X, b: Y) extends Thing[X, Y]

case class WrappedMaps(
    a: Map[Byte, Int],
    b: Map[Int, Int],
    c: Map[Long, Int],
    d: Map[Double, Int],
    e: Map[Float, Int],
    f: Map[Short, Int],
    g: Map[BigInt, Int],
    h: Map[BigDecimal, Int],
    i: Map[Boolean, Int],
    j: Map[Char, Int],
    k: Map[String, Int])

trait Address { val postalCode: String }
case class USAddress(
    street:     String,
    city:       String,
    state:      String,
    postalCode: String)
  extends Address
case class DefaultAddress(postalCode: String) extends Address
trait Demographic { val address: Address }
case class USDemographic(age: Int, address: Address) extends Demographic

trait Body
case class FancyBody(message: String) extends Body
case class Envelope[T <: Body](id: String, body: T) {
  type Giraffe = T
}

opaque type Phone = String

// Override just Phone
object PhoneAdapter extends TypeAdapterFactory with TypeAdapter[Phone]:
  def matches(concrete: RType): Boolean = 
    concrete match {
      case a: AliasInfo if a.name == "Phone" => 
        true
      case _ => 
        false
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

case class Employee(name: String, phone: Phone)
