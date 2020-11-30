package co.blocke.scalajack
package json.primitives

import java.util.UUID
import java.lang.{
  Boolean => JBoolean,
  Byte => JByte,
  Character => JChar,
  Double => JDouble,
  Float => JFloat,
  Integer => JInt,
  Long => JLong,
  Number => JNumber,
  Short => JShort
}
import java.math.{ BigDecimal => JBigDecimal, BigInteger => JBigInteger }
import java.time._
import scala.math._

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
object SizeWithType extends Enumeration {
  type SizeWithType = Value
  val Little, Grand = Value
}
import SizeWithType._
case class SampleEnum(
    e1: Size.Value,
    e2: Size.Value,
    e3: Size.Value,
    e4: Size.Value,
    e5: Size.Value,
    e6: SizeWithType)

enum Color {
    case Red, Blue, Green
}
case class TVColors( color1: Color, color2: Color )

sealed trait Flavor
case object Vanilla extends Flavor
case object Chocolate extends Flavor
case object Bourbon extends Flavor

sealed trait Vehicle
case class Truck(numberOfWheels: Int) extends Vehicle
case class Car(numberOfWheels: Int, color: String) extends Vehicle
case class Plane(numberOfEngines: Int) extends Vehicle

case class Ride( wheels: Vehicle )
case class Favorite( flavor: Flavor )

case class SampleFloat(f1: Float, f2: Float, f3: Float, f4: Float)
case class SampleInt(i1: Int, i2: Int, i3: Int, i4: Int)
case class SampleLong(l1: Long, l2: Long, l3: Long, l4: Long)
case class SampleShort(s1: Short, s2: Short, s3: Short, s4: Short)
case class SampleString(s1: String, s2: String, s3: String)

// === Java
case class SampleJBigDecimal(
    bd1: JBigDecimal,
    bd2: JBigDecimal,
    bd3: JBigDecimal,
    bd4: JBigDecimal,
    bd5: JBigDecimal)
case class SampleJBigInteger(
    bi1: JBigInteger,
    bi2: JBigInteger,
    bi3: JBigInteger,
    bi4: JBigInteger,
    bi5: JBigInteger,
    bi6: JBigInteger,
    bi7: JBigInteger)
case class SampleJBoolean(
    bool1: JBoolean,
    bool2: JBoolean,
    bool3: JBoolean,
    bool4: JBoolean,
    bool5: JBoolean)
case class SampleJByte(b1: JByte, b2: JByte, b3: JByte, b4: JByte, b5: JByte)
case class SampleJChar(c1: JChar, c2: JChar, c3: JChar)
case class SampleJDouble(
    d1: JDouble,
    d2: JDouble,
    d3: JDouble,
    d4: JDouble,
    d5: JDouble)
case class SampleJFloat(
    f1: JFloat,
    f2: JFloat,
    f3: JFloat,
    f4: JFloat,
    f5: JFloat)
case class SampleJInt(i1: JInt, i2: JInt, i3: JInt, i4: JInt, i5: JInt)
case class SampleJLong(l1: JLong, l2: JLong, l3: JLong, l4: JLong, l5: JLong)
case class SampleJNumber(
    n1:  JNumber,
    n2:  JNumber,
    n3:  JNumber,
    n4:  JNumber,
    n5:  JNumber,
    n6:  JNumber,
    n7:  JNumber,
    n8:  JNumber,
    n9:  JNumber,
    n10: JNumber,
    n11: JNumber,
    n12: JNumber,
    n13: JNumber,
    n14: JNumber,
    n15: JNumber,
    n16: JNumber,
    n17: JNumber)
case class SampleJShort(
    s1: JShort,
    s2: JShort,
    s3: JShort,
    s4: JShort,
    s5: JShort)
case class SampleUUID(u1: UUID, u2: UUID)

// === Java Time
case class SampleDuration(d1: Duration, d2: Duration, d3: Duration)
case class SampleInstant(
    i1: Instant,
    i2: Instant,
    i3: Instant,
    i4: Instant,
    i5: Instant)
case class SampleLocalDateTime(
    d1: LocalDateTime,
    d2: LocalDateTime,
    d3: LocalDateTime,
    d4: LocalDateTime)
case class SampleLocalDate(
    d1: LocalDate,
    d2: LocalDate,
    d3: LocalDate,
    d4: LocalDate)
case class SampleLocalTime(
    d1: LocalTime,
    d2: LocalTime,
    d3: LocalTime,
    d4: LocalTime,
    d5: LocalTime,
    d6: LocalTime)
case class SampleOffsetDateTime(
    o1: OffsetDateTime,
    o2: OffsetDateTime,
    o3: OffsetDateTime,
    o4: OffsetDateTime)
case class SampleOffsetTime(
    o1: OffsetTime,
    o2: OffsetTime,
    o3: OffsetTime,
    o4: OffsetTime)
case class SamplePeriod(p1: Period, p2: Period, p3: Period)
case class SampleZonedDateTime(o1: ZonedDateTime, o2: ZonedDateTime)

// === Any primitives
case class AnyShell(a: Any)

// === Value Classes
case class VCBigDecimal(vc: BigDecimal) extends AnyVal
case class VCBigInt(vc: BigInt) extends AnyVal
case class VCBoolean(vc: Boolean) extends AnyVal
case class VCByte(vc: Byte) extends AnyVal
case class VCChar(vc: Char) extends AnyVal
case class VCDouble(vc: Double) extends AnyVal
case class VCEnum(vc: Color) extends AnyVal
case class VCEnumeration(vc: Size.Value) extends AnyVal
case class VCFloat(vc: Float) extends AnyVal
case class VCInt(vc: Int) extends AnyVal
case class VCLong(vc: Long) extends AnyVal
case class VCShort(vc: Short) extends AnyVal
case class VCString(vc: String) extends AnyVal
case class VCUUID(vc: UUID) extends AnyVal
case class VCNumber(vc: Number) extends AnyVal

// === Permissives test
case class Holder[T](value: T)