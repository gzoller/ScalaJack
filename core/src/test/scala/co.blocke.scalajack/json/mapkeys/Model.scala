package co.blocke.scalajack
package json.mapkeys

import java.util.UUID
import java.lang.{
  Boolean => JBoolean,
  Byte => JByte,
  Character => JChar,
  Double => JDouble,
  Float => JFloat,
  Integer => JInteger,
  Long => JLong,
  Number => JNumber,
  Short => JShort
}
import java.math.{ BigDecimal => JBigDecimal, BigInteger => JBigInteger }
import java.time._

object Size extends Enumeration {
  val Small, Medium, Large = Value
}

trait Thing[A, B] { val a: A; val b: B }
case class AThing[Y, X](a: X, b: Y) extends Thing[X, Y]
trait Part[A] { val p: A }
case class APart[A](p: A) extends Part[A]

// === Scala Primitive Keys
case class SampleBigDecimal(m: Map[BigDecimal, BigDecimal])
case class SampleBigInt(m: Map[BigInt, BigInt])
case class SampleBoolean(m: Map[Boolean, Boolean])
case class SampleByte(m: Map[Byte, Byte])
case class SampleChar(m: Map[Char, Char])
case class SampleDouble(m: Map[Double, Double])
case class SampleEnumeration(m: Map[Size.Value, Size.Value])
case class SampleFloat(m: Map[Float, Float])
case class SampleInt(m: Map[Int, Int])
case class SampleLong(m: Map[Long, Long])
case class SampleShort(m: Map[Short, Short])

// === Java Primitive Keys
case class SampleJBigDecimal(m: Map[JBigDecimal, JBigDecimal])
case class SampleJBigInteger(m: Map[JBigInteger, JBigInteger])
case class SampleJBoolean(m: Map[JBoolean, JBoolean])
case class SampleJByte(m: Map[JByte, JByte])
case class SampleJChar(m: Map[JChar, JChar])
case class SampleJDouble(m: Map[JDouble, JDouble])
case class SampleJFloat(m: Map[JFloat, JFloat])
case class SampleJInteger(m: Map[JInteger, JInteger])
case class SampleJLong(m: Map[JLong, JLong])
case class SampleJNumber(m: Map[JNumber, JNumber])
case class SampleJShort(m: Map[JShort, JShort])

// === Java Time Keys
case class SampleDuration(m: Map[Duration, Duration])
case class SampleInstant(m: Map[Instant, Instant])
case class SampleLocalDateTime(m: Map[LocalDateTime, LocalDateTime])
case class SampleLocalDate(m: Map[LocalDate, LocalDate])
case class SampleLocalTime(m: Map[LocalTime, LocalTime])
case class SampleOffsetDateTime(m: Map[OffsetDateTime, OffsetDateTime])
case class SampleOffsetTime(m: Map[OffsetTime, OffsetTime])
case class SamplePeriod(m: Map[Period, Period])
case class SampleZonedDateTime(m: Map[ZonedDateTime, ZonedDateTime])

// === Any primitives
case class AnyShell(m: Map[Any, Any])

// === Class Keys
case class SimpleClass(name: String, age: Int, isOk: Boolean, favorite: Any)
case class SampleSimple(m: Map[SimpleClass, SimpleClass])
case class ComplexClass(id: UUID, simple: SimpleClass, allDone: Boolean)
case class SampleComplex(m: Map[ComplexClass, ComplexClass])

object Food extends Enumeration {
  val Seeds, Meat, Pellets, Veggies = Value
}
trait Pet {
  val name: String
  val food: Food.Value
}
case class FishPet(name: String, food: Food.Value, waterTemp: Double)
  extends Pet
case class DogPet(name: String, food: Food.Value, numLegs: Int) extends Pet
case class CompoundPet(name: String, food: Food.Value, pet: Pet) extends Pet
trait PetHolder {
  val address: String
  val pet: Pet
}
case class ShinyPetHolder(address: String, pet: Pet) extends PetHolder
case class SamplePet(m: Map[Pet, Pet])
case class PolyClass(lookup: Map[String, Int], favs: List[String])
case class SamplePolyClass(m: Map[PolyClass, PolyClass])
case class SampleShiny(m: Map[PetHolder, PetHolder])
case class NCKey(nc: Map[Int, Boolean], name: String)
case class SampleNCKey(m: Map[NCKey, NCKey])

// === Collections - Tuple
case class SampleTuple(m: Map[(Int, String, Char), (String, Boolean)])
case class SampleTupleList(
    m: Map[(List[String], List[Int]), (List[String], List[Int])]
)
case class SampleTupleMap(
    m: Map[(Map[String, Int], Map[Int, String]), (Map[String, Int], Map[Int, String])]
)
case class SampleTupleTuple(
    m: Map[((String, Boolean), (Int, Double)), ((String, Boolean), (Int, Double))]
)
case class SampleTupleClass(
    m: Map[(SampleChar, SampleInt), (SampleChar, SampleInt)]
)
case class SampleTupleTrait(m: Map[(Pet, Pet), (Pet, Pet)])
case class SampleTupleAny(m: Map[(Any, Any), (Any, Any)])
case class SampleTupleOptional(
    m: Map[(Option[Int], Option[String]), (Option[Boolean], Option[Food.Value])]
)
case class SampleTupleVC(m: Map[(VCChar, VCChar), (VCChar, VCChar)])
case class SampleTupleComplex(
    m: Map[(ComplexClass, ComplexClass), (ComplexClass, ComplexClass)]
)
case class SampleTuplePolyClass(
    m: Map[(PolyClass, PolyClass), (PolyClass, PolyClass)]
)

// === Value Classes
case class VCBigDecimal(vc: BigDecimal) extends AnyVal
case class SampleVCBigDecimal(m: Map[VCBigDecimal, VCBigDecimal])
case class VCBigInt(vc: BigInt) extends AnyVal
case class SampleVCBigInt(m: Map[VCBigInt, VCBigInt])
case class VCBoolean(vc: Boolean) extends AnyVal
case class SampleVCBoolean(m: Map[VCBoolean, VCBoolean])
case class VCByte(vc: Byte) extends AnyVal
case class SampleVCByte(m: Map[VCByte, VCByte])
case class VCChar(vc: Char) extends AnyVal
case class SampleVCChar(m: Map[VCChar, VCChar])
case class VCDouble(vc: Double) extends AnyVal
case class SampleVCDouble(m: Map[VCDouble, VCDouble])
case class VCEnumeration(vc: Food.Value) extends AnyVal
case class SampleVCEnumeration(m: Map[VCEnumeration, VCEnumeration])
case class VCFloat(vc: Float) extends AnyVal
case class SampleVCFloat(m: Map[VCFloat, VCFloat])
case class VCInt(vc: Int) extends AnyVal
case class SampleVCInt(m: Map[VCInt, VCInt])
case class VCLong(vc: Long) extends AnyVal
case class SampleVCLong(m: Map[VCLong, VCLong])
case class VCShort(vc: Short) extends AnyVal
case class SampleVCShort(m: Map[VCShort, VCShort])
case class VCString(vc: String) extends AnyVal
case class SampleVCString(m: Map[VCString, VCString])
case class VCUUID(vc: UUID) extends AnyVal
case class SampleVCUUID(m: Map[VCUUID, VCUUID])
case class VCNumber(vc: Number) extends AnyVal
case class SampleVCNumber(m: Map[VCNumber, VCNumber])
case class VCList(vc: List[Int]) extends AnyVal
case class SampleVCList(m: Map[VCList, VCList])
case class VCMap(vc: Map[Int, Int]) extends AnyVal
case class SampleVCMap(m: Map[VCMap, VCMap])
case class VCTuple(vc: Tuple3[Int, String, Boolean]) extends AnyVal
case class SampleVCTuple(m: Map[VCTuple, VCTuple])

case class VCClass(vc: ComplexClass) extends AnyVal
case class SampleVCClass(m: Map[VCClass, VCClass])
case class VCTrait(vc: Pet) extends AnyVal
case class SampleVCTrait(m: Map[VCTrait, VCTrait])
case class VCOption(vc: Option[String]) extends AnyVal
case class SampleVCOption(m: Map[VCOption, VCOption])
case class VCNested(vc: List[Map[String, String]]) extends AnyVal
case class SampleVCNested(m: Map[VCNested, VCNested])

case class VCParamClass[A, B](vc: AThing[A, B]) extends AnyVal
case class SampleVCParamClass[A, B](
    m: Map[VCParamClass[A, B], VCParamClass[A, B]]
)
case class VCParamTrait[A, B](vc: Thing[A, B]) extends AnyVal
case class SampleVCParamTrait[A, B](
    m: Map[VCParamTrait[A, B], VCParamTrait[A, B]]
)
