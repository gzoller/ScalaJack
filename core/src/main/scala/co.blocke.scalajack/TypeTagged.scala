package co.blocke.scalajack

import co.blocke.scalajack.TypeTagged.{ BooleanType, ByteType, CharType, DoubleType, FloatType, IntType, LongType, ShortType }

import scala.reflect.runtime.universe.{ Type, typeOf }

object TypeTagged {

  val BooleanType: Type = typeOf[Boolean]
  val ByteType: Type = typeOf[Byte]
  val CharType: Type = typeOf[Char]
  val DoubleType: Type = typeOf[Double]
  val FloatType: Type = typeOf[Float]
  val IntType: Type = typeOf[Int]
  val LongType: Type = typeOf[Long]
  val ShortType: Type = typeOf[Short]

  /**
   * Enables pattern matching.
   */
  @inline final def unapply[T](tagged: TypeTagged[T]): tagged.type = tagged

  def inferFromRuntimeClass[T](value: T): TypeTagged[T] = ???

  def apply[T](value: T, valueType: Type): TypeTagged[T] = Fixed(value, valueType)

  def apply(booleanValue: Boolean): TypeTaggedBoolean = TypeTaggedBoolean(booleanValue)

  def apply(byteValue: Byte): TypeTaggedByte = TypeTaggedByte(byteValue)

  def apply(charValue: Char): TypeTaggedChar = TypeTaggedChar(charValue)

  def apply(doubleValue: Double): TypeTaggedDouble = TypeTaggedDouble(doubleValue)

  def apply(floatValue: Float): TypeTaggedFloat = TypeTaggedFloat(floatValue)

  def apply(intValue: Int): TypeTaggedInt = TypeTaggedInt(intValue)

  def apply(longValue: Long): TypeTaggedLong = TypeTaggedLong(longValue)

  def apply(shortValue: Short): TypeTaggedShort = TypeTaggedShort(shortValue)

  private case class Fixed[+T](get: T, tpe: Type) extends TypeTagged[T]

}

/**
 * A combination of a value and its known [[Type]].
 *
 * @tparam T
 */
trait TypeTagged[+T] {

  def get: T

  def tpe: Type

  /**
   * Used to fulfill the requirements of [[TypeTagged#unapply]].
   */
  @inline final def isEmpty: Boolean = false

}

case class TypeTaggedBoolean(booleanValue: Boolean) extends TypeTagged[Boolean] {

  override def get: Boolean = booleanValue

  override def tpe: Type = BooleanType

}

case class TypeTaggedByte(byteValue: Byte) extends TypeTagged[Byte] {

  override def get: Byte = byteValue

  override def tpe: Type = ByteType

}

case class TypeTaggedChar(charValue: Char) extends TypeTagged[Char] {

  override def get: Char = charValue

  override def tpe: Type = CharType

}

case class TypeTaggedDouble(doubleValue: Double) extends TypeTagged[Double] {

  override def get: Double = doubleValue

  override def tpe: Type = DoubleType

}

case class TypeTaggedFloat(floatValue: Float) extends TypeTagged[Float] {

  override def get: Float = floatValue

  override def tpe: Type = FloatType

}

case class TypeTaggedInt(intValue: Int) extends TypeTagged[Int] {

  override def get: Int = intValue

  override def tpe: Type = IntType

}

case class TypeTaggedLong(longValue: Long) extends TypeTagged[Long] {

  override def get: Long = longValue

  override def tpe: Type = LongType

}

case class TypeTaggedShort(shortValue: Short) extends TypeTagged[Short] {

  override def get: Short = shortValue

  override def tpe: Type = ShortType

}
