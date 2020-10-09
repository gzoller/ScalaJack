package co.blocke.scalajack
package typeadapter

import model._
import co.blocke.scala_reflection._
import co.blocke.scala_reflection.info.JavaClassInfo
import co.blocke.scala_reflection.impl.PrimitiveType

import java.math.BigDecimal
import java.math.BigInteger

import model._
import scala.collection.mutable
import scala.language.implicitConversions

object JavaBigDecimalTypeAdapterFactory extends TypeAdapterFactory with TypeAdapter[BigDecimal] with ScalarTypeAdapter[BigDecimal]:
  def matches(concrete: RType): Boolean = 
    concrete match {
      case j: JavaClassInfo if j.name == "java.math.BigDecimal" => true
      case j => false
    }
  def makeTypeAdapter(concrete: RType)(implicit taCache: TypeAdapterCache): TypeAdapter[BigDecimal] = this

  val info = RType.of[java.math.BigDecimal]
  def read(parser: Parser): BigDecimal = 
    parser.expectNumber(true) match {
      case null => null
      case bd   => new BigDecimal(bd)
    }
  def write[WIRE](t: BigDecimal, writer: Writer[WIRE], out: mutable.Builder[WIRE, WIRE]): Unit =
    t match {
      case null => writer.writeNull(out)
      case _    => writer.writeDecimal(t, out)
    }

object JavaBigIntegerTypeAdapterFactory extends TypeAdapterFactory with TypeAdapter[BigInteger] with ScalarTypeAdapter[BigInteger]:
  def matches(concrete: RType): Boolean = 
    concrete match {
      case j: JavaClassInfo if j.name == "java.math.BigInteger" => true
      case _ => false
    }
  def makeTypeAdapter(concrete: RType)(implicit taCache: TypeAdapterCache): TypeAdapter[BigInteger] = this

  val info = RType.of[java.math.BigInteger]
  def read(parser: Parser): BigInteger = 
    parser.expectNumber(true) match {
      case null => null
      case bd   => new BigInteger(bd)
    }
  def write[WIRE](t: BigInteger, writer: Writer[WIRE], out: mutable.Builder[WIRE, WIRE]): Unit = t match {
    case null => writer.writeNull(out)
    case _    => writer.writeBigInt(t, out)
  }


object JavaBooleanTypeAdapterFactory extends TypeAdapterFactory with TypeAdapter[java.lang.Boolean] with ScalarTypeAdapter[java.lang.Boolean]:
  def matches(concrete: RType): Boolean = concrete.infoClass == PrimitiveType.Java_Boolean.infoClass
  def makeTypeAdapter(concrete: RType)(implicit taCache: TypeAdapterCache): TypeAdapter[java.lang.Boolean] = this

  val info = RType.of[java.lang.Boolean]
  def read(parser: Parser): java.lang.Boolean = 
    if (parser.peekForNull)
      null // Booleans are nullable in Java, but not in Scala
    else
      java.lang.Boolean.valueOf(parser.expectBoolean())
  def write[WIRE](t: java.lang.Boolean, writer: Writer[WIRE], out: mutable.Builder[WIRE, WIRE]): Unit =
    t match {
      case null => writer.writeNull(out)
      case _    => writer.writeBoolean(t, out)
    }


object JavaByteTypeAdapterFactory extends TypeAdapterFactory with TypeAdapter[java.lang.Byte] with ScalarTypeAdapter[java.lang.Byte]:
  def matches(concrete: RType): Boolean = concrete.infoClass == PrimitiveType.Java_Byte.infoClass
  def makeTypeAdapter(concrete: RType)(implicit taCache: TypeAdapterCache): TypeAdapter[java.lang.Byte] = this

  val info = RType.of[java.lang.Byte]
  def read(parser: Parser): java.lang.Byte =
    if (parser.peekForNull)
      null // Bytes are nullable in Java, but not Scala
    else
      java.lang.Byte.valueOf(parser.expectNumber().toInt.toByte)
  def write[WIRE](t: java.lang.Byte, writer: Writer[WIRE], out: mutable.Builder[WIRE, WIRE]): Unit =
    t match {
      case null => writer.writeNull(out)
      case _    => writer.writeInt(t.toByte, out)
    }
    

object JavaCharacterTypeAdapterFactory extends TypeAdapterFactory with TypeAdapter[java.lang.Character] with ScalarTypeAdapter[java.lang.Character]:
  def matches(concrete: RType): Boolean = concrete.infoClass == PrimitiveType.Java_Char.infoClass
  def makeTypeAdapter(concrete: RType)(implicit taCache: TypeAdapterCache): TypeAdapter[java.lang.Character] = this

  override def isStringish: Boolean = true
  val info = RType.of[java.lang.Character]
  def read(parser: Parser): java.lang.Character =
    if (parser.peekForNull)
      null
    else
      parser.expectString() match {
        case "" =>
          parser.backspace()
          throw new ScalaJackError(
            parser.showError("Tried to read a Character but empty string found")
          )
        case c => java.lang.Character.valueOf(c.toCharArray()(0))
      }    
  def write[WIRE](t: java.lang.Character, writer: Writer[WIRE], out: mutable.Builder[WIRE, WIRE]): Unit =
    t match {
      case null => writer.writeNull(out)
      case _    => writer.writeString(t.toString, out)
    }    


object JavaDoubleTypeAdapterFactory extends TypeAdapterFactory with TypeAdapter[java.lang.Double] with ScalarTypeAdapter[java.lang.Double]:
  def matches(concrete: RType): Boolean = concrete.infoClass == PrimitiveType.Java_Double.infoClass
  def makeTypeAdapter(concrete: RType)(implicit taCache: TypeAdapterCache): TypeAdapter[java.lang.Double] = this

  val info = RType.of[java.lang.Double]
  def read(parser: Parser): java.lang.Double =
    if (parser.peekForNull)
      null
    else
      java.lang.Double.valueOf(parser.expectNumber())
  def write[WIRE](t: java.lang.Double, writer: Writer[WIRE], out: mutable.Builder[WIRE, WIRE]): Unit =
    t match {
      case null => writer.writeNull(out)
      case _    => writer.writeDouble(t, out)
    }


object JavaFloatTypeAdapterFactory extends TypeAdapterFactory with TypeAdapter[java.lang.Float] with ScalarTypeAdapter[java.lang.Float]:
  def matches(concrete: RType): Boolean = concrete.infoClass == PrimitiveType.Java_Float.infoClass
  def makeTypeAdapter(concrete: RType)(implicit taCache: TypeAdapterCache): TypeAdapter[java.lang.Float] = this

  val info = RType.of[java.lang.Float]
  def read(parser: Parser): java.lang.Float =
    if (parser.peekForNull)
      null
    else
      java.lang.Float.valueOf(parser.expectNumber())
  def write[WIRE](t: java.lang.Float, writer: Writer[WIRE], out: mutable.Builder[WIRE, WIRE]): Unit =
    t match {
      case null => writer.writeNull(out)
      case _    => writer.writeDouble(util.FixFloat.capFloat(t), out)
    }
   

object JavaIntegerTypeAdapterFactory extends TypeAdapterFactory with TypeAdapter[java.lang.Integer] with ScalarTypeAdapter[java.lang.Integer]:
  def matches(concrete: RType): Boolean = concrete.infoClass == PrimitiveType.Java_Int.infoClass
  def makeTypeAdapter(concrete: RType)(implicit taCache: TypeAdapterCache): TypeAdapter[java.lang.Integer] = this

  val info = RType.of[java.lang.Integer]
  def read(parser: Parser): java.lang.Integer =
    if (parser.peekForNull)
      null
    else
      java.lang.Integer.valueOf(parser.expectNumber())
  def write[WIRE](t: java.lang.Integer, writer: Writer[WIRE], out: mutable.Builder[WIRE, WIRE]): Unit =
    t match {
      case null => writer.writeNull(out)
      case _    => writer.writeInt(t, out)
    }


object JavaLongTypeAdapterFactory extends TypeAdapterFactory with TypeAdapter[java.lang.Long] with ScalarTypeAdapter[java.lang.Long]:
  def matches(concrete: RType): Boolean = concrete.infoClass == PrimitiveType.Java_Long.infoClass
  def makeTypeAdapter(concrete: RType)(implicit taCache: TypeAdapterCache): TypeAdapter[java.lang.Long] = this

  val info = RType.of[java.lang.Long]
  def read(parser: Parser): java.lang.Long =
    if (parser.peekForNull)
      null
    else
      java.lang.Long.valueOf(parser.expectNumber())
  def write[WIRE](t: java.lang.Long, writer: Writer[WIRE], out: mutable.Builder[WIRE, WIRE]): Unit =
    t match {
      case null => writer.writeNull(out)
      case _    => writer.writeLong(t, out)
    }


object JavaNumberTypeAdapterFactory extends TypeAdapterFactory with TypeAdapter[java.lang.Number] with ScalarTypeAdapter[java.lang.Number]:
  def matches(concrete: RType): Boolean = concrete.infoClass == PrimitiveType.Java_Number.infoClass
  def makeTypeAdapter(concrete: RType)(implicit taCache: TypeAdapterCache): TypeAdapter[java.lang.Number] = this

  val info = RType.of[java.lang.Number]
  def read(parser: Parser): java.lang.Number =
    if (parser.peekForNull)
      null
    else
      scala.BigDecimal(parser.expectNumber()) match {
        case d if d.isValidByte     => java.lang.Byte.valueOf(d.toByteExact)
        case d if d.isValidShort    => java.lang.Short.valueOf(d.toShortExact)
        case d if d.isValidInt      => java.lang.Integer.valueOf(d.toIntExact)
        case d if d.isValidLong     => java.lang.Long.valueOf(d.toLongExact)
        case d if d.isDecimalFloat  => java.lang.Float.valueOf(d.toFloat)
        case d if d.isDecimalDouble => java.lang.Double.valueOf(d.toDouble)
        case d                      => d.bigDecimal
      }
  def write[WIRE](t: java.lang.Number, writer: Writer[WIRE], out: mutable.Builder[WIRE, WIRE]): Unit =
    t match {
      case null => writer.writeNull(out)
      case t if t.isInstanceOf[java.lang.Integer] =>
        writer.writeInt(t.intValue, out)
      case t if t.isInstanceOf[java.lang.Long] =>
        writer.writeLong(t.longValue, out)
      case t if t.isInstanceOf[java.lang.Byte] =>
        writer.writeInt(t.byteValue, out)
      case t if t.isInstanceOf[java.lang.Short] =>
        writer.writeInt(t.shortValue, out)
      case t if t.isInstanceOf[java.lang.Float] =>
        writer.writeDouble(util.FixFloat.capFloat(t.floatValue), out)
      case t if t.isInstanceOf[java.lang.Double] =>
        writer.writeDouble(t.doubleValue, out)
      case t if t.isInstanceOf[java.math.BigInteger] =>
        writer.writeBigInt(BigInt(t.asInstanceOf[BigInteger]), out)
      case t if t.isInstanceOf[java.math.BigDecimal] =>
        writer.writeDecimal(scala.BigDecimal(t.asInstanceOf[BigDecimal]), out)
    }


object JavaShortTypeAdapterFactory extends TypeAdapterFactory with TypeAdapter[java.lang.Short] with ScalarTypeAdapter[java.lang.Short]:
  def matches(concrete: RType): Boolean = concrete.infoClass == PrimitiveType.Java_Short.infoClass
  def makeTypeAdapter(concrete: RType)(implicit taCache: TypeAdapterCache): TypeAdapter[java.lang.Short] = this

  val info = RType.of[java.lang.Short]
  def read(parser: Parser): java.lang.Short =
    if (parser.peekForNull)
      null
    else
      java.lang.Short.valueOf(parser.expectNumber())
  def write[WIRE](t: java.lang.Short, writer: Writer[WIRE], out: mutable.Builder[WIRE, WIRE]): Unit =
    t match {
      case null => writer.writeNull(out)
      case _    => writer.writeInt(t.intValue, out)
    }
