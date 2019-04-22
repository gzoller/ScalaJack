package co.blocke.scalajack
package typeadapter

import java.math.BigDecimal
import java.math.BigInteger

import model._
import util.Path

import scala.util.{ Try, Success, Failure }
import scala.collection.mutable.Builder

object JavaBigDecimalTypeAdapterFactory extends TypeAdapter.=:=[BigDecimal] with JavaBigDecimalTypeAdapter
trait JavaBigDecimalTypeAdapter {
  def read[WIRE](path: Path, reader: Reader[WIRE], isMapKey: Boolean): BigDecimal = reader.readDecimal(path) match {
    case null => null
    case bd   => bd.bigDecimal
  }
  def write[WIRE](t: BigDecimal, writer: Writer[WIRE], out: Builder[WIRE, WIRE], isMapKey: Boolean): Unit = t match {
    case null => writer.writeNull(out)
    case _    => writer.writeDecimal(t, out)
  }
}

object JavaBigIntegerTypeAdapterFactory extends TypeAdapter.=:=[BigInteger] with JavaBigIntegerTypeAdapter
trait JavaBigIntegerTypeAdapter {
  def read[WIRE](path: Path, reader: Reader[WIRE], isMapKey: Boolean): BigInteger = reader.readBigInt(path) match {
    case null => null
    case bi   => bi.bigInteger
  }
  def write[WIRE](t: BigInteger, writer: Writer[WIRE], out: Builder[WIRE, WIRE], isMapKey: Boolean): Unit = t match {
    case null => writer.writeNull(out)
    case _    => writer.writeBigInt(t, out)
  }
}

object JavaBooleanTypeAdapterFactory extends TypeAdapter.=:=[java.lang.Boolean] with JavaBooleanTypeAdapter
trait JavaBooleanTypeAdapter {
  def read[WIRE](path: Path, reader: Reader[WIRE], isMapKey: Boolean): java.lang.Boolean =
    Try(reader.readBoolean(path)) match {
      case Success(b) => b
      case Failure(x: ReadUnexpectedError) if x.nullFound =>
        reader.next
        null // Booleans are nullable in Java, but not Scala
      case Failure(x) => throw (x)
    }
  def write[WIRE](t: java.lang.Boolean, writer: Writer[WIRE], out: Builder[WIRE, WIRE], isMapKey: Boolean): Unit = t match {
    case null => writer.writeNull(out)
    case _    => writer.writeBoolean(t, out)
  }
}

object JavaByteTypeAdapterFactory extends TypeAdapter.=:=[java.lang.Byte] with JavaByteTypeAdapter
trait JavaByteTypeAdapter {
  def read[WIRE](path: Path, reader: Reader[WIRE], isMapKey: Boolean): java.lang.Byte =
    Try(reader.readInt(path).toByte) match {
      case Success(b) => b
      case Failure(x: ReadUnexpectedError) if x.nullFound =>
        reader.next
        null // Bytes are nullable in Java, but not Scala
      case Failure(x) => throw (x)
    }

  def write[WIRE](t: java.lang.Byte, writer: Writer[WIRE], out: Builder[WIRE, WIRE], isMapKey: Boolean): Unit = t match {
    case null => writer.writeNull(out)
    case _    => writer.writeInt(t.toByte, out)
  }
}

object JavaCharacterTypeAdapterFactory extends TypeAdapter.=:=[java.lang.Character] with JavaCharacterTypeAdapter with Stringish
trait JavaCharacterTypeAdapter {
  def read[WIRE](path: Path, reader: Reader[WIRE], isMapKey: Boolean): java.lang.Character =
    reader.readString(path) match {
      case null         => null
      case c if c != "" => c.toCharArray()(0)
      case _ =>
        reader.back
        throw new ReadInvalidError(reader.showError(path, "Tried to read a Character but empty string found"))
    }
  def write[WIRE](t: java.lang.Character, writer: Writer[WIRE], out: Builder[WIRE, WIRE], isMapKey: Boolean): Unit = t match {
    case null => writer.writeNull(out)
    case _    => writer.writeString(t.toString, out)
  }
}

object JavaDoubleTypeAdapterFactory extends TypeAdapter.=:=[java.lang.Double] with JavaDoubleTypeAdapter
trait JavaDoubleTypeAdapter {
  def read[WIRE](path: Path, reader: Reader[WIRE], isMapKey: Boolean): java.lang.Double =
    Try(reader.readDouble(path)) match {
      case Success(d) => d
      case Failure(x: ReadUnexpectedError) if x.nullFound =>
        reader.next
        null // Double are nullable in Java, but not Scala
      case Failure(x) => throw (x)
    }
  def write[WIRE](t: java.lang.Double, writer: Writer[WIRE], out: Builder[WIRE, WIRE], isMapKey: Boolean): Unit = t match {
    case null => writer.writeNull(out)
    case _    => writer.writeDouble(t, out)
  }
}

object JavaFloatTypeAdapterFactory extends TypeAdapter.=:=[java.lang.Float] with JavaFloatTypeAdapter
trait JavaFloatTypeAdapter {
  def read[WIRE](path: Path, reader: Reader[WIRE], isMapKey: Boolean): java.lang.Float =
    Try(reader.readDouble(path)) match {
      case Success(d) => d.floatValue()
      case Failure(x: ReadUnexpectedError) if x.nullFound =>
        reader.next
        null // Floats are nullable in Java, but not Scala
      case Failure(x) => throw (x)
    }
  def write[WIRE](t: java.lang.Float, writer: Writer[WIRE], out: Builder[WIRE, WIRE], isMapKey: Boolean): Unit = t match {
    case null => writer.writeNull(out)
    case _    => writer.writeDouble(util.FixFloat.capFloat(t), out)
  }
}

object JavaIntTypeAdapterFactory extends TypeAdapter.=:=[java.lang.Integer] with JavaIntTypeAdapter
trait JavaIntTypeAdapter {
  def read[WIRE](path: Path, reader: Reader[WIRE], isMapKey: Boolean): java.lang.Integer =
    Try(reader.readInt(path)) match {
      case Success(d) => d
      case Failure(x: ReadUnexpectedError) if x.nullFound =>
        reader.next
        null // Integers are nullable in Java, but not Scala
      case Failure(x) => throw (x)
    }
  def write[WIRE](t: java.lang.Integer, writer: Writer[WIRE], out: Builder[WIRE, WIRE], isMapKey: Boolean): Unit = t match {
    case null => writer.writeNull(out)
    case _    => writer.writeInt(t, out)
  }
}

object JavaLongTypeAdapterFactory extends TypeAdapter.=:=[java.lang.Long] with JavaLongTypeAdapter
trait JavaLongTypeAdapter {
  def read[WIRE](path: Path, reader: Reader[WIRE], isMapKey: Boolean): java.lang.Long =
    Try(reader.readLong(path)) match {
      case Success(d) => d
      case Failure(x: ReadUnexpectedError) if x.nullFound =>
        reader.next
        null // Longs are nullable in Java, but not Scala
      case Failure(x) => throw (x)
    }
  def write[WIRE](t: java.lang.Long, writer: Writer[WIRE], out: Builder[WIRE, WIRE], isMapKey: Boolean): Unit = t match {
    case null => writer.writeNull(out)
    case _    => writer.writeLong(t, out)
  }
}

object JavaNumberTypeAdapterFactory extends TypeAdapter.=:=[java.lang.Number] with JavaNumberTypeAdapter
trait JavaNumberTypeAdapter {
  def read[WIRE](path: Path, reader: Reader[WIRE], isMapKey: Boolean): java.lang.Number =
    reader.readDecimal(path) match {
      case null                                       => null
      case d: scala.BigDecimal if (d.isValidByte)     => d.toByteExact
      case d: scala.BigDecimal if (d.isValidShort)    => d.toShortExact
      case d: scala.BigDecimal if (d.isValidInt)      => d.toIntExact
      case d: scala.BigDecimal if (d.isValidLong)     => d.toLongExact
      case d: scala.BigDecimal if (d.isDecimalFloat)  => d.toFloat
      case d: scala.BigDecimal if (d.isDecimalDouble) => d.toDouble
      case d: scala.BigDecimal                        => d.bigDecimal
    }
  def write[WIRE](t: java.lang.Number, writer: Writer[WIRE], out: Builder[WIRE, WIRE], isMapKey: Boolean): Unit = t match {
    case null                                      => writer.writeNull(out)
    case t if t.isInstanceOf[java.lang.Integer]    => writer.writeInt(t.intValue, out)
    case t if t.isInstanceOf[java.lang.Long]       => writer.writeLong(t.longValue, out)
    case t if t.isInstanceOf[java.lang.Byte]       => writer.writeInt(t.byteValue, out)
    case t if t.isInstanceOf[java.lang.Short]      => writer.writeInt(t.shortValue, out)
    case t if t.isInstanceOf[java.lang.Float]      => writer.writeDouble(util.FixFloat.capFloat(t.floatValue), out)
    case t if t.isInstanceOf[java.lang.Double]     => writer.writeDouble(t.doubleValue, out)
    case t if t.isInstanceOf[java.math.BigInteger] => writer.writeBigInt(BigInt(t.asInstanceOf[BigInteger]), out)
    case t if t.isInstanceOf[java.math.BigDecimal] => writer.writeDecimal(scala.BigDecimal(t.asInstanceOf[BigDecimal]), out)
  }
}

object JavaShortTypeAdapterFactory extends TypeAdapter.=:=[java.lang.Short] with JavaShortTypeAdapter
trait JavaShortTypeAdapter {
  def read[WIRE](path: Path, reader: Reader[WIRE], isMapKey: Boolean): java.lang.Short =
    Try(reader.readInt(path)) match {
      case Success(d) => d.shortValue()
      case Failure(x: ReadUnexpectedError) if x.nullFound =>
        reader.next
        null // Shorts are nullable in Java, but not Scala
      case Failure(x) => throw (x)
    }
  def write[WIRE](t: java.lang.Short, writer: Writer[WIRE], out: Builder[WIRE, WIRE], isMapKey: Boolean): Unit = t match {
    case null => writer.writeNull(out)
    case _    => writer.writeInt(t.intValue, out)
  }
}