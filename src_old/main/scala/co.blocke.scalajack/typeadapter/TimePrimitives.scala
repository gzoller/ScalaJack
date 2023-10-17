package co.blocke.scalajack
package typeadapter

import co.blocke.scala_reflection.impl.Clazzes._
import co.blocke.scala_reflection.info._
import co.blocke.scala_reflection._

import scala.collection.mutable
import scala.util.{ Failure, Success, Try }
import java.time._
import java.time.format.DateTimeFormatter._

import model._

 object DurationTypeAdapterFactory extends TypeAdapterFactory with TypeAdapter[Duration]:
  def matches(concrete: RType): Boolean = 
    concrete match {
      case u: JavaClassInfo if u.infoClass.getName == "java.time.Duration" => true
      case _ => false
    }
  def makeTypeAdapter(concrete: RType)(implicit taCache: TypeAdapterCache): TypeAdapter[Duration] = this
  val info = RType.of[Duration]
  override def isStringish: Boolean = true

  def read(parser: Parser): Duration =
    parser.expectString() match {
      case null => null
      case s =>
        Try(Duration.parse(s)) match {
          case Success(d) => d
          case Failure(u) =>
            parser.backspace()
            throw new ScalaJackError(
              parser.showError(s"""Failed to parse Duration from input '$s'""")
            )
        }
    }

  def write[WIRE](
      t:      Duration,
      writer: Writer[WIRE],
      out:    mutable.Builder[WIRE, WIRE]): Unit =
    t match {
      case null => writer.writeNull(out)
      case _    => writer.writeString(t.toString, out)
    }


object InstantTypeAdapterFactory extends TypeAdapterFactory with TypeAdapter[Instant]:
  def matches(concrete: RType): Boolean = 
    concrete match {
      case u: JavaClassInfo if u.infoClass.getName == "java.time.Instant" => true
      case _ => false
    }
  def makeTypeAdapter(concrete: RType)(implicit taCache: TypeAdapterCache): TypeAdapter[Instant] = this
  val info = RType.of[Instant]
  override def isStringish: Boolean = true

  def read(parser: Parser): Instant =
    parser.expectString() match {
      case null => null
      case s =>
        Try(Instant.parse(s)) match {
          case Success(d) => d
          case Failure(u) =>
            parser.backspace()
            throw new ScalaJackError(
              parser.showError(s"""Failed to parse Instant from input '$s'""")
            )
        }
    }

  def write[WIRE](
      t:      Instant,
      writer: Writer[WIRE],
      out:    mutable.Builder[WIRE, WIRE]): Unit =
    t match {
      case null => writer.writeNull(out)
      case _    => writer.writeString(t.toString, out)
    }


object LocalDateTimeTypeAdapterFactory extends TypeAdapterFactory with TypeAdapter[LocalDateTime]:
  def matches(concrete: RType): Boolean = 
    concrete match {
      case u: JavaClassInfo if u.infoClass.getName == "java.time.LocalDateTime" => true
      case _ => false
    }
  def makeTypeAdapter(concrete: RType)(implicit taCache: TypeAdapterCache): TypeAdapter[LocalDateTime] = this
  val info = RType.of[LocalDateTime]
  override def isStringish: Boolean = true

  def read(parser: Parser): LocalDateTime =
    parser.expectString() match {
      case null => null
      case s =>
        Try(LocalDateTime.parse(s, ISO_LOCAL_DATE_TIME)) match {
          case Success(d) => d
          case Failure(u) =>
            parser.backspace()
            throw new ScalaJackError(
              parser
                .showError(s"""Failed to parse LocalDateTime from input '$s'""")
            )
        }
    }

  def write[WIRE](
      t:      LocalDateTime,
      writer: Writer[WIRE],
      out:    mutable.Builder[WIRE, WIRE]): Unit =
    t match {
      case null => writer.writeNull(out)
      case _    => writer.writeString(t.format(ISO_LOCAL_DATE_TIME), out)
    }


object LocalDateTypeAdapterFactory extends TypeAdapterFactory with TypeAdapter[LocalDate]:
  def matches(concrete: RType): Boolean = 
    concrete match {
      case u: JavaClassInfo if u.infoClass.getName == "java.time.LocalDate" => true
      case _ => false
    }
  def makeTypeAdapter(concrete: RType)(implicit taCache: TypeAdapterCache): TypeAdapter[LocalDate] = this
  val info = RType.of[LocalDate]
  override def isStringish: Boolean = true

  def read(parser: Parser): LocalDate =
    parser.expectString() match {
      case null => null
      case s =>
        Try(LocalDate.parse(s, ISO_LOCAL_DATE)) match {
          case Success(d) => d
          case Failure(u) =>
            parser.backspace()
            throw new ScalaJackError(
              parser.showError(s"""Failed to parse LocalDate from input '$s'""")
            )
        }
    }

  def write[WIRE](
      t:      LocalDate,
      writer: Writer[WIRE],
      out:    mutable.Builder[WIRE, WIRE]): Unit =
    t match {
      case null => writer.writeNull(out)
      case _    => writer.writeString(t.format(ISO_LOCAL_DATE), out)
    }


object LocalTimeTypeAdapterFactory extends TypeAdapterFactory with TypeAdapter[LocalTime]:
  def matches(concrete: RType): Boolean = 
    concrete match {
      case u: JavaClassInfo if u.infoClass.getName == "java.time.LocalTime" => true
      case _ => false
    }
  def makeTypeAdapter(concrete: RType)(implicit taCache: TypeAdapterCache): TypeAdapter[LocalTime] = this
  val info = RType.of[LocalTime]
  override def isStringish: Boolean = true

  def read(parser: Parser): LocalTime =
    parser.expectString() match {
      case null => null
      case s =>
        Try(LocalTime.parse(s, ISO_LOCAL_TIME)) match {
          case Success(d) => d
          case Failure(u) =>
            parser.backspace()
            throw new ScalaJackError(
              parser.showError(s"""Failed to parse LocalTime from input '$s'""")
            )
        }
    }

  def write[WIRE](
      t:      LocalTime,
      writer: Writer[WIRE],
      out:    mutable.Builder[WIRE, WIRE]): Unit =
    t match {
      case null => writer.writeNull(out)
      case _    => writer.writeString(t.format(ISO_LOCAL_TIME), out)
    }


object OffsetDateTimeTypeAdapterFactory extends TypeAdapterFactory with TypeAdapter[OffsetDateTime]:
  def matches(concrete: RType): Boolean = 
    concrete match {
      case u: JavaClassInfo if u.infoClass.getName == "java.time.OffsetDateTime" => true
      case _ => false
    }
  def makeTypeAdapter(concrete: RType)(implicit taCache: TypeAdapterCache): TypeAdapter[OffsetDateTime] = this
  val info = RType.of[OffsetDateTime]
  override def isStringish: Boolean = true

  def read(parser: Parser): OffsetDateTime =
    parser.expectString() match {
      case null => null
      case s =>
        Try(OffsetDateTime.parse(s, ISO_OFFSET_DATE_TIME)) match {
          case Success(d) => d
          case Failure(u) =>
            parser.backspace()
            throw new ScalaJackError(
              parser.showError(
                s"""Failed to parse OffsetDateTime from input '$s'"""
              )
            )
        }
    }

  def write[WIRE](
      t:      OffsetDateTime,
      writer: Writer[WIRE],
      out:    mutable.Builder[WIRE, WIRE]): Unit =
    t match {
      case null => writer.writeNull(out)
      case _    => writer.writeString(t.format(ISO_OFFSET_DATE_TIME), out)
    }


object OffsetTimeTypeAdapterFactory extends TypeAdapterFactory with TypeAdapter[OffsetTime]:
  def matches(concrete: RType): Boolean = 
    concrete match {
      case u: JavaClassInfo if u.infoClass.getName == "java.time.OffsetTime" => true
      case _ => false
    }
  def makeTypeAdapter(concrete: RType)(implicit taCache: TypeAdapterCache): TypeAdapter[OffsetTime] = this
  val info = RType.of[OffsetTime]
  override def isStringish: Boolean = true

  def read(parser: Parser): OffsetTime =
    parser.expectString() match {
      case null => null
      case s =>
        Try(OffsetTime.parse(s, ISO_OFFSET_TIME)) match {
          case Success(d) => d
          case Failure(u) =>
            parser.backspace()
            throw new ScalaJackError(
              parser
                .showError(s"""Failed to parse OffsetTime from input '$s'""")
            )
        }
    }

  def write[WIRE](
      t:      OffsetTime,
      writer: Writer[WIRE],
      out:    mutable.Builder[WIRE, WIRE]): Unit =
    t match {
      case null => writer.writeNull(out)
      case _    => writer.writeString(t.format(ISO_OFFSET_TIME), out)
    }


object PeriodTypeAdapterFactory extends TypeAdapterFactory with TypeAdapter[Period]:
  def matches(concrete: RType): Boolean = 
    concrete match {
      case u: JavaClassInfo if u.infoClass.getName == "java.time.Period" => true
      case _ => false
    }
  def makeTypeAdapter(concrete: RType)(implicit taCache: TypeAdapterCache): TypeAdapter[Period] = this
  val info = RType.of[Period]
  override def isStringish: Boolean = true

  def read(parser: Parser): Period =
    parser.expectString() match {
      case null => null
      case s =>
        Try(Period.parse(s)) match {
          case Success(d) => d
          case Failure(u) =>
            parser.backspace()
            throw new ScalaJackError(
              parser.showError(s"""Failed to parse Period from input '$s'""")
            )
        }
    }

  def write[WIRE](
      t:      Period,
      writer: Writer[WIRE],
      out:    mutable.Builder[WIRE, WIRE]): Unit =
    t match {
      case null => writer.writeNull(out)
      case _    => writer.writeString(t.toString, out)
    }


object ZonedDateTimeTypeAdapterFactory extends TypeAdapterFactory with TypeAdapter[ZonedDateTime]:
  def matches(concrete: RType): Boolean = 
    concrete match {
      case u: JavaClassInfo if u.infoClass.getName == "java.time.ZonedDateTime" => true
      case _ => false
    }
  def makeTypeAdapter(concrete: RType)(implicit taCache: TypeAdapterCache): TypeAdapter[ZonedDateTime] = this
  val info = RType.of[ZonedDateTime]
  override def isStringish: Boolean = true

  def read(parser: Parser): ZonedDateTime =
    parser.expectString() match {
      case null => null
      case s =>
        Try(ZonedDateTime.parse(s, ISO_ZONED_DATE_TIME)) match {
          case Success(d) => d
          case Failure(u) =>
            parser.backspace()
            throw new ScalaJackError(
              parser
                .showError(s"""Failed to parse ZonedDateTime from input '$s'""")
            )
        }
    }

  def write[WIRE](
      t:      ZonedDateTime,
      writer: Writer[WIRE],
      out:    mutable.Builder[WIRE, WIRE]): Unit =
    t match {
      case null => writer.writeNull(out)
      case _    => writer.writeString(t.format(ISO_ZONED_DATE_TIME), out)
    }
