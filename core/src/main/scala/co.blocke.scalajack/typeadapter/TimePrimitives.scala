package co.blocke.scalajack
package typeadapter

import scala.collection.mutable
import scala.util.{ Failure, Success, Try }
import java.time._
import java.time.format.DateTimeFormatter._

import model._

object DurationTypeAdapterFactory
  extends TypeAdapter.=:=[Duration]
  with Stringish {
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
}

object InstantTypeAdapterFactory
  extends TypeAdapter.=:=[Instant]
  with Stringish {
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
}

object LocalDateTimeTypeAdapterFactory
  extends TypeAdapter.=:=[LocalDateTime]
  with Stringish {
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
}

object LocalDateTypeAdapterFactory
  extends TypeAdapter.=:=[LocalDate]
  with Stringish {
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
}

object LocalTimeTypeAdapterFactory
  extends TypeAdapter.=:=[LocalTime]
  with Stringish {
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
}

object OffsetDateTimeTypeAdapterFactory
  extends TypeAdapter.=:=[OffsetDateTime]
  with Stringish {
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
}

object OffsetTimeTypeAdapterFactory
  extends TypeAdapter.=:=[OffsetTime]
  with Stringish {
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
}

object PeriodTypeAdapterFactory extends TypeAdapter.=:=[Period] with Stringish {
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
}

object ZonedDateTimeTypeAdapterFactory
  extends TypeAdapter.=:=[ZonedDateTime]
  with Stringish {
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
}
