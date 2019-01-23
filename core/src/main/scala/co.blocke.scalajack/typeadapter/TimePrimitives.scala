package co.blocke.scalajack
package typeadapter

import model._
import util._
import util.Path

import scala.collection.mutable.Builder
import scala.util.{ Try, Success, Failure }
import java.time._
import java.time.format.DateTimeFormatter._

// Abortive Macro adventure...
//object Foo {
//  val duration: Any = MyMacro.readWrite[Duration]((s: String) => Duration.parse(s), "Duration", (t: Duration) => t.toString)
//}

object DurationTypeAdapterFactory extends TypeAdapter.=:=[Duration] {
  def read[WIRE](path: Path, reader: Transceiver[WIRE], isMapKey: Boolean = false): Duration =
    reader.readString(path) match {
      case null => null
      case s => Try(Duration.parse(s)) match {
        case Success(d) => d
        case Failure(u) => throw new ReadMalformedError(path, s"""Failed to parse Duration from input '$s'""", List.empty[String], u)
      }
    }

  def write[WIRE](t: Duration, writer: Transceiver[WIRE], out: Builder[Any, WIRE]): Unit =
    t match {
      case null => writer.writeNull(out)
      case _    => writer.writeString(t.toString, out)
    }
}

object InstantTypeAdapterFactory extends TypeAdapter.=:=[Instant] {
  def read[WIRE](path: Path, reader: Transceiver[WIRE], isMapKey: Boolean = false): Instant =
    reader.readString(path) match {
      case null => null
      case s => Try(Instant.parse(s)) match {
        case Success(d) => d
        case Failure(u) => throw new ReadMalformedError(path, s"""Failed to parse Instant from input '$s'""", List.empty[String], u)
      }
    }

  def write[WIRE](t: Instant, writer: Transceiver[WIRE], out: Builder[Any, WIRE]): Unit =
    t match {
      case null => writer.writeNull(out)
      case _    => writer.writeString(t.toString, out)
    }
}

object LocalDateTimeTypeAdapterFactory extends TypeAdapter.=:=[LocalDateTime] {
  def read[WIRE](path: Path, reader: Transceiver[WIRE], isMapKey: Boolean = false): LocalDateTime =
    reader.readString(path) match {
      case null => null
      case s => Try(LocalDateTime.parse(s, ISO_LOCAL_DATE_TIME)) match {
        case Success(d) => d
        case Failure(u) => throw new ReadMalformedError(path, s"""Failed to parse LocalDateTime from input '$s'""", List.empty[String], u)
      }
    }

  def write[WIRE](t: LocalDateTime, writer: Transceiver[WIRE], out: Builder[Any, WIRE]): Unit =
    t match {
      case null => writer.writeNull(out)
      case _    => writer.writeString(t.format(ISO_LOCAL_DATE_TIME), out)
    }
}

object LocalDateTypeAdapterFactory extends TypeAdapter.=:=[LocalDate] {
  def read[WIRE](path: Path, reader: Transceiver[WIRE], isMapKey: Boolean = false): LocalDate =
    reader.readString(path) match {
      case null => null
      case s => Try(LocalDate.parse(s, ISO_LOCAL_DATE)) match {
        case Success(d) => d
        case Failure(u) => throw new ReadMalformedError(path, s"""Failed to parse LocalDate from input '$s'""", List.empty[String], u)
      }
    }

  def write[WIRE](t: LocalDate, writer: Transceiver[WIRE], out: Builder[Any, WIRE]): Unit =
    t match {
      case null => writer.writeNull(out)
      case _    => writer.writeString(t.format(ISO_LOCAL_DATE), out)
    }
}

object LocalTimeTypeAdapterFactory extends TypeAdapter.=:=[LocalTime] {
  def read[WIRE](path: Path, reader: Transceiver[WIRE], isMapKey: Boolean = false): LocalTime =
    reader.readString(path) match {
      case null => null
      case s => Try(LocalTime.parse(s, ISO_LOCAL_TIME)) match {
        case Success(d) => d
        case Failure(u) => throw new ReadMalformedError(path, s"""Failed to parse LocalTime from input '$s'""", List.empty[String], u)
      }
    }

  def write[WIRE](t: LocalTime, writer: Transceiver[WIRE], out: Builder[Any, WIRE]): Unit =
    t match {
      case null => writer.writeNull(out)
      case _    => writer.writeString(t.format(ISO_LOCAL_TIME), out)
    }
}

object OffsetDateTimeTypeAdapterFactory extends TypeAdapter.=:=[OffsetDateTime] {
  def read[WIRE](path: Path, reader: Transceiver[WIRE], isMapKey: Boolean = false): OffsetDateTime =
    reader.readString(path) match {
      case null => null
      case s => Try(OffsetDateTime.parse(s, ISO_OFFSET_DATE_TIME)) match {
        case Success(d) => d
        case Failure(u) => throw new ReadMalformedError(path, s"""Failed to parse OffsetDateTime from input '$s'""", List.empty[String], u)
      }
    }

  def write[WIRE](t: OffsetDateTime, writer: Transceiver[WIRE], out: Builder[Any, WIRE]): Unit =
    t match {
      case null => writer.writeNull(out)
      case _    => writer.writeString(t.format(ISO_OFFSET_DATE_TIME), out)
    }
}

object OffsetTimeTypeAdapterFactory extends TypeAdapter.=:=[OffsetTime] {
  def read[WIRE](path: Path, reader: Transceiver[WIRE], isMapKey: Boolean = false): OffsetTime =
    reader.readString(path) match {
      case null => null
      case s => Try(OffsetTime.parse(s, ISO_OFFSET_TIME)) match {
        case Success(d) => d
        case Failure(u) => throw new ReadMalformedError(path, s"""Failed to parse OffsetTime from input '$s'""", List.empty[String], u)
      }
    }

  def write[WIRE](t: OffsetTime, writer: Transceiver[WIRE], out: Builder[Any, WIRE]): Unit =
    t match {
      case null => writer.writeNull(out)
      case _    => writer.writeString(t.format(ISO_OFFSET_TIME), out)
    }
}

object PeriodTypeAdapterFactory extends TypeAdapter.=:=[Period] {
  def read[WIRE](path: Path, reader: Transceiver[WIRE], isMapKey: Boolean = false): Period =
    reader.readString(path) match {
      case null => null
      case s => Try(Period.parse(s)) match {
        case Success(d) => d
        case Failure(u) => throw new ReadMalformedError(path, s"""Failed to parse Period from input '$s'""", List.empty[String], u)
      }
    }

  def write[WIRE](t: Period, writer: Transceiver[WIRE], out: Builder[Any, WIRE]): Unit =
    t match {
      case null => writer.writeNull(out)
      case _    => writer.writeString(t.toString, out)
    }
}

object ZonedDateTimeTypeAdapterFactory extends TypeAdapter.=:=[ZonedDateTime] {
  def read[WIRE](path: Path, reader: Transceiver[WIRE], isMapKey: Boolean = false): ZonedDateTime =
    reader.readString(path) match {
      case null => null
      case s => Try(ZonedDateTime.parse(s, ISO_ZONED_DATE_TIME)) match {
        case Success(d) => d
        case Failure(u) => throw new ReadMalformedError(path, s"""Failed to parse ZonedDateTime from input '$s'""", List.empty[String], u)
      }
    }

  def write[WIRE](t: ZonedDateTime, writer: Transceiver[WIRE], out: Builder[Any, WIRE]): Unit =
    t match {
      case null => writer.writeNull(out)
      case _    => writer.writeString(t.format(ISO_ZONED_DATE_TIME), out)
    }
}
