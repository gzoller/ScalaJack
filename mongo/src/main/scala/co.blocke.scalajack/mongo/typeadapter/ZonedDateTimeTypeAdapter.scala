package co.blocke.scalajack
package mongo

import util.Path
import model._

import scala.collection.mutable.Builder
import org.bson._
import java.time._

object ZonedDateTimeTypeAdapterFactory extends TypeAdapter.=:=[ZonedDateTime] {

  def read[WIRE](path: Path, reader: Reader[WIRE], isMapKey: Boolean): ZonedDateTime =
    reader.head.input match {
      case null =>
        reader.next
        null
      // $COVERAGE-OFF$Doesn't seem to be reachable -- naked nulls passed from Bson, but just in case...
      case i if i.asInstanceOf[BsonValue].isNull() =>
        reader.next
        null
      // $COVERAGE-ON$
      case _ =>
        val dateTimeLong = reader.asInstanceOf[MongoReader].readDateTime(path)
        ZonedDateTime.ofInstant(Instant.ofEpochMilli(dateTimeLong), ZoneId.of("UTC"))
    }

  def write[WIRE](t: ZonedDateTime, writer: Writer[WIRE], out: Builder[WIRE, WIRE], isMapKey: Boolean): Unit =
    t match {
      case null =>
        out += new BsonNull().asInstanceOf[WIRE]
      case _ =>
        //        println("--------------------------------- Zoned Time Test")
        //        println("Raw ZonedDateTime: " + t)
        //        println("Shifted to utc: " + t.withZoneSameInstant(ZoneId.of("UTC")))
        //        println("To Instant: " + t.withZoneSameInstant(ZoneId.of("UTC")).toInstant)
        //        println("Final (epocmilli): " + t.withZoneSameInstant(ZoneId.of("UTC")).toInstant.toEpochMilli)
        //        println("---------------------------------")
        out += new BsonDateTime(t.withZoneSameInstant(ZoneId.of("UTC")).toInstant.toEpochMilli).asInstanceOf[WIRE]
    }
}
