package co.blocke.scalajack
package mongo

import scala.collection.mutable
import org.bson._
import java.time._

import model._

object ZonedDateTimeTypeAdapter extends TypeAdapter.=:=[ZonedDateTime] {

  def read(parser: Parser): ZonedDateTime =
    parser.expectNumber(true) match {
      case null => null
      case dateTimeLong =>
        ZonedDateTime.ofInstant(
          Instant.ofEpochMilli(dateTimeLong.toLong),
          ZoneId.of("UTC")
        )
    }

  def write[WIRE](
      t:      ZonedDateTime,
      writer: Writer[WIRE],
      out:    mutable.Builder[WIRE, WIRE]): Unit =
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
        out += new BsonDateTime(
          t.withZoneSameInstant(ZoneId.of("UTC")).toInstant.toEpochMilli
        ).asInstanceOf[WIRE]
    }
}
