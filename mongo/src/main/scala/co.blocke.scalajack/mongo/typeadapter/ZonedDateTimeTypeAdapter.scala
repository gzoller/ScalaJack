package co.blocke.scalajack
package mongo

import util.Path
import model._

import scala.collection.mutable.Builder
import org.mongodb.scala.bson._
import java.time._

object ZonedDateTimeTypeAdapterFactory extends TypeAdapter.=:=[ZonedDateTime] {

  def read[WIRE](path: Path, reader: Reader[WIRE]): ZonedDateTime =
    reader.head.input match {
      case BsonNull => null
      case _ =>
        val dateTimeLong = reader.asInstanceOf[MongoReader].readDateTime(path)
        ZonedDateTime.ofInstant(Instant.ofEpochMilli(dateTimeLong), ZoneId.of("UTC"))
    }

  def write[WIRE](t: ZonedDateTime, writer: Writer[WIRE], out: Builder[WIRE, WIRE], isMapKey: Boolean): Unit =
    t match {
      case null => out += BsonNull().asInstanceOf[WIRE]
      case _ =>
        out += BsonDateTime(t.withZoneSameInstant(ZoneId.of("UTC")).toInstant.toEpochMilli).asInstanceOf[WIRE]
    }
}
