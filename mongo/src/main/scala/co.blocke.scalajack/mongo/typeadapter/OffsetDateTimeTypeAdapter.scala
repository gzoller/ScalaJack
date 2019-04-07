package co.blocke.scalajack
package mongo

import util.Path
import model._

import scala.collection.mutable.Builder
import org.mongodb.scala.bson._
import java.time._

object OffsetDateTimeTypeAdapterFactory extends TypeAdapter.=:=[OffsetDateTime] {

  def read[WIRE](path: Path, reader: Reader[WIRE]): OffsetDateTime = {
    val dateTimeLong = reader.asInstanceOf[MongoReader].readDateTime(path)
    OffsetDateTime.ofInstant(Instant.ofEpochMilli(dateTimeLong), ZoneOffset.UTC)
  }

  def write[WIRE](t: OffsetDateTime, writer: Writer[WIRE], out: Builder[WIRE, WIRE], isMapKey: Boolean): Unit =
    out += BsonDateTime(t.toInstant.toEpochMilli).asInstanceOf[WIRE]
}
