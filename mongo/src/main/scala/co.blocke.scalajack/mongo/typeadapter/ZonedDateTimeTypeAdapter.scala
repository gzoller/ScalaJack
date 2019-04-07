package co.blocke.scalajack
package mongo

import util.Path
import model._

import scala.collection.mutable.Builder
import org.mongodb.scala.bson._
import java.time._

object ZonedDateTimeTypeAdapterFactory extends TypeAdapter.=:=[ZonedDateTime] {

  def read[BsonValue](path: Path, reader: Reader[BsonValue]): ZonedDateTime = {
    val dateTimeLong = reader.asInstanceOf[MongoReader].readDateTime(path)
    ZonedDateTime.ofInstant(Instant.ofEpochMilli(dateTimeLong), ZoneId.systemDefault)
  }

  def write[BsonValue](t: ZonedDateTime, writer: Writer[BsonValue], out: Builder[BsonValue, BsonValue], isMapKey: Boolean): Unit =
    out += BsonDateTime(t.toInstant.toEpochMilli).asInstanceOf[BsonValue]
}
