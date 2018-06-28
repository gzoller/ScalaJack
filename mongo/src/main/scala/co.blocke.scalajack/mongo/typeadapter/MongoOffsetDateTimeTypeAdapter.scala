package co.blocke.scalajack
package mongo
package typeadapter

import java.time.{ Instant, OffsetDateTime, ZoneOffset }

import org.bson.BsonDateTime

import scala.reflect.runtime.universe.TypeTag

object MongoOffsetDateTimeTypeAdapter extends TypeAdapterFactory.=:=[OffsetDateTime] {

  override def create(next: TypeAdapterFactory)(implicit context: Context, tt: TypeTag[OffsetDateTime]): TypeAdapter[OffsetDateTime] = {
    val bsonDateTimeTypeAdapter = context.typeAdapterOf[BsonDateTime]
    MongoOffsetDateTimeTypeAdapter(bsonDateTimeTypeAdapter)
  }

}

case class MongoOffsetDateTimeTypeAdapter(bsonDateTimeTypeAdapter: TypeAdapter[BsonDateTime]) extends TypeAdapter[OffsetDateTime] {

  override def read(reader: Reader): OffsetDateTime = {
    val bsonDateTime = bsonDateTimeTypeAdapter.read(reader)
    if (bsonDateTime == null) {
      null
    } else {
      OffsetDateTime.ofInstant(Instant.ofEpochMilli(bsonDateTime.getValue), ZoneOffset.UTC)
    }
  }

  override def write(value: OffsetDateTime, writer: Writer): Unit =
    if (value == null) {
      writer.writeNull()
    } else {
      bsonDateTimeTypeAdapter.write(new BsonDateTime(value.toInstant.toEpochMilli), writer)
    }

}
