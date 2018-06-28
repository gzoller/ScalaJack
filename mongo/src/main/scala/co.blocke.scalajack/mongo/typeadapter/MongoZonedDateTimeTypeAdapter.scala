package co.blocke.scalajack
package mongo
package typeadapter

import java.time.{ Instant, ZoneId, ZoneOffset, ZonedDateTime }

import org.bson.BsonDateTime

import scala.reflect.runtime.universe.TypeTag

object MongoZonedDateTimeTypeAdapter extends TypeAdapterFactory.=:=[ZonedDateTime] {

  override def create(next: TypeAdapterFactory)(implicit context: Context, tt: TypeTag[ZonedDateTime]): TypeAdapter[ZonedDateTime] = {
    val bsonDateTimeTypeAdapter = context.typeAdapterOf[BsonDateTime]
    MongoZonedDateTimeTypeAdapter(bsonDateTimeTypeAdapter)
  }

}

case class MongoZonedDateTimeTypeAdapter(bsonDateTimeTypeAdapter: TypeAdapter[BsonDateTime]) extends TypeAdapter[ZonedDateTime] {

  override def read(reader: Reader): ZonedDateTime =
    reader.peek match {
      case TokenType.Number =>
        val millis = reader.readLong()
        ZonedDateTime.ofInstant(Instant.ofEpochMilli(millis), ZoneId.ofOffset("UTC", ZoneOffset.UTC))

      case _ =>
        val bsonDateTime = bsonDateTimeTypeAdapter.read(reader)
        if (bsonDateTime == null) {
          null
        } else {
          ZonedDateTime.ofInstant(Instant.ofEpochMilli(bsonDateTime.getValue), ZoneId.systemDefault)
        }
    }

  override def write(value: ZonedDateTime, writer: Writer): Unit =
    if (value == null) {
      writer.writeNull()
    } else {
      //      bsonDateTimeTypeAdapter.write(new BsonDateTime(value.toInstant.toEpochMilli), writer)
      writer.writeLong(value.toInstant.toEpochMilli)
    }

}
