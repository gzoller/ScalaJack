package co.blocke.scalajack

import java.time.{Instant, ZoneId, ZoneOffset, ZonedDateTime}

import org.bson.BsonDateTime

import scala.reflect.runtime.universe.{Type, typeOf}

object ZonedDateTimeTypeAdapter extends TypeAdapterFactory {

  override def typeAdapter(tpe: Type, context: Context, next: TypeAdapterFactory): Option[TypeAdapter[_]] =
    if (tpe =:= typeOf[ZonedDateTime]) {
      val bsonDateTimeTypeAdapter = context.typeAdapterOf[BsonDateTime]
      Some(ZonedDateTimeTypeAdapter(bsonDateTimeTypeAdapter))
    } else {
      next.typeAdapter(tpe, context)
    }

}

case class ZonedDateTimeTypeAdapter(bsonDateTimeTypeAdapter: TypeAdapter[BsonDateTime]) extends TypeAdapter[ZonedDateTime] {

  override def read(reader: Reader): ZonedDateTime = {
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
      bsonDateTimeTypeAdapter.write(new BsonDateTime(value.toInstant.toEpochMilli), writer)
    }

}
