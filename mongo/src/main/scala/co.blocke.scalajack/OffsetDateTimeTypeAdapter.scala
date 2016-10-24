package co.blocke.scalajack

import java.time.{Instant, OffsetDateTime, ZoneOffset}

import org.bson.BsonDateTime

import scala.reflect.runtime.universe.{TypeTag, typeOf}

object OffsetDateTimeTypeAdapter extends TypeAdapterFactory {

  override def typeAdapterOf[T](next: TypeAdapterFactory)(implicit context: Context, tt: TypeTag[T]): TypeAdapter[T] =
    if (tt.tpe =:= typeOf[OffsetDateTime]) {
      val bsonDateTimeTypeAdapter = context.typeAdapterOf[BsonDateTime]
      OffsetDateTimeTypeAdapter(bsonDateTimeTypeAdapter).asInstanceOf[TypeAdapter[T]]
    } else {
      next.typeAdapterOf[T]
    }

}

case class OffsetDateTimeTypeAdapter(bsonDateTimeTypeAdapter: TypeAdapter[BsonDateTime]) extends TypeAdapter[OffsetDateTime] {

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
