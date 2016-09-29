package co.blocke.scalajack

import org.bson.BsonDateTime
import org.joda.time.DateTime

import scala.reflect.runtime.universe.{ Type, typeOf }

case class DateTimeContainer($date: Long)

object JodaDateTimeTypeAdapter extends TypeAdapterFactory {

  override def typeAdapter(tpe: Type, context: Context): Option[TypeAdapter[_]] =
    if (tpe =:= typeOf[DateTime]) {
      val bsonDateTimeTypeAdapter = context.typeAdapterOf[BsonDateTime]
      Some(JodaDateTimeTypeAdapter(bsonDateTimeTypeAdapter))
    } else {
      None
    }

}

case class JodaDateTimeTypeAdapter(bsonDateTimeTypeAdapter: TypeAdapter[BsonDateTime]) extends TypeAdapter[DateTime] {

  override def read(reader: Reader): DateTime = {
    val bsonDateTime = bsonDateTimeTypeAdapter.read(reader)
    if (bsonDateTime == null) {
      null
    } else {
      new DateTime(bsonDateTime.getValue)
    }
  }

  override def write(value: DateTime, writer: Writer): Unit =
    if (value == null) {
      writer.writeNull()
    } else {
      bsonDateTimeTypeAdapter.write(new BsonDateTime(value.getMillis), writer)
    }

}
