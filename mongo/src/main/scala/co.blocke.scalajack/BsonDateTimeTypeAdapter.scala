package co.blocke.scalajack

import co.blocke.scalajack.json.{Context, Reader, TypeAdapter, TypeAdapterFactory, Writer}
import co.blocke.scalajack.json.typeadapter.SimpleTypeAdapter
import org.bson.BsonDateTime
import scala.reflect.runtime.universe.{Type, typeOf}

case class DateContainer($date: Long)

object BsonDateTimeTypeAdapter extends TypeAdapterFactory {

  override def typeAdapter(tpe: Type, context: Context): Option[TypeAdapter[_]] =
    if (tpe =:= typeOf[BsonDateTime]) {
      val containerTypeAdapter = context.typeAdapterOf[DateContainer]
      Some(BsonDateTimeTypeAdapter(containerTypeAdapter))
    } else {
      None
    }

}

case class BsonDateTimeTypeAdapter(containerTypeAdapter: TypeAdapter[DateContainer]) extends TypeAdapter[BsonDateTime] {

  override def read(reader: Reader): BsonDateTime = {
    val container = containerTypeAdapter.read(reader)
    if (container == null) {
      null
    } else {
      new BsonDateTime(container.$date)
    }
  }

  override def write(value: BsonDateTime, writer: Writer): Unit =
    if (value == null) {
      writer.writeNull()
    } else {
      containerTypeAdapter.write(DateContainer(value.getValue), writer)
    }

}
