package co.blocke.scalajack

import org.bson.BsonDateTime

import scala.reflect.runtime.universe.{TypeTag, typeOf}

case class DateContainer($date: Long)

object BsonDateTimeTypeAdapter extends TypeAdapterFactory {

  override def typeAdapterOf[T](next: TypeAdapterFactory)(implicit context: Context, tt: TypeTag[T]): TypeAdapter[T] =
    if (tt.tpe =:= typeOf[BsonDateTime]) {
      val containerTypeAdapter = context.typeAdapterOf[DateContainer]
      BsonDateTimeTypeAdapter(containerTypeAdapter).asInstanceOf[TypeAdapter[T]]
    } else {
      next.typeAdapterOf[T]
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
