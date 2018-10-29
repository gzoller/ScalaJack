package co.blocke.scalajack
package mongo
package typeadapter

import org.bson.BsonDateTime

case class DateContainer($date: Long)

object BsonDateTimeTypeAdapter extends TypeAdapterFactory.=:=[BsonDateTime] {

  override def create(next: TypeAdapterFactory)(implicit context: Context, tt: TypeTag[BsonDateTime]): TypeAdapter[BsonDateTime] = {
    val containerTypeAdapter = context.typeAdapterOf[DateContainer]
    BsonDateTimeTypeAdapter(containerTypeAdapter)
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
      // $COVERAGE-OFF$A BsonDateTime value should never be null -- safety valve
      writer.writeNull()
      // $COVERAGE-ON$
    } else {
      containerTypeAdapter.write(DateContainer(value.getValue), writer)
    }

}
