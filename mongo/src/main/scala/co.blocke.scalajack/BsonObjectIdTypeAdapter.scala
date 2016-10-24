package co.blocke.scalajack

import org.bson.BsonObjectId

import scala.reflect.runtime.universe.{TypeTag, typeOf}

case class BsonObjectIdContainer($oid: String)

object BsonObjectIdTypeAdapter extends TypeAdapterFactory {

  override def typeAdapterOf[T](next: TypeAdapterFactory)(implicit context: Context, tt: TypeTag[T]): TypeAdapter[T] =
    if (tt.tpe =:= typeOf[BsonObjectId]) {
      val typeAdapter = context.typeAdapterOf[BsonObjectIdContainer]
      BsonObjectIdTypeAdapter(typeAdapter).asInstanceOf[TypeAdapter[T]]
    } else {
      next.typeAdapterOf[T]
    }

}

case class BsonObjectIdTypeAdapter(containerTypeAdapter: TypeAdapter[BsonObjectIdContainer]) extends TypeAdapter[BsonObjectId] {

  override def read(reader: Reader): org.mongodb.scala.bson.BsonObjectId = {
    val container = containerTypeAdapter.read(reader)
    if (container == null) {
      null
    } else {
      new org.mongodb.scala.bson.BsonObjectId(new org.bson.types.ObjectId(container.$oid))
    }
  }

  override def write(value: org.mongodb.scala.bson.BsonObjectId, writer: Writer): Unit = {
    if (value == null) {
      writer.writeNull()
    } else {
      writer.beginObject()
      writer.writeString("$oid")
      writer.writeString(value.getValue.toString)
      writer.endObject()
    }
  }

}
