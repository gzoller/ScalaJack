package co.blocke.scalajack

import org.bson.BsonObjectId

import scala.reflect.runtime.universe.{ Type, typeOf }

case class BsonObjectIdContainer($oid: String)

object BsonObjectIdTypeAdapter extends TypeAdapterFactory {

  override def typeAdapter(tpe: Type, context: Context): Option[TypeAdapter[_]] =
    if (tpe =:= typeOf[org.mongodb.scala.bson.BsonObjectId]) {
      val typeAdapter = context.typeAdapterOf[BsonObjectIdContainer]
      Some(BsonObjectIdTypeAdapter(typeAdapter))
    } else {
      None
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
