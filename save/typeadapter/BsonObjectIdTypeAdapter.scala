package co.blocke.scalajack
package mongo
package typeadapter

import org.bson.BsonObjectId

case class BsonObjectIdContainer($oid: String)

object BsonObjectIdTypeAdapter extends TypeAdapterFactory.=:=[BsonObjectId] {

  override def create(next: TypeAdapterFactory)(implicit context: Context, tt: TypeTag[BsonObjectId]): TypeAdapter[BsonObjectId] = {
    val typeAdapter = context.typeAdapterOf[BsonObjectIdContainer]
    BsonObjectIdTypeAdapter(typeAdapter)
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
