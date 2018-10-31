package co.blocke.scalajack
package mongo
package typeadapter

object BsonObjectIdTypeAdapter extends TypeAdapterFactory.===[ObjectId] {

  override def create(next: TypeAdapterFactory)(implicit context: Context, tt: TypeTag[ObjectId]): TypeAdapter[ObjectId] =
    BsonObjectIdTypeAdapter()
}

case class BsonObjectIdTypeAdapter() extends TypeAdapter[ObjectId] {
  override val serializer: Serializer[ObjectId] = new BsonObjectIdSerializer()
  override val deserializer: Deserializer[ObjectId] = new BsonObjectIdDeserializer()
}
