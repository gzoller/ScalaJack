package co.blocke.scalajack
package mongo
package typeadapter

//  import org.bson.types.{ObjectId => BsonObjectId}

class BsonObjectIdSerializer(containerTypeAdapter: TypeAdapter[ObjectId]) extends Serializer[ObjectId] {

  override def serialize[AST, S](tagged: TypeTagged[ObjectId])(implicit ops: AstOps[AST, S], guidance: SerializationGuidance): SerializationResult[AST] =
    tagged match {
      //      case TypeTagged(null) => SerializationSuccess(AstNull())
      case TypeTagged(bsoid) =>
        containerTypeAdapter.serializer.serialize(TypeTagged(bsoid, typeOf[ObjectId]))
    }
}