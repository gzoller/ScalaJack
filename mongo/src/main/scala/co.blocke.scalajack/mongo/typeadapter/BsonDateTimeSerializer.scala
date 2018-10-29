package co.blocke.scalajack
package mongo
package typeadapter

import org.bson.BsonDateTime

class BsonDateTimeSerializer(containerTypeAdapter: TypeAdapter[DateContainer]) extends Serializer[BsonDateTime] {

  override def serialize[AST, S](tagged: TypeTagged[BsonDateTime])(implicit ops: AstOps[AST, S], guidance: SerializationGuidance): SerializationResult[AST] =
    tagged match {
      case TypeTagged(null) => SerializationSuccess(AstNull())
      case TypeTagged(datetime) =>
        containerTypeAdapter.serializer.serialize(TypeTagged(DateContainer(datetime.getValue), typeOf[DateContainer]))
    }
}
