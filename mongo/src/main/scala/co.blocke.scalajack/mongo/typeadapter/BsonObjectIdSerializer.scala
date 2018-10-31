package co.blocke.scalajack
package mongo
package typeadapter

import org.mongodb.scala.bson.{ BsonObjectId, ObjectId => BObjId }

class BsonObjectIdSerializer() extends Serializer[ObjectId] {

  override def serialize[AST, S](tagged: TypeTagged[ObjectId])(implicit ops: AstOps[AST, S], guidance: SerializationGuidance): SerializationResult[AST] =
    tagged match {
      case TypeTagged(null) => SerializationSuccess(AstNull())
      case TypeTagged(oid: ObjectId) =>
        if (ops == BsonOps)
          SerializationSuccess(new BsonObjectId(new BObjId(oid)).asInstanceOf[AST])
        else
          SerializationSuccess(AstString(oid))
    }
}