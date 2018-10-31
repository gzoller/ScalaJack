package co.blocke.scalajack
package mongo
package typeadapter

import org.bson.BsonObjectId

class BsonObjectIdDeserializer() extends Deserializer[ObjectId] {

  self =>

  override def deserialize[AST, S](path: Path, ast: AST)(implicit ops: AstOps[AST, S], guidance: SerializationGuidance): DeserializationResult[ObjectId] =
    ast match {
      case AstNull() => DeserializationSuccess(TypeTagged(null.asInstanceOf[ObjectId], typeOf[ObjectId]))
      case bsoid: BsonObjectId =>
        DeserializationSuccess(TypeTagged(bsoid.getValue().toHexString(), typeOf[ObjectId]))
      case AstString(oid) if (ops != BsonOps) =>
        DeserializationSuccess(TypeTagged(oid, typeOf[ObjectId]))
      case _ =>
        DeserializationFailure(path, DeserializationError.Unexpected("Expected a Bson ObjectId value", reportedBy = self))
    }

}
