package co.blocke.scalajack
package mongo
package typeadapter

import org.bson.BsonDateTime

class BsonDateTimeDeserializer() extends Deserializer[MongoTime] {

  self =>

  override def deserialize[AST, S](path: Path, ast: AST)(implicit ops: AstOps[AST, S], guidance: SerializationGuidance): DeserializationResult[MongoTime] =
    ast match {
      case bsonDT: BsonDateTime =>
        DeserializationSuccess(TypeTagged(bsonDT.getValue(), typeOf[MongoTime]))
      case AstLong(dt) if (ops != BsonOps) =>
        DeserializationSuccess(TypeTagged(dt, typeOf[MongoTime]))
      case _ =>
        DeserializationFailure(path, DeserializationError.Unexpected("Expected a BsonDateTime value", reportedBy = self))
    }

}
