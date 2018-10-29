package co.blocke.scalajack
package mongo
package typeadapter

import org.bson.BsonDateTime

class BsonDateTimeDeserializer(containerTypeAdapter: TypeAdapter[DateContainer]) extends Deserializer[BsonDateTime] {

  self =>

  override def deserialize[AST, S](path: Path, ast: AST)(implicit ops: AstOps[AST, S], guidance: SerializationGuidance): DeserializationResult[BsonDateTime] =
    ast match {
      case AstNull() => DeserializationSuccess(TypeTagged(null, typeOf[BsonDateTime]))
      case AstObject(_) =>
        containerTypeAdapter.deserializer.deserialize(path, ast) match {
          case DeserializationSuccess(dt) =>
            val bsonDT: BsonDateTime = new BsonDateTime(dt.get.$date)
            DeserializationSuccess(TypeTagged(bsonDT, typeOf[BsonDateTime]))
          case fail: DeserializationFailure =>
            fail
        }
      case _ =>
        DeserializationFailure(path, DeserializationError.Unexpected("Expected a BsonDateTime value", reportedBy = self))
    }

}
