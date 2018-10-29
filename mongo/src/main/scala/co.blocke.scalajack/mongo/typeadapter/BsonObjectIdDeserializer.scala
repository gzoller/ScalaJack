package co.blocke.scalajack
package mongo
package typeadapter

import org.bson.BsonObjectId

class BsonObjectIdDeserializer(containerTypeAdapter: TypeAdapter[ObjectId]) extends Deserializer[ObjectId] {

  self =>

  override def deserialize[AST, S](path: Path, ast: AST)(implicit ops: AstOps[AST, S], guidance: SerializationGuidance): DeserializationResult[ObjectId] =
    ast match {
      case AstNull() => DeserializationSuccess(TypeTagged(null.asInstanceOf[ObjectId], typeOf[ObjectId]))
      case AstObject(_) =>
        containerTypeAdapter.deserializer.deserialize(path, ast) match {
          case DeserializationSuccess(objid) =>
            val bsonObjId = objid.get.bsonObjectId
            //            val container = BsonObjectIdContainer(bsonObjId.getValue.toString)
            containerTypeAdapter.deserializer.deserialize(path, bsonObjId.asInstanceOf[AST])
          case fail: DeserializationFailure =>
            fail
        }
      case _ =>
        DeserializationFailure(path, DeserializationError.Unexpected("Expected a Bson ObjectId value", reportedBy = self))
    }

}
