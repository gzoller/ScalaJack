package co.blocke.scalajack
package mongo
package typeadapter

import org.bson.BsonDateTime
import java.time._

class MongoOffsetDateTimeDeserializer(now: OffsetDateTime) extends Deserializer[OffsetDateTime] {

  self =>

  override def deserialize[AST, S](path: Path, ast: AST)(implicit ops: AstOps[AST, S], guidance: SerializationGuidance): DeserializationResult[OffsetDateTime] =
    ast match {
      case AstNull() => DeserializationSuccess(TypeTagged(null, typeOf[OffsetDateTime]))
      case bsonDateTime: BsonDateTime =>
        val result = OffsetDateTime.ofInstant(Instant.ofEpochMilli(bsonDateTime.getValue()), ZoneOffset.UTC)
        val normalized = result.withOffsetSameInstant(now.getOffset)
        DeserializationSuccess(TypeTagged(normalized, typeOf[OffsetDateTime]))
      case _ =>
        DeserializationFailure(path, DeserializationError.Unexpected("Expected a OffsetDateTime value", reportedBy = self))
    }

}
