package co.blocke.scalajack
package mongo
package typeadapter

import org.bson.BsonDateTime
import java.time._

class MongoZonedDateTimeDeserializer() extends Deserializer[ZonedDateTime] {

  self =>

  override def deserialize[AST, S](path: Path, ast: AST)(implicit ops: AstOps[AST, S], guidance: SerializationGuidance): DeserializationResult[ZonedDateTime] =
    ast match {
      case AstNull() => DeserializationSuccess(TypeTagged(null, typeOf[ZonedDateTime]))
      case bsonDateTime: BsonDateTime =>
        val result = ZonedDateTime.ofInstant(Instant.ofEpochMilli(bsonDateTime.getValue()), ZoneId.ofOffset("UTC", ZoneOffset.UTC))
        DeserializationSuccess(TypeTagged(result, typeOf[ZonedDateTime]))
      case _ =>
        DeserializationFailure(path, DeserializationError.Unexpected("Expected a ZonedDateTime value", reportedBy = self))
    }
}