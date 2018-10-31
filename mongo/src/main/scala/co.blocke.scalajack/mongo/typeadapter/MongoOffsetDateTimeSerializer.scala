package co.blocke.scalajack
package mongo
package typeadapter

import java.time.OffsetDateTime

import org.bson.BsonDateTime

class MongoOffsetDateTimeSerializer() extends Serializer[OffsetDateTime] {

  override def serialize[AST, S](tagged: TypeTagged[OffsetDateTime])(implicit ops: AstOps[AST, S], guidance: SerializationGuidance): SerializationResult[AST] =
    tagged match {
      case TypeTagged(null) => SerializationSuccess(AstNull())
      case TypeTagged(datetime) =>
        val toBson = new BsonDateTime(datetime.toZonedDateTime().toInstant.toEpochMilli)
        SerializationSuccess(toBson.asInstanceOf[AST])
    }
}
