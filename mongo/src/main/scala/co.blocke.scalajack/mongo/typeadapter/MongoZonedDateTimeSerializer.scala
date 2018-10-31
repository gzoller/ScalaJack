package co.blocke.scalajack
package mongo
package typeadapter

import java.time.ZonedDateTime
import org.bson.BsonDateTime

class MongoZonedDateTimeSerializer() extends Serializer[ZonedDateTime] {

  override def serialize[AST, S](tagged: TypeTagged[ZonedDateTime])(implicit ops: AstOps[AST, S], guidance: SerializationGuidance): SerializationResult[AST] =
    tagged match {
      case TypeTagged(null) => SerializationSuccess(AstNull())
      case TypeTagged(zoned) =>
        val toBson = new BsonDateTime(zoned.toInstant.toEpochMilli)
        SerializationSuccess(toBson.asInstanceOf[AST])
    }
}
