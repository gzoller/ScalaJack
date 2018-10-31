package co.blocke.scalajack
package mongo
package typeadapter

import org.mongodb.scala.bson.BsonDateTime

class BsonDateTimeSerializer() extends Serializer[MongoTime] {

  override def serialize[AST, S](tagged: TypeTagged[MongoTime])(implicit ops: AstOps[AST, S], guidance: SerializationGuidance): SerializationResult[AST] =
    tagged match {
      case TypeTagged(datetime: MongoTime) =>
        if (ops == BsonOps)
          SerializationSuccess(new BsonDateTime(datetime).asInstanceOf[AST])
        else
          SerializationSuccess(AstLong(datetime))
    }
}
