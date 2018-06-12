package co.blocke.scalajack
package typeadapter

import java.util.UUID

class UUIDSerializer extends Serializer[UUID] {

  override def serialize[J](tagged: TypeTagged[UUID])(implicit ops: JsonOps[J]): SerializationResult[J] =
    tagged match {
      case TypeTagged(null) => SerializationSuccess(JsonNull())
      case TypeTagged(uuid) => SerializationSuccess(JsonString(uuid.toString))
    }

}
