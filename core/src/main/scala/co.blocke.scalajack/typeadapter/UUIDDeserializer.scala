package co.blocke.scalajack
package typeadapter

import java.util.UUID

class UUIDDeserializer extends Deserializer[UUID] {

  self =>

  private val UUIDType: Type = typeOf[UUID]

  override def deserialize[J](path: Path, json: J)(implicit ops: JsonOps[J]): DeserializationResult[UUID] =
    json match {
      case JsonNull() => DeserializationSuccess(TypeTagged(null, UUIDType))
      case JsonString(x) => DeserializationResult(path)(TypeTagged(UUID.fromString(x), UUIDType), {
        case e: IllegalArgumentException =>
          DeserializationError.Malformed(e, reportedBy = self)
      })
      case _ => DeserializationFailure(path, DeserializationError.Unexpected("Expected a JSON string", reportedBy = self))
    }

}
