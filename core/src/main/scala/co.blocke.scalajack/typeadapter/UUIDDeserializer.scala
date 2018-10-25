package co.blocke.scalajack
package typeadapter

import java.util.UUID

class UUIDDeserializer extends Deserializer[UUID] {

  self =>

  private val UUIDType: Type = typeOf[UUID]

  override def deserialize[AST, S](path: Path, ast: AST)(implicit ops: AstOps[AST, S], guidance: SerializationGuidance): DeserializationResult[UUID] =
    ast match {
      case AstNull() => DeserializationSuccess(TypeTagged(null, UUIDType))
      case AstString(x) => DeserializationResult(path)(TypeTagged(UUID.fromString(x), UUIDType), {
        case e: IllegalArgumentException =>
          DeserializationError.Malformed(e, reportedBy = self)
      })
      case _ => DeserializationFailure(path, DeserializationError.Unexpected("Expected a JSON string", reportedBy = self))
    }

}
