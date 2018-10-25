package co.blocke.scalajack
package typeadapter

import java.util.UUID

class UUIDSerializer extends Serializer[UUID] {

  override def serialize[AST, S](tagged: TypeTagged[UUID])(implicit ops: AstOps[AST, S], guidance: SerializationGuidance): SerializationResult[AST] =
    tagged match {
      case TypeTagged(null) => SerializationSuccess(AstNull())
      case TypeTagged(uuid) => SerializationSuccess(AstString(uuid.toString))
    }

}
