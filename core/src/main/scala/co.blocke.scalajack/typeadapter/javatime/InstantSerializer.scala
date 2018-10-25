package co.blocke.scalajack
package typeadapter
package javatime

import java.time.Instant

class InstantSerializer extends Serializer[Instant] {

  override def serialize[AST, S](tagged: TypeTagged[Instant])(implicit ops: AstOps[AST, S], guidance: SerializationGuidance): SerializationResult[AST] =
    tagged match {
      case TypeTagged(null) => SerializationSuccess(AstNull())
      case TypeTagged(x)    => SerializationSuccess(AstString(x.toString))
    }

}
