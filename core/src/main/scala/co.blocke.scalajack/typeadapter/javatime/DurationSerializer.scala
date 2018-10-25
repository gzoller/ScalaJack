package co.blocke.scalajack
package typeadapter
package javatime

import java.time.Duration

class DurationSerializer extends Serializer[Duration] {

  override def serialize[AST, S](tagged: TypeTagged[Duration])(implicit ops: AstOps[AST, S], guidance: SerializationGuidance): SerializationResult[AST] =
    tagged match {
      case TypeTagged(null) => SerializationSuccess(AstNull())
      case TypeTagged(x)    => SerializationSuccess(AstString(x.toString))
    }

}
