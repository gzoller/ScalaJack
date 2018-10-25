package co.blocke.scalajack
package typeadapter
package javatime

import java.time.Period

class PeriodSerializer extends Serializer[Period] {

  override def serialize[AST, S](tagged: TypeTagged[Period])(implicit ops: AstOps[AST, S], guidance: SerializationGuidance): SerializationResult[AST] =
    tagged match {
      case TypeTagged(null) => SerializationSuccess(AstNull())
      case TypeTagged(x)    => SerializationSuccess(AstString(x.toString))
    }

}
