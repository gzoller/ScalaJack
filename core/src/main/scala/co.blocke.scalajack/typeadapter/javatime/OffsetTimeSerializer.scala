package co.blocke.scalajack
package typeadapter
package javatime

import java.time.OffsetTime
import java.time.format.DateTimeFormatter

class OffsetTimeSerializer(formatter: DateTimeFormatter) extends Serializer[OffsetTime] {

  override def serialize[AST, S](tagged: TypeTagged[OffsetTime])(implicit ops: AstOps[AST, S], guidance: SerializationGuidance): SerializationResult[AST] =
    tagged match {
      case TypeTagged(null) => SerializationSuccess(AstNull())
      case TypeTagged(x)    => SerializationSuccess(AstString(x.format(formatter)))
    }

}
