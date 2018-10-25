package co.blocke.scalajack
package typeadapter
package javatime

import java.time.LocalTime
import java.time.format.DateTimeFormatter

class LocalTimeSerializer(formatter: DateTimeFormatter) extends Serializer[LocalTime] {

  override def serialize[AST, S](tagged: TypeTagged[LocalTime])(implicit ops: AstOps[AST, S], guidance: SerializationGuidance): SerializationResult[AST] =
    tagged match {
      case TypeTagged(null) => SerializationSuccess(AstNull())
      case TypeTagged(x)    => SerializationSuccess(AstString(x.format(formatter)))
    }

}
