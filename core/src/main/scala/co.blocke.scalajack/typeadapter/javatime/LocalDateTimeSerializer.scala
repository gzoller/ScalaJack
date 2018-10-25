package co.blocke.scalajack
package typeadapter
package javatime

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

class LocalDateTimeSerializer(formatter: DateTimeFormatter) extends Serializer[LocalDateTime] {

  override def serialize[AST, S](tagged: TypeTagged[LocalDateTime])(implicit ops: AstOps[AST, S], guidance: SerializationGuidance): SerializationResult[AST] =
    tagged match {
      case TypeTagged(null) => SerializationSuccess(AstNull())
      case TypeTagged(x)    => SerializationSuccess(AstString(x.format(formatter)))
    }

}
