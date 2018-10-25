package co.blocke.scalajack
package typeadapter
package javatime

import java.time.OffsetDateTime
import java.time.format.DateTimeFormatter

class OffsetDateTimeSerializer(formatter: DateTimeFormatter) extends Serializer[OffsetDateTime] {

  override def serialize[AST, S](tagged: TypeTagged[OffsetDateTime])(implicit ops: AstOps[AST, S], guidance: SerializationGuidance): SerializationResult[AST] =
    tagged match {
      case TypeTagged(null) => SerializationSuccess(AstNull())
      case TypeTagged(x)    => SerializationSuccess(AstString(x.format(formatter)))
    }

}
