package co.blocke.scalajack
package typeadapter
package javatime

import java.time.LocalDate
import java.time.format.DateTimeFormatter

class LocalDateSerializer(formatter: DateTimeFormatter) extends Serializer[LocalDate] {

  override def serialize[AST, S](tagged: TypeTagged[LocalDate])(implicit ops: AstOps[AST, S], guidance: SerializationGuidance): SerializationResult[AST] =
    tagged match {
      case TypeTagged(null) => SerializationSuccess(AstNull())
      case TypeTagged(x)    => SerializationSuccess(AstString(x.format(formatter)))
    }

}
