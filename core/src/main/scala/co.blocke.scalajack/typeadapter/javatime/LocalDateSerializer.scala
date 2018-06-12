package co.blocke.scalajack
package typeadapter
package javatime

import java.time.LocalDate
import java.time.format.DateTimeFormatter

class LocalDateSerializer(formatter: DateTimeFormatter) extends Serializer[LocalDate] {

  override def serialize[J](tagged: TypeTagged[LocalDate])(implicit ops: JsonOps[J]): SerializationResult[J] =
    tagged match {
      case TypeTagged(null) => SerializationSuccess(JsonNull())
      case TypeTagged(x)    => SerializationSuccess(JsonString(x.format(formatter)))
    }

}
