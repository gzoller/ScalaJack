package co.blocke.scalajack
package typeadapter
package javatime

import java.time.Period

class PeriodSerializer extends Serializer[Period] {

  override def serialize[J](tagged: TypeTagged[Period])(implicit ops: JsonOps[J]): SerializationResult[J] =
    tagged match {
      case TypeTagged(null) => SerializationSuccess(JsonNull())
      case TypeTagged(x)    => SerializationSuccess(JsonString(x.toString))
    }

}
