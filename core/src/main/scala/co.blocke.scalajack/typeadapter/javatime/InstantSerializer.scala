package co.blocke.scalajack
package typeadapter
package javatime

import java.time.Instant

class InstantSerializer extends Serializer[Instant] {

  override def serialize[J](tagged: TypeTagged[Instant])(implicit ops: JsonOps[J]): SerializationResult[J] =
    tagged match {
      case TypeTagged(null) => SerializationSuccess(JsonNull())
      case TypeTagged(x)    => SerializationSuccess(JsonString(x.toString))
    }

}
