package co.blocke.scalajack
package typeadapter
package javatime

import java.time.Duration

class DurationSerializer extends Serializer[Duration] {

  override def serialize[J](tagged: TypeTagged[Duration])(implicit ops: JsonOps[J]): SerializationResult[J] =
    tagged match {
      case TypeTagged(null) => SerializationSuccess(JsonNull())
      case TypeTagged(x)    => SerializationSuccess(JsonString(x.toString))
    }

}
