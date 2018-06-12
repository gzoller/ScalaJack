package co.blocke.scalajack
package typeadapter

class DoubleSerializer extends Serializer[Double] {

  override def serialize[J](tagged: TypeTagged[Double])(implicit ops: JsonOps[J]): SerializationResult[J] =
    tagged match {
      case TypeTaggedDouble(doubleValue) => SerializationSuccess(JsonDouble(doubleValue))
      case TypeTagged(doubleValue)       => SerializationSuccess(JsonDouble(doubleValue))
    }

}
