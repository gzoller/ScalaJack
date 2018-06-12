package co.blocke.scalajack
package typeadapter

class FloatSerializer extends Serializer[Float] {

  override def serialize[J](tagged: TypeTagged[Float])(implicit ops: JsonOps[J]): SerializationResult[J] =
    tagged match {
      case TypeTaggedFloat(floatValue) => SerializationSuccess(JsonDouble(floatValue.doubleValue))
    }

}
