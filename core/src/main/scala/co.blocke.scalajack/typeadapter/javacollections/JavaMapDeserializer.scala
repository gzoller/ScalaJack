package co.blocke.scalajack
package typeadapter
package javacollections

class JavaMapDeserializer[K, V, M <: java.util.Map[K, V]](keyDeserializer: Deserializer[K], valueDeserializer: Deserializer[V], newEmptyMap: () => M)(implicit tt: TypeTag[M]) extends Deserializer[M] {

  private class TaggedMap(override val get: M, taggedKeys: List[TypeTagged[K]], taggedValues: List[TypeTagged[V]]) extends TypeTagged[M] {
    override lazy val tpe: Type = ???
  }

  override def deserialize[J](path: Path, json: J)(implicit ops: JsonOps[J]): DeserializationResult[M] =
    json match {
      case JsonObject(x) =>
        val objectFields = x.asInstanceOf[ops.ObjectFields]

        val map: M = newEmptyMap()

        ops.foreachObjectField(objectFields, { (fieldName, fieldValue) =>
          val DeserializationSuccess(TypeTagged(key)) = keyDeserializer.deserialize(path \ fieldName, JsonString(fieldName))
          ???
        })

        ???
    }

}
