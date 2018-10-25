package co.blocke.scalajack
package typeadapter
package javacollections

class JavaMapDeserializer[K, V, M <: java.util.Map[K, V]](keyDeserializer: Deserializer[K], valueDeserializer: Deserializer[V], newEmptyMap: () => M)(implicit tt: TypeTag[M]) extends Deserializer[M] {

  self =>

  private val taggedNull: TypeTagged[M] = TypeTagged(null.asInstanceOf[M], tt.tpe)

  override def deserialize[AST, S](path: Path, ast: AST)(implicit ops: AstOps[AST, S], guidance: SerializationGuidance): DeserializationResult[M] =
    ast match {
      case AstObject(x) =>

        val objectFields = x.asInstanceOf[ops.ObjectFields]

        val map: M = newEmptyMap()

        ops.foreachObjectField(objectFields, { (fieldName, fieldValue) =>
          val DeserializationSuccess(TypeTagged(key)) = keyDeserializer.deserialize(path \ fieldName, AstString(fieldName))
          val DeserializationSuccess(TypeTagged(value)) = valueDeserializer.deserialize(path \ fieldName, fieldValue)
          map.put(key, value)
        })

        DeserializationSuccess(TypeTagged(map, tt.tpe))

      case AstNull() => DeserializationSuccess(taggedNull)

      case x =>
        println("X: " + x)
        DeserializationFailure(path, DeserializationError.Unsupported("Expected a JSON object", reportedBy = self))
    }

}
