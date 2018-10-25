package co.blocke.scalajack
package typeadapter
package javacollections

class JavaMapSerializer[K, V, M <: java.util.Map[K, V]](keySerializer: Serializer[K], valueSerializer: Serializer[V]) extends Serializer[M] {

  private val mapSymbol: Symbol = symbolOf[java.util.Map[_, _]]

  override def serialize[AST, S](tagged: TypeTagged[M])(implicit ops: AstOps[AST, S], guidance: SerializationGuidance): SerializationResult[AST] =
    tagged match {
      case TypeTagged(null) =>
        SerializationSuccess(AstNull())

      case TypeTagged(map) =>
        lazy val baseType: Type = tagged.tpe.baseType(mapSymbol)

        lazy val keyType: Type = {
          val k :: _ :: Nil = baseType.typeArgs
          k
        }

        lazy val valueType: Type = {
          val _ :: v :: Nil = baseType.typeArgs
          v
        }

        class TaggedKey(override val get: K) extends TypeTagged[K] {
          override def tpe: Type = keyType
        }

        class TaggedValue(override val get: V) extends TypeTagged[V] {
          override def tpe: Type = valueType
        }

        SerializationSuccess(AstObject { appendField =>
          val iterator = map.entrySet().iterator()
          while (iterator.hasNext) {
            val mapEntry = iterator.next()
            val SerializationSuccess(AstString(keyString)) = keySerializer.serialize(new TaggedKey(mapEntry.getKey))
            val SerializationSuccess(valueAst) = valueSerializer.serialize(new TaggedValue(mapEntry.getValue))
            appendField(keyString, valueAst)
          }
        })
    }

}
