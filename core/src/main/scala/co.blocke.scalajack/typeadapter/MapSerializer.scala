package co.blocke.scalajack
package typeadapter

import scala.collection.GenMap
import scala.collection.immutable

class MapSerializer[K, V, M <: GenMap[K, V]](keySerializer: Serializer[K], valueSerializer: Serializer[V], context: Context) extends Serializer[M] {

  override def serialize[AST, S](tagged: TypeTagged[M])(implicit ops: AstOps[AST, S], guidance: SerializationGuidance): SerializationResult[AST] =
    tagged match {
      case TypeTagged(null) => SerializationSuccess(AstNull())
      case TypeTagged(map) =>
        lazy val mapType: Type = tagged.tpe.baseType(symbolOf[GenMap[_, _]])

        lazy val keyType: Type = {
          val k :: _ :: Nil = mapType.typeArgs
          k
        }

        lazy val valueType: Type = {
          val _ :: v :: Nil = mapType.typeArgs
          v
        }

        class TaggedKey(override val get: K) extends TypeTagged[K] {
          override def tpe: Type = keyType
        }

        class TaggedValue(override val get: V) extends TypeTagged[V] {
          override def tpe: Type = valueType
        }

        val errorsBuilder = immutable.Seq.newBuilder[SerializationError]

        val json = AstObject[AST, S] { appendField =>
          map foreach {
            case (key, value) =>
              val keySerializationResult = keySerializer.serialize(new TaggedKey(key))(ops, guidance.withMapKey())
              val valueSerializationResult = valueSerializer.serialize(new TaggedValue(value))(ops, guidance.withMapValue())

              (keySerializationResult, valueSerializationResult) match {
                case (SerializationSuccess(keyAst), SerializationSuccess(valueAst)) =>
                  val keyString =
                    keyAst match {
                      // Problem:  Map keys are strings in Ast, but we don't want to presume the same for
                      // non-json Map representations!  Therefor keyAst needs to be a dependent value, either String or ???
                      case AstString(s)                => s
                      case _ if (guidance.isCanonical) => ops.renderCompact(keyAst, context.sjFlavor.get).asInstanceOf[String]
                      case _                           => No_Quote_Marker + ops.renderCompact(keyAst, context.sjFlavor.get).toString
                    }
                  appendField(keyString, valueAst)

                case (SerializationSuccess(keyAst), SerializationFailure(Seq(SerializationError.Nothing))) =>
                  val keyString =
                    keyAst match {
                      case AstString(s) => s
                      case _            => ops.renderCompact(keyAst, context.sjFlavor.get).toString
                    }
                  appendField(keyString, AstNull())

                case _ =>
                  errorsBuilder ++= keySerializationResult.errors
                  errorsBuilder ++= valueSerializationResult.errors
              }
          }
        }

        val errors = errorsBuilder.result()

        if (errors.nonEmpty) {
          SerializationFailure(errors)
        } else {
          SerializationSuccess(json)
        }

    }

}
