package co.blocke.scalajack
package typeadapter

import co.blocke.scalajack.typeadapter.TupleTypeAdapter.Field

import scala.collection.immutable

class TupleSerializer[Tuple](fields: Seq[Field[Tuple]]) extends Serializer[Tuple] {

  override def serialize[AST, S](taggedTuple: TypeTagged[Tuple])(implicit ops: AstOps[AST, S], guidance: SerializationGuidance): SerializationResult[AST] =
    taggedTuple match {
      case TypeTagged(null) =>
        SerializationSuccess(AstNull())

      case TypeTagged(_) =>
        val errorsBuilder = immutable.Seq.newBuilder[SerializationError]

        val json = AstArray[AST, S] { appendElement =>
          for (field <- fields) {
            val taggedFieldValue = field.valueIn(taggedTuple)
            val fieldSerializationResult = field.valueSerializer.serialize[AST, S](taggedFieldValue)
            fieldSerializationResult match {
              case SerializationSuccess(fieldValueAst) =>
                appendElement(fieldValueAst)

              case SerializationFailure(fieldErrors) =>
                if (fieldErrors.head.toString == "Nothing")
                  appendElement(ops.applyNull)
                else
                  errorsBuilder ++= fieldErrors
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
