package co.blocke.scalajack
package typeadapter

import co.blocke.scalajack.typeadapter.TupleTypeAdapter.Field

import scala.collection.immutable

class TupleSerializer[Tuple](fields: Seq[Field[Tuple]]) extends Serializer[Tuple] {

  override def serialize[J](taggedTuple: TypeTagged[Tuple])(implicit ops: JsonOps[J], guidance: SerializationGuidance): SerializationResult[J] =
    taggedTuple match {
      case TypeTagged(null) =>
        SerializationSuccess(JsonNull())

      case TypeTagged(_) =>
        val errorsBuilder = immutable.Seq.newBuilder[SerializationError]

        val json = JsonArray[J] { appendElement =>
          for (field <- fields) {
            val taggedFieldValue = field.valueIn(taggedTuple)
            val fieldSerializationResult = field.valueSerializer.serialize[J](taggedFieldValue)
            fieldSerializationResult match {
              case SerializationSuccess(fieldValueJson) =>
                appendElement(fieldValueJson)

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
