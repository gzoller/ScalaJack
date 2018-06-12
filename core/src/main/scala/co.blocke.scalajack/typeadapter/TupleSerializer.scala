package co.blocke.scalajack
package typeadapter

import co.blocke.scalajack.typeadapter.TupleTypeAdapter.Field

class TupleSerializer[Tuple](fields: Seq[Field[Tuple]]) extends Serializer[Tuple] {

  override def serialize[J](taggedTuple: TypeTagged[Tuple])(implicit ops: JsonOps[J]): SerializationResult[J] =
    taggedTuple match {
      case TypeTagged(null) =>
        SerializationSuccess(JsonNull())

      case TypeTagged(_) =>
        SerializationSuccess(JsonArray { appendElement =>
          for (field <- fields) {
            val taggedFieldValue = field.valueIn(taggedTuple)
            val SerializationSuccess(fieldValueJson) = field.valueSerializer.serialize[J](taggedFieldValue)
            appendElement(fieldValueJson)
          }
        })
    }

}
