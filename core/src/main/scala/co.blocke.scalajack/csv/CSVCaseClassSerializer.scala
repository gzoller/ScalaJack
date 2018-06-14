package co.blocke.scalajack
package csv

import co.blocke.scalajack.csv.CSVCaseClassTypeAdapter.Member

class CSVCaseClassSerializer[CC](members: Seq[Member[CC]])(implicit tt: TypeTag[CC]) extends Serializer[CC] {

  override def serialize[J](tagged: TypeTagged[CC])(implicit ops: JsonOps[J]): SerializationResult[J] =
    tagged match {
      case TypeTagged(null) =>
        SerializationSuccess(JsonNull())

      case TypeTagged(instance) =>
        SerializationSuccess(JsonArray { appendElement =>
          for (member <- members) {
            val SerializationSuccess(memberValueJson) = member.serializeValue(member.valueIn(tagged))
            appendElement(memberValueJson)
          }
        })

    }

}
