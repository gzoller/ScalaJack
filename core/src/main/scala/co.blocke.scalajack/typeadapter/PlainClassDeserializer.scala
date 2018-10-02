package co.blocke.scalajack
package typeadapter

import co.blocke.scalajack.typeadapter.PlainClassTypeAdapter.PlainFieldMember

import scala.collection.{ immutable, mutable }

class PlainClassDeserializer[C](members: List[PlainFieldMember[C]], newInstance: () => C)(implicit tt: TypeTag[C]) extends Deserializer[C] {

  self =>

  private type Member = PlainFieldMember[C]
  private val instanceType: Type = tt.tpe
  private val nullTypeTagged: TypeTagged[C] = TypeTagged(null.asInstanceOf[C], instanceType)
  private val membersByName: Map[String, Member] = members.map(member => member.name -> member).toMap

  override def deserialize[J](path: Path, json: J)(implicit ops: JsonOps[J], guidance: SerializationGuidance): DeserializationResult[C] =
    json match {
      case JsonNull() =>
        DeserializationSuccess(nullTypeTagged)

      case JsonObject(x) =>
        DeserializationResult.trapExceptions(path) {
          val fields = x.asInstanceOf[ops.ObjectFields]

          val deserializationResultsByMember = new mutable.HashMap[Member, DeserializationResult[Any]]

          ops.foreachObjectField(fields, { (name, valueJson) =>
            for (member <- membersByName.get(name)) {
              val deserializationResult = member.deserializer.deserialize(path \ name, valueJson)
              deserializationResultsByMember += member -> deserializationResult
            }
          })

          // TODO handle exceptions

          for (member <- members if !deserializationResultsByMember.contains(member)) {
            deserializationResultsByMember += member -> member.deserializer.deserializeFromNothing(path \ member.name)
          }

          if (deserializationResultsByMember.values.exists(_.isFailure)) {
            DeserializationFailure(deserializationResultsByMember.values.flatMap(_.errors).to[immutable.Seq])
          } else {
            val instance = newInstance()

            for ((member, deserializationResult) <- deserializationResultsByMember) {
              val DeserializationSuccess(TypeTagged(memberValue)) = deserializationResult
              member.valueSet(instance, memberValue.asInstanceOf[member.Value])
            }

            DeserializationSuccess(TypeTagged(instance, instanceType))
          }
        }

      case _ =>
        DeserializationFailure(path, DeserializationError.Unexpected("Expected a JSON object", reportedBy = self))
    }

}
