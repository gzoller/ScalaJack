package co.blocke.scalajack
package typeadapter

import co.blocke.scalajack.typeadapter.PlainClassTypeAdapter.PlainFieldMember

import scala.collection.{ immutable, mutable }

class PlainClassDeserializer[C](members: List[PlainFieldMember[C]], newInstance: () => C, isSJCapture: Boolean)(implicit tt: TypeTag[C]) extends Deserializer[C] {

  self =>

  private type Member = PlainFieldMember[C]
  private val instanceType: Type = tt.tpe
  private val nullTypeTagged: TypeTagged[C] = TypeTagged(null.asInstanceOf[C], instanceType)
  private val membersByName: Map[String, Member] = members.map(member => member.name -> member).toMap

  override def deserialize[AST, S](path: Path, ast: AST)(implicit ops: AstOps[AST, S], guidance: SerializationGuidance): DeserializationResult[C] =
    ast match {
      case AstNull() =>
        DeserializationSuccess(nullTypeTagged)

      case AstObject(x) =>
        DeserializationResult.trapExceptions(path) {
          val fields = x.asInstanceOf[ops.ObjectFields]

          val deserializationResultsByMember = new mutable.HashMap[Member, DeserializationResult[Any]]

          ops.foreachObjectField(fields, { (name, valueAst) =>
            for (member <- membersByName.get(name)) {
              val deserializationResult = member.deserializer.deserialize(path \ name, valueAst)
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

            if (isSJCapture) {
              val partitionedFields = ops.partitionObjectFields(fields, membersByName.keySet.toList)
              val captured = partitionedFields._2
              import AstOps._
              implicit val aux: Aux[AST, ops.ObjectFields, S] = ops.asInstanceOf[Aux[AST, ops.ObjectFields, S]]
              instance.asInstanceOf[SJCapture].captured = Some(AstAndOps(captured))
            }

            DeserializationSuccess(TypeTagged(instance, instanceType))
          }
        }

      case _ =>
        DeserializationFailure(path, DeserializationError.Unexpected("Expected a JSON object", reportedBy = self))
    }
}
