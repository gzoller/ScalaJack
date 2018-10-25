package co.blocke.scalajack
package typeadapter

import co.blocke.scalajack.typeadapter.PlainClassTypeAdapter.PlainFieldMember

class PlainClassSerializer[C](members: List[PlainFieldMember[C]], isSJCapture: Boolean) extends Serializer[C] {

  override def serialize[AST, S](tagged: TypeTagged[C])(implicit ops: AstOps[AST, S], guidance: SerializationGuidance): SerializationResult[AST] =
    tagged match {
      case TypeTagged(null) =>
        SerializationSuccess(AstNull())

      case TypeTagged(obj) =>
        SerializationSuccess(AstObject { appendField =>
          for (member <- members) {
            val memberName = member.name
            val memberValue = member.valueIn(tagged)

            member.serializeValue(memberValue) match {
              case SerializationSuccess(memberValueAst)                            => appendField(memberName, memberValueAst)
              case SerializationFailure(f) if f == Seq(SerializationError.Nothing) => // do nothing--ignore optional fields of value None
            }

            if (isSJCapture) {
              val sjc = obj.asInstanceOf[SJCapture]
              sjc.captured.map { cap =>
                cap.astOps.foreachObjectField(cap.capturedFields.asInstanceOf[cap.astOps.ObjectFields], { (memberName, memberValue) =>
                  appendField(memberName, AstValue.transform[cap.ASTType, AST, cap.SrcType, S](memberValue)(cap.astOps, ops))
                })
              }
            }

          }
        })
    }

}
