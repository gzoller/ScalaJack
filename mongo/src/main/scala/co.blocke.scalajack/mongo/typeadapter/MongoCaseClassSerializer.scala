package co.blocke.scalajack
package mongo
package typeadapter

import co.blocke.scalajack.typeadapter.{ CaseClassTypeAdapter, ClassFieldMember, ClassSerializer }

class MongoCaseClassSerializer[C](
    dbKeys:            List[ClassFieldMember[C]],
    idMemberName:      String,
    context:           Context,
    constructorMirror: MethodMirror,
    typeSerializer:    Serializer[Type],
    typeMembers:       List[CaseClassTypeAdapter.TypeMember[C]],
    fieldMembers:      List[ClassFieldMember[C]],
    isSJCapture:       Boolean)(implicit tt: TypeTag[C]) extends ClassSerializer[C](context, constructorMirror, typeSerializer, typeMembers, fieldMembers, isSJCapture) {

  override protected def handleDBKeys[AST, S](ast: AST, members: List[ClassFieldMember[C]])(implicit ops: AstOps[AST, S]): AST = {

    val keysWithIndex = members.zipWithIndex.filter { case (member, idx) => member.dbKeyIndex == idx }

    //    val dbkeyFields = keysWithIndex.map(_._1)
    //    val dbkeyFieldNames = dbkeyFields.map(_.name)
    //    val (astDBfields, astNonDBfields) = ops.partitionObjectFields(ast.asInstanceOf[ops.ObjectFields], { case (fieldname, _) => dbkeyFieldNames.contains(fieldname) })

    dbKeys.size match {
      case 0 => ast // no db keys specified... do nothing and return original ast
      case 1 => // simplified _id : value notation
        val keyFieldName = dbKeys.head.name
        val fixed = ops.mapObjectFields(ast.asInstanceOf[ops.ObjectFields], { (fieldname, value) =>
          fieldname match {
            case s: String if s == keyFieldName => (idMemberName, value)
            case _                              => (fieldname, value)
          }
        })
        ops.applyObject { appendField =>
          for ((fieldName, fieldValue) <- fixed) {
            appendField(fieldName, fieldValue)
          }
        }
      case _ => ??? // compound notation _id : { key:value, key:value}
    }
  }
}
