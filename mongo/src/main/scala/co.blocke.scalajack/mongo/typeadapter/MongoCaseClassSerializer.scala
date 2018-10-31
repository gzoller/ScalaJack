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

    dbKeys.size match {
      case 0 => ast // no db keys specified... do nothing and return original ast
      case 1 => // simplified _id : value notation
        val keyFieldName = dbKeys.head.name
        ops.mapObjectFields(ast.asInstanceOf[ops.ObjectFields], { (fieldname, value) =>
          fieldname match {
            case s: String if s == keyFieldName => (idMemberName, value)
            case _                              => (fieldname, value)
          }
        }).asInstanceOf[AST]
      case _ => // compound notation _id : { key:value, key:value}
        val dbkeyFieldNames = dbKeys.map(_.name)
        val (astDBfields, astNonDBfields) = ops.partitionObjectFields(ast.asInstanceOf[ops.ObjectFields], dbkeyFieldNames)
        val id = ops.applyObject(appendField => appendField(idMemberName, astDBfields.asInstanceOf[AST]))
        ops.mergeObjectFields(id.asInstanceOf[ops.ObjectFields], astNonDBfields.asInstanceOf[ops.ObjectFields]).asInstanceOf[AST]
    }
  }
}
