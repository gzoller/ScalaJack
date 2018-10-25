package co.blocke.scalajack
package typeadapter

object TypeSerializer {

  def typeToTypeName(tpe: Type): String = tpe.typeSymbol.fullName

}

class TypeSerializer(typeToTypeName: Type => String = TypeSerializer.typeToTypeName) extends Serializer[Type] {

  override def serialize[AST, S](tagged: TypeTagged[Type])(implicit ops: AstOps[AST, S], guidance: SerializationGuidance): SerializationResult[AST] =
    tagged match {
      case TypeTagged(null) => SerializationSuccess(AstNull())
      case TypeTagged(tpe)  => SerializationSuccess(AstString(typeToTypeName(tpe)))
    }

}
