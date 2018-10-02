package co.blocke.scalajack
package typeadapter

object TypeSerializer {

  def typeToTypeName(tpe: Type): String = tpe.typeSymbol.fullName

}

class TypeSerializer(typeToTypeName: Type => String = TypeSerializer.typeToTypeName) extends Serializer[Type] {

  override def serialize[J](tagged: TypeTagged[Type])(implicit ops: JsonOps[J], guidance: SerializationGuidance): SerializationResult[J] =
    tagged match {
      case TypeTagged(null) => SerializationSuccess(JsonNull())
      case TypeTagged(tpe)  => SerializationSuccess(JsonString(typeToTypeName(tpe)))
    }

}
