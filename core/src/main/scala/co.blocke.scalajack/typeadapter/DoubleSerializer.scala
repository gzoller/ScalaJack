package co.blocke.scalajack
package typeadapter

class DoubleSerializer extends Serializer[Double] {

  override def serialize[AST, S](tagged: TypeTagged[Double])(implicit ops: AstOps[AST, S], guidance: SerializationGuidance): SerializationResult[AST] =
    tagged match {
      case TypeTaggedDouble(doubleValue) => SerializationSuccess(AstDouble(doubleValue))
      case TypeTagged(doubleValue)       => SerializationSuccess(AstDouble(doubleValue))
    }

}
