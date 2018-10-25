package co.blocke.scalajack
package typeadapter

class FloatSerializer extends Serializer[Float] {

  // Bizzare set of magic to try to "fix" the precision slop when moving from Float->Double (prints extra digits in JSON)
  private def capFloat(f: Float): Double = {
    val d = f.toString.toDouble
    val diff = f.toDouble - d
    f - diff
  }

  override def serialize[AST, S](tagged: TypeTagged[Float])(implicit ops: AstOps[AST, S], guidance: SerializationGuidance): SerializationResult[AST] = {
    tagged match {
      case TypeTaggedFloat(floatValue) => SerializationSuccess(AstDouble(capFloat(floatValue)))
      case TypeTagged(floatValue)      => SerializationSuccess(AstDouble(capFloat(floatValue)))
    }
  }

}
