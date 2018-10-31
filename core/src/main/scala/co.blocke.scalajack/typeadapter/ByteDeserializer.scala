package co.blocke.scalajack
package typeadapter

class ByteDeserializer extends Deserializer[Byte] {

  self =>

  override def deserialize[AST, S](path: Path, ast: AST)(implicit ops: AstOps[AST, S], guidance: SerializationGuidance): DeserializationResult[Byte] =
    ast match {
      case AstInt(intValue) if (intValue >= -128 && intValue <= 127) => DeserializationSuccess(TypeTagged(intValue.byteValue))
      case AstInt(_) => DeserializationFailure(path, DeserializationError.Unexpected("Byte value out of range", reportedBy = self))
      case AstLong(longValue) if (longValue >= -128 && longValue <= 127) => DeserializationSuccess(TypeTagged(longValue.byteValue))
      case AstLong(_) => DeserializationFailure(path, DeserializationError.Unexpected("Byte value out of range", reportedBy = self))
      case AstString(s) if (guidance.isMapKey) => this.deserialize(path, ops.parse(s.asInstanceOf[S]))(ops, guidance = guidance.copy(isMapKey = false))
      case _ =>
        DeserializationFailure(path, DeserializationError.Unexpected("Expected a JSON number (byte)", reportedBy = self))
    }

}
