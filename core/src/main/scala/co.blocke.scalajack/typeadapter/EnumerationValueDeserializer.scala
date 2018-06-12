package co.blocke.scalajack
package typeadapter

class EnumerationValueDeserializer[E <: Enumeration](enumeration: E)(implicit tt: TypeTag[E#Value]) extends Deserializer[E#Value] {

  private val enumerationName: String = enumeration.getClass.getName
  private val enumerationValueType: Type = tt.tpe

  override def deserialize[J](path: Path, json: J)(implicit ops: JsonOps[J]): DeserializationResult[E#Value] = {
    json match {
      case JsonString(name) =>
        DeserializationResult(path)(TypeTagged(enumeration.withName(name), enumerationValueType), {
          case _: NoSuchElementException =>
            DeserializationError.Malformed(s"Enumeration $enumerationName does not contain a value named $name")
        })

      case JsonLong(index) =>
        DeserializationResult(path)(TypeTagged(enumeration(index.intValue), enumerationValueType), {
          case _: NoSuchElementException =>
            DeserializationError.Malformed(s"Enumeration $enumerationName does not contain a value at index $index")
        })

      case JsonNull() =>
        DeserializationSuccess(TypeTagged(null, enumerationValueType))

      case _ =>
        DeserializationFailure(path, DeserializationError.Unsupported("Expected a JSON string or int"))
    }

  }

}
