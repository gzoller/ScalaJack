package co.blocke.scalajack
package typeadapter

import co.blocke.scalajack.typeadapter.TupleTypeAdapter.Field

import scala.collection.immutable

class TupleDeserializer[Tuple](fields: IndexedSeq[Field[Tuple]], tupleConstructorMirror: MethodMirror)(implicit tt: TypeTag[Tuple]) extends Deserializer[Tuple] {

  private val tupleTypeConstructor: Type = tt.tpe.typeConstructor

  private class TaggedTuple(override val get: Tuple, taggedElements: Array[TypeTagged[Any]]) extends TypeTagged[Tuple] {
    override lazy val tpe: Type = appliedType(tupleTypeConstructor, taggedElements.map(_.tpe).toList)
  }

  override def deserialize[J](path: Path, json: J)(implicit ops: JsonOps[J]): DeserializationResult[Tuple] =
    json match {
      case JsonArray(x) =>
        val elements = x.asInstanceOf[ops.ArrayElements]

        val deserializationResults: Array[DeserializationResult[Any]] = new Array[DeserializationResult[Any]](fields.length)

        ops.foreachArrayElement(elements, { (index, element) =>
          deserializationResults(index) = fields(index).valueDeserializer.deserialize(path \ index, element)
        })

        if (deserializationResults.exists(_.isFailure)) {
          DeserializationFailure(deserializationResults.flatMap(_.errors).to[immutable.Seq])
        } else {
          DeserializationResult(path)({
            val tuple = tupleConstructorMirror(deserializationResults.map(_.get.get): _*).asInstanceOf[Tuple]
            val taggedElements = deserializationResults.map(_.get)
            new TaggedTuple(tuple, taggedElements)
          })
        }

      case _ => DeserializationFailure(path, DeserializationError.Unsupported(s"Expected a JSON array, not $json"))
    }

}
