package co.blocke.scalajack
package csv

import co.blocke.scalajack.csv.CSVCaseClassTypeAdapter.Member

import scala.collection.immutable

class CSVCaseClassDeserializer[CC](
    members:           IndexedSeq[Member[_]],
    constructorMirror: MethodMirror)(implicit tt: TypeTag[CC]) extends Deserializer[CC] {

  self =>

  private val caseClassType: Type = tt.tpe
  private val nullTypeTagged: TypeTagged[CC] = TypeTagged[CC](null.asInstanceOf[CC], caseClassType)

  override def deserialize[J](path: Path, json: J)(implicit ops: JsonOps[J]): DeserializationResult[CC] =
    json match {
      case JsonNull() =>
        DeserializationSuccess(nullTypeTagged)

      case JsonArray(x) =>
        val elements = x.asInstanceOf[ops.ArrayElements]

        val deserializationResults: Array[DeserializationResult[Any]] = new Array[DeserializationResult[Any]](members.size)

        ops.foreachArrayElement(elements, { (index, element) =>
          deserializationResults(index) = members(index).valueTypeAdapter.deserializer.deserialize(path \ index, element)
        })

        if (deserializationResults.exists(_.isFailure)) {
          DeserializationFailure(deserializationResults.flatMap(_.errors).to[immutable.Seq])
        } else {
          val arguments = for (DeserializationSuccess(TypeTagged(argument)) <- deserializationResults) yield argument
          DeserializationSuccess(TypeTagged[CC](constructorMirror(arguments: _*).asInstanceOf[CC], caseClassType))
        }

      case _ =>
        DeserializationFailure(path, DeserializationError.Unsupported(s"Expected a JSON array, not $json", reportedBy = self))
    }

}
