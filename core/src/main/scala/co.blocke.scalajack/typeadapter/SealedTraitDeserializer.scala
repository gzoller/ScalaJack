package co.blocke.scalajack
package typeadapter

import co.blocke.scalajack.typeadapter.SealedTraitDeserializer.Implementation

import scala.collection.immutable

object SealedTraitDeserializer {

  trait Implementation[T] {

    val fieldNames: immutable.Set[String]

    val deserializer: Deserializer[T]

  }

}

class SealedTraitDeserializer[T](implementations: immutable.Set[Implementation[T]])(implicit tt: TypeTag[T]) extends Deserializer[T] {

  self =>

  override def deserialize[J](path: Path, json: J)(implicit ops: JsonOps[J]): DeserializationResult[T] =
    json match {
      case JsonObject(x) =>
        val fields = x.asInstanceOf[ops.ObjectFields]

        val allFieldNamesBuilder = immutable.Set.newBuilder[String]

        ops.foreachObjectField(fields, { (fieldName, _) =>
          allFieldNamesBuilder += fieldName
        })

        val allFieldNames: immutable.Set[String] = allFieldNamesBuilder.result()

        implementations.filter(implementation => implementation.fieldNames.subsetOf(allFieldNames)) match {
          case emptySet if emptySet.isEmpty =>
            throw new RuntimeException(s"No sub-classes of ${tt.tpe.typeSymbol.fullName} match field names $allFieldNames")

          case setOfOne if setOfOne.size == 1 =>
            val implementation = setOfOne.head
            implementation.deserializer.deserialize(path, json)

          case matchingSubclasses =>
            throw new RuntimeException(s"Multiple sub-classes of ${tt.tpe.typeSymbol.fullName} match field names $allFieldNames")
        }

      case _ =>
        DeserializationFailure(path, DeserializationError.Unsupported("Expected a JSON object", reportedBy = Some(self)))
    }

}
