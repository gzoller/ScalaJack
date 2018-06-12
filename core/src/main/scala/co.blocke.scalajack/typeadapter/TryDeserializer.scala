package co.blocke.scalajack
package typeadapter

import scala.util.control.NonFatal
import scala.util.{ Failure, Success, Try }

class TryDeserializer[T](next: Deserializer[T])(implicit tt: TypeTag[T]) extends Deserializer[Try[T]] {

  private val successTypeConstructor: Type = typeOf[Success[_]].typeConstructor
  private val failureType: Type = appliedType(typeOf[Failure[_]].typeConstructor, tt.tpe)

  private class TaggedSuccess(override val get: Success[T], taggedValue: TypeTagged[T]) extends TypeTagged[Success[T]] {
    override lazy val tpe: Type = appliedType(successTypeConstructor, taggedValue.tpe)
  }

  override def deserialize[J](path: Path, json: J)(implicit ops: JsonOps[J]): DeserializationResult[Try[T]] =
    try {
      next.deserialize(path, json) map {
        case tagged @ TypeTagged(value) =>
          new TaggedSuccess(Success(value), tagged)
      }
    } catch {
      case NonFatal(e) =>
        DeserializationSuccess(TypeTagged(Failure(e), failureType))
    }

}
