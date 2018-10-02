package co.blocke.scalajack
package typeadapter

import co.blocke.scalajack.typeadapter.SealedTraitSerializer.Implementation

import scala.collection.immutable

object SealedTraitSerializer {

  trait Implementation[T] {

    def isInstance(tagged: TypeTagged[T]): Boolean

    def serialize[J](tagged: TypeTagged[T])(implicit ops: JsonOps[J], guidance: SerializationGuidance): SerializationResult[J]

  }

}

class SealedTraitSerializer[T](implementations: immutable.Set[Implementation[T]]) extends Serializer[T] {

  override def serialize[J](tagged: TypeTagged[T])(implicit ops: JsonOps[J], guidance: SerializationGuidance): SerializationResult[J] =
    tagged match {
      case TypeTagged(null) => SerializationSuccess(JsonNull())
      case TypeTagged(x) =>
        implementations.find(_.isInstance(tagged)) match {
          case Some(implementation) =>
            implementation.serialize(tagged)

          case None =>
            ???
        }
    }

}
