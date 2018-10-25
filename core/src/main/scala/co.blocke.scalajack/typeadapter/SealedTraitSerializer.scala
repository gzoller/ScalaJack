package co.blocke.scalajack
package typeadapter

import co.blocke.scalajack.typeadapter.SealedTraitSerializer.Implementation

import scala.collection.immutable

object SealedTraitSerializer {

  trait Implementation[T] {

    def isInstance(tagged: TypeTagged[T]): Boolean

    def serialize[AST, S](tagged: TypeTagged[T])(implicit ops: AstOps[AST, S], guidance: SerializationGuidance): SerializationResult[AST]

  }

}

class SealedTraitSerializer[T](implementations: immutable.Set[Implementation[T]]) extends Serializer[T] {

  override def serialize[AST, S](tagged: TypeTagged[T])(implicit ops: AstOps[AST, S], guidance: SerializationGuidance): SerializationResult[AST] =
    tagged match {
      case TypeTagged(null) => SerializationSuccess(AstNull())
      case TypeTagged(_) =>
        implementations.find(_.isInstance(tagged)) match {
          case Some(implementation) =>
            implementation.serialize(tagged)

          case None =>
            ???
        }
    }

}
