package co.blocke.scalajack

import java.util.concurrent.atomic.AtomicReference

class SerializerReference[T](initialSerializer: Serializer[T]) extends Serializer[T] {

  private val ref = new AtomicReference[Serializer[T]]

  override def toString: String = s"SerializerReference($referencedSerializer)"

  def referencedSerializer: Serializer[T] = ref.get()

  def referencedSerializer_=(serializer: Serializer[T]): Unit = {
    require(serializer ne null, "Referenced serializer must not be null")
    ref.set(serializer)
  }

  referencedSerializer = initialSerializer

  override def serialize[AST, S](tagged: TypeTagged[T])(implicit ops: AstOps[AST, S], guidance: SerializationGuidance): SerializationResult[AST] =
    ref.get().serialize[AST, S](tagged)

}
