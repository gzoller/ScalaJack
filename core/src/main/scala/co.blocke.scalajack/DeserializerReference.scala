package co.blocke.scalajack

import java.util.concurrent.atomic.AtomicReference

class DeserializerReference[T](initialDeserializer: Deserializer[T]) extends Deserializer[T] {

  private val ref = new AtomicReference[Deserializer[T]]

  override def toString: String = s"DeserializerReference($referencedDeserializer)"

  def referencedDeserializer: Deserializer[T] = ref.get()

  def referencedDeserializer_=(deserializer: Deserializer[T]): Unit = {
    require(deserializer ne null, "Referenced deserializer must not be null")
    ref.set(deserializer)
  }

  referencedDeserializer = initialDeserializer

  override def deserializeFromNothing[AST, S](path: Path)(implicit ops: AstOps[AST, S]): DeserializationResult[T] =
    ref.get().deserializeFromNothing[AST, S](path)

  override def deserialize[AST, S](path: Path, ast: AST)(implicit ops: AstOps[AST, S], guidance: SerializationGuidance): DeserializationResult[T] =
    ref.get().deserialize[AST, S](path, ast)

}
