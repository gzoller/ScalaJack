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

  override def deserializeFromNothing[J](path: Path)(implicit ops: JsonOps[J]): DeserializationResult[T] =
    ref.get().deserializeFromNothing[J](path)

  override def deserialize[J](path: Path, json: J)(implicit ops: JsonOps[J]): DeserializationResult[T] =
    ref.get().deserialize[J](path, json)

}
