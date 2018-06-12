package co.blocke.scalajack.typeadapter
package javacollections

import co.blocke.scalajack.{ Context, Deserializer, Reader, Serializer, TypeAdapter, TypeAdapterFactory, Writer }

import scala.reflect.runtime.currentMirror
import scala.reflect.runtime.universe.TypeTag

object JavaMapTypeAdapter extends TypeAdapterFactory.<:<.withTwoTypeParams[java.util.Map] {

  override def create[K, V, M <: java.util.Map[K, V]](next: TypeAdapterFactory)(implicit context: Context, tt: TypeTag[M], ttMap: TypeTag[java.util.Map[K, V]], ttKey: TypeTag[K], ttValue: TypeTag[V]): TypeAdapter[M] = {
    val mapClass: java.lang.Class[M] = currentMirror.runtimeClass(tt.tpe).asInstanceOf[java.lang.Class[M]]
    val mapConstructor: java.lang.reflect.Constructor[M] = mapClass.getConstructor()

    val keyTypeAdapter = context.typeAdapterOf[K]
    val valueTypeAdapter = context.typeAdapterOf[V]

    new JavaMapTypeAdapter[K, V, M](
      deserializer = new JavaMapDeserializer[K, V, M](keyTypeAdapter.deserializer, valueTypeAdapter.deserializer, () => mapConstructor.newInstance()),
      serializer   = new JavaMapSerializer[K, V, M](keyTypeAdapter.serializer, valueTypeAdapter.serializer))
  }

}

class JavaMapTypeAdapter[K, V, M <: java.util.Map[K, V]](override val deserializer: Deserializer[M], override val serializer: Serializer[M]) extends TypeAdapter[M] {

  override def read(reader: Reader): M = ???

  override def write(value: M, writer: Writer): Unit = ???

}
