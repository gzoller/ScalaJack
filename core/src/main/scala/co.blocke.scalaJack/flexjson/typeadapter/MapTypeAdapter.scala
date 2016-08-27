package co.blocke.scalajack.flexjson.typeadapter

import co.blocke.scalajack.flexjson.{Context, Reader, TypeAdapter, TypeAdapterFactory, Writer}

import scala.reflect.runtime.universe.{Type, typeOf}

object MapTypeAdapter extends TypeAdapterFactory {

  override def typeAdapter(tpe: Type, context: Context): Option[TypeAdapter[_]] =
    if (tpe <:< typeOf[Map[_, _]]) {
      val keyType = tpe.typeArgs(0)
      val keyTypeAdapter = context.typeAdapter(keyType)

      val valueType = tpe.typeArgs(1)
      val valueTypeAdapter = context.typeAdapter(valueType)

      Some(MapTypeAdapter(keyTypeAdapter, valueTypeAdapter))
    } else {
      None
    }

}

case class MapTypeAdapter[K, V](keyTypeAdapter: TypeAdapter[K],
                                valueTypeAdapter: TypeAdapter[V]) extends TypeAdapter[Map[K, V]] {

  override def read(reader: Reader): Map[K, V] = {
    val builder = Map.canBuildFrom[K, V]()

    reader.beginObject()

    while (reader.hasMoreFields) {
      val key = keyTypeAdapter.read(reader)
      val value = valueTypeAdapter.read(reader)

      builder += key -> value
    }

    reader.endObject()

    builder.result()
  }

  override def write(value: Map[K, V], writer: Writer): Unit = ???

}