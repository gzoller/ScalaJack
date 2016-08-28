package co.blocke.scalajack.flexjson.typeadapter

import co.blocke.scalajack.flexjson.{BijectiveFunction, Reader, TypeAdapter, Writer}

case class TransformedTypeAdapter[A, B](typeAdapter: TypeAdapter[A],
                                        f: BijectiveFunction[A, B]) extends TypeAdapter[B] {

  override def read(reader: Reader): B =
    f.apply(typeAdapter.read(reader))

  override def write(value: B, writer: Writer): Unit =
    typeAdapter.write(f.unapply(value), writer)

}
