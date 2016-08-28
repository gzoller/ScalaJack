package co.blocke.scalajack.flexjson

import co.blocke.scalajack.flexjson.typeadapter.TransformedTypeAdapter

trait TypeAdapter[T] {

  def read(reader: Reader): T

  def write(value: T, writer: Writer): Unit

  def transform[U](f: BijectiveFunction[T, U]): TransformedTypeAdapter[T, U] =
    TransformedTypeAdapter(this, f)

}
