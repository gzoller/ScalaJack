package co.blocke.scalajack.flexjson

trait TypeAdapter[T] {

  def read(reader: Reader): T

  def write(value: T, writer: Writer): Unit

}
