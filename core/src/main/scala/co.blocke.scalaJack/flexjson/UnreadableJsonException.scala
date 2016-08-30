package co.blocke.scalajack.flexjson

abstract class UnreadableJsonException(cause: Throwable) extends RuntimeException(cause) {

  def write(writer: Writer): Unit

}
