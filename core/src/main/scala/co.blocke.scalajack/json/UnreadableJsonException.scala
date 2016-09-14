package co.blocke.scalajack.json

abstract class UnreadableJsonException(cause: Throwable) extends RuntimeException(cause) {

  def write(writer: Writer): Unit

}
