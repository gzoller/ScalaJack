package co.blocke.scalajack

abstract class UnreadableException(cause: Throwable) extends RuntimeException(cause) {

  def write(writer: Writer): Unit

}
