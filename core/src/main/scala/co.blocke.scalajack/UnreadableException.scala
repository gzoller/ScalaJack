package co.blocke.scalajack

class UnreadableException(source: Array[Char], offset: Int, length: Int, cause: Throwable) extends RuntimeException(cause) {

  def write(writer: Writer): Unit = {
    writer.writeRawValue(source, offset, length)
  }

}
