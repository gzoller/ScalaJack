package co.blocke.scalajack

class UnreadableException(original: Any, cause: Throwable) extends RuntimeException(cause) {

  def write(writer: Writer): Unit = {
    writer.writeRawValue(original.toString)
  }

}

// class UnreadableException(source: Array[Char], offset: Int, length: Int, cause: Throwable) extends RuntimeException(cause) {

//   def write(writer: Writer): Unit = {
//     writer.writeRawValue(source, offset, length)
//   }

// }
