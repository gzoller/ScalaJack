package co.blocke.scalajack

object RunMe extends App:

  val cfg = WriterConfig
    .withEscapeUnicode(false)
    .withIndentionStep(0)
    .withPreferredBufSize(32768)  // 2^15
    .withThrowWriterExceptionWithStackTrace(true)

  val writer = JsonWriter(new Array[Byte](256), 0, 0, 0, false, false, null, null, cfg)

  writer.writeArrayStart()
  (0 to 300).map(writer.writeVal(_))
  writer.writeArrayEnd()

  println("Result: "+writer.result)
  writer.reset
  writer.writeArrayStart()
  writer.writeVal("Greogry")
  writer.writeVal("William")
  writer.writeVal("Zoller")
  writer.writeArrayEnd()
  println("Name: "+writer.result)

  println("Done.")

