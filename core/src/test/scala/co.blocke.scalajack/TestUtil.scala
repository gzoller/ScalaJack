package co.blocke.scalajack

object TestUtil {

  def hexStringToByteArray(s: String): Array[Byte] = {
    val len = s.length
    val data = new Array[Byte](len / 2)
    var i = 0
    while ({
      i < len
    }) {
      data(i / 2) = ((Character.digit(s.charAt(i), 16) << 4) + Character.digit(s.charAt(i + 1), 16)).toByte

      i += 2
    }
    data
  }

  // Utility to generate test code quickly
  def showException(label: String, fnStr: String, fn: () => Any) =
    try {
      fn()
    } catch {
      case x: IndexOutOfBoundsException => throw x
      case t: Throwable =>
        if (!t.getMessage.contains("\n"))
          throw t
        val msg = "\"\"\"" + t.getMessage().replace("\n", "\n  |") + "\"\"\""
        println(label + " >> " + t.getClass.getName + "\n-----------------------\n" +
          s"val msg = $msg.stripMargin\nthe[${t.getClass.getName}] thrownBy $fnStr should have message msg\n")
    }
}

object SpotTest extends org.scalatest.Tag("SpotTest")