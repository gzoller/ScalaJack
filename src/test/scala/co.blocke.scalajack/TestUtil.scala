package co.blocke.scalajack

object TestUtil {

  inline def colorString(str: String, color: String = Console.MAGENTA): String =
    str.split("\n").map(s => s"$color$s${Console.RESET}").mkString("\n")

  def hexStringToByteArray(s: String): Array[Byte] = {
    val len = s.length
    val data = new Array[Byte](len / 2)
    var i = 0
    while i < len
    do {
      data(i / 2) = ((Character.digit(s.charAt(i), 16) << 4) + Character.digit(
        s.charAt(i + 1),
        16
      )).toByte

      i += 2
    }
    data
  }

  // Utility to generate test code quickly
  def showException(label: String, fnStr: String, fn: () => Any) =
    try
      fn()
    catch {
      case x: IndexOutOfBoundsException => throw x
      case t: Throwable =>
        if !t.getMessage.contains("\n") then throw t
        val msg = "\"\"\"" + t.getMessage().replace("\n", "\n  |") + "\"\"\""
        println(
          label + " >> " + t.getClass.getName + "\n-----------------------\n" +
            s"val msg = $msg.stripMargin\nthe[${t.getClass.getName}] thrownBy $fnStr should have message msg\n"
        )
    }
}
