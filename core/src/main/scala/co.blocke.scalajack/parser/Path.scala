package co.blocke.scalajack
package parser

trait Path {

  protected[parser] def parserHello( parser: Parser ): Unit

  // Parser callbacks
  protected[parser] def pushBoolean(b: Boolean): Unit    = throw new UnexpectedException()
  protected[parser] def pushDecimal(d: BigDecimal): Unit = throw new UnexpectedException()
  protected[parser] def pushInt(i: BigInt): Unit         = throw new UnexpectedException()
  protected[parser] def pushNull(): Unit                 = throw new UnexpectedException()
  protected[parser] def pushString(s: String): Unit      = throw new UnexpectedException()

  protected[parser] def pushValue(a: Any): Unit          = throw new UnexpectedException()
  protected[parser] def endSeries(): Unit = {}
}

case class IllegalPath() extends Path {
  protected[parser] def parserHello( parser: Parser): Unit = {}
}

case class UnexpectedException(msg:String) extends Exception(msg)

