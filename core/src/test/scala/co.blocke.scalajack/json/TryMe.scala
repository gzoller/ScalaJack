package co.blocke.scalaJack.json

import co.blocke.scalajack._

object TryMe {

  def boom[T](js: String, sj: ScalaJackLike[String, _], fn: () => _)(implicit tt: TypeTag[T]): String = {
    try {
      sj.read[T](js)
      fn()
    } catch {
      case t: Throwable =>
        println(t.getMessage())
        "Bad"
    }
    "OK"
  }

}
