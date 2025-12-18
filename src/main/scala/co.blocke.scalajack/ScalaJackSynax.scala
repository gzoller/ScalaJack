package co.blocke.scalajack

object ScalaJackSyntax:

  // encode a value
  extension [T](a: T)(using sj: ScalaJack[T])
    def toJson: String =
      sj.toJson(a)

  // encode a list
  extension [T](xs: List[T])(using sj: ScalaJack[T])
    def toJson: String =
      sj.toJsonList(xs)

  // decode a string
  extension (js: String)
    def fromJson[T](using sj: ScalaJack[T]): T =
      sj.fromJson(js)

    def fromJsonList[T](using sj: ScalaJack[T]): List[T] =
      sj.fromJsonList(js)