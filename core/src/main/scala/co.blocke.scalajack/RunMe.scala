package co.blocke.scalajack

case class Bar(size: Int, ok: String)
case class Foo[T](m: T)
case class TT(t: (String, String))

object RunMe extends App {
  val sj = ScalaJack() //.isCanonical(false)

  def show[A](f: Foo[A])(implicit tt: TypeTag[A]): Unit = {
    println(f)
    val js = sj.render(f)
    println(js)
    println(sj.read[Foo[A]](js))
  }

  println("------------ 1 ------------")
  show(Foo(Map(Bar(1, "Mike") -> 5, Bar(2, "Fred") -> 3)))

  println("\n\n------------ 2 ------------")
  show(Foo(Map(List(1, 2, 3) -> 5, List(4, 5, 6) -> 3)))

  println("\n\n------------ 3 ------------")
  show(Foo(Map(true -> 5, false -> 3)))

  println("\n\n------------ 4 ------------")
  show(Foo(Map(33 -> 5, 44 -> 3)))

  println("\n\n------------ 5 ------------")
  show(Foo(Map(("hey", "jude") -> 5, ("purple", "rain") -> 3)))

  //  val x = sj.parse("""{"a":1,"b":2}""")
  //  val x = sj.parse("[1,2,3]")
  val x = sj.parse(sj.render(Foo(Map(("hey", "jude") -> 5, ("purple", "rain") -> 3))))
  println("\n" + x)
  println("\n" + sj.render(x))

}
