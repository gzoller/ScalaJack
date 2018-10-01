package co.blocke.scalajack

case class Bar(size: Int, ok: String)
case class Foo[T](m: T)
case class TT(t: (String, String))

object RunMe extends App {
  /*

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
  */

  trait XFoo { val a: Int }
  trait XBar { val b: Int; val c: XFoo }
  case class One(a: Int) extends XFoo
  case class Two(b: Int, c: XFoo) extends XBar

  //  sealed trait Answer
  //  case object Yes extends Answer
  //  case object No extends Answer
  //  case class Thingy(a: Answer, i: Int)

  //  case class Yes(a: String = "Wow") extends Answer
  //  case class No(a: String = "Nope") extends Answer

  val sj = ScalaJack().withDefaultHint("kind").withHints((typeOf[XFoo] -> "mark"), (typeOf[XBar] -> "greg"))
  val inst: XBar = Two(3, One(2))
  try {
    //    val js = sj.render(Thingy(Yes, 5))
    //    println(js)
    //    val obj = sj.read[Thingy](js)
    //    println(obj)

    val j = sj.render(inst)
    println(j)
    val k = sj.read[XBar](j)
    println(k)

  } catch {
    case t: Throwable => println("Boom: " + t)
  }

  import org.apache.commons.text.StringEscapeUtils.escapeJava

  val s = "something\b\n\f\r\t☆"
  println(escapeJava(s))
}

