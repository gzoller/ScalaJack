package co.blocke.scalajack

object Runme extends App {

  val sj = ScalaJack()
  type M = Map[String, Any]

  val m = Map("a" -> 2, "b" -> "1")
  val js = sj.render(m)
  println(js)
  val i = sj.read[M](js)
  println(i.map { case (k, v) => v.getClass.getName })
}
