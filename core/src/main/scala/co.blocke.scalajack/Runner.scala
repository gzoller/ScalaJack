package co.blocke.scalajack

import co.blocke.scalajack.typeadapter.classes.PlainClassTypeAdapter
import model._

@Collection(name = "plains")
class Plain() {
  @DBKey(index = 1) var name: String = "" // public var member
  var age: Option[Int] = Some(19)
}

object Runner extends App {

  val sj = ScalaJack()
  val adapter = sj.context.typeAdapter(typeOf[Plain]).asInstanceOf[ClassHelper.ClassLikeTypeAdapter[_]]
  println(">> " + adapter.dbKeys)

  //  val js = """{"e1":"Bogus","e2":null}"""
  //  sj.read[SampleEnum3](js)
}
