package co.blocke.scalajack
package mongo

case class Foo(name: String, something: Any)
case class Bar(woo: String)

object Hello extends App {

  val sj = ScalaJack(MongoFlavor())

  //Extra: List((_hint,ExtraFieldValue(co.blocke.scalajack.mongo.Bar,co.blocke.scalajack.typeadapter.StringTypeAdapterFactory$@40844aab)))
  //Extra: List((_hint,ExtraFieldValue(co.blocke.scalajack.mongo.Bar,co.blocke.scalajack.typeadapter.StringTypeAdapterFactory$@2cc3ad05)))

  val f = Foo("Greg", Bar("hoo"))
  val d = sj.render(f)
  println(d)
  println(sj.read[Foo](d))

}
