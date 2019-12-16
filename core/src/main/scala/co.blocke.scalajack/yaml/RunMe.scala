package co.blocke.scalajack
package yaml

case class Person(name: String, age: Int)

object RunMe extends App {

  val sj = ScalaJack()

  val writer = YamlWriter()

  val stuff = List("foo", "Greg Zoller")

  val multi = List(List("a", "b"), List("c", "d"))

  val multiM = List(Map("one" -> 1, "two" -> 2), Map("three" -> 3, "four" -> 4))
  val abc = Map("One" -> List(1, 2, 3), "Two" -> List(4, 5, 6))

  val foo =
    Map(
      List("a\nGreg\nZoller", "b") -> List(1, 2),
      List("c", "d") -> List(3, 4)
    )

  val out = model.StringBuilder()

  val stringTA = sj.taCache.typeAdapterOf[String]
  val ta1 = sj.taCache.typeAdapterOf[List[String]]
  //  val ta2 = sj.taCache.typeAdapterOf[List[Int]]
  //  val ta2 = sj.taCache.typeAdapterOf[List[Map[String, String]]]
  val ta2 = sj.taCache.typeAdapterOf[Person]

  //  writer.writeMap(foo, ta1, ta2, out)
  //  writer.writeMap(Map("foo\nbar" -> "bar\nbaz"), stringTA, stringTA, out)
  //  writer.writeMap(Map("foobar" -> "bar\nbaz"), stringTA, stringTA, out)

  writer.writeObject(
    Person("Greg", 53),
    ta2.asInstanceOf[typeadapter.CaseClassTypeAdapter[_]].orderedFieldNames,
    ta2
      .asInstanceOf[typeadapter.CaseClassTypeAdapter[_]]
      .fieldMembersByName
      .asInstanceOf[Map[String, model.ClassHelper.ClassFieldMember[Any, Any]]],
    out,
    Nil
  )

  /*
  writer.writeMap(
    Map(
      "foobar" -> List(
        Map("a" -> "b"),
        Map("x" -> "y", "Greg\nZoller" -> "blah\nblather"),
        Map("c" -> "one")
      )
    ),
    stringTA,
    ta2,
    out
  )
   */

  println(out.result())
}
