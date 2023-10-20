package co.blocke.scalajack
package run

import co.blocke.scala_reflection.*

object RunMe extends App:

  val p = Person("Greg", 57, List(false, true, true), Colors.Blue, "Fred".asInstanceOf[BigName])

  val d = Dog("Fido", 4, 2, Some(Dog("Mindy", 4, 0, None)))
  val d2 = Dog("Spot", 4, 3, Some(Dog("Floppy", 3, 1, None)))

  val mapper = (a: Any) =>
    a.getClass.getPackage.getName match
      case p if p.startsWith("co.blocke") => "Blocke"
      case x                              => "Something " + x.getClass.getName

  given json.JsonConfig = json
    .JsonConfig()
    .copy(
      typeHintDefaultTransformer = (s: String) => s.split("\\.").last,
      typeHintLabelByTrait = Map("co.blocke.scalajack.run.Animal" -> "kind"),
      typeHintTransformer = Map("co.blocke.scalajack.run.Dog" -> mapper)
    )

  println(ScalaJack.write(d))

  val t0 = System.currentTimeMillis()
  for i <- 0 to 10000 do
    ScalaJack.write(d)
    // if i % 100 == 0 then println(i)
    // if i == 10000 then println(i)

  // println(Codec.write(d)(using cfg))
  // println("")
  // println(Codec.write(d2)(using cfg))
  // println("")
  // println(Codec.write(d)(using cfg))

  val t1 = System.currentTimeMillis()
  println("TIME: " + (t1 - t0))
