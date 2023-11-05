package co.blocke.scalajack
package run

import co.blocke.scala_reflection.*
import scala.jdk.CollectionConverters.*

enum Color:
  case Red, Green, Blue

object RunMe extends App:

  /*
  val f = () => parser.expectLong
  val f2 = () => parser.expectList[Long](() => parser.expectLong)
  val r = parser.expectList[List[Long]](f2)

  println("R: " + r)
   */

  given json.JsonConfig = json
    .JsonConfig()
    .copy(noneAsNull = true)
  // .copy(enumsAsIds = '*')

  try
    val v =
      Person(
        "Greg",
        Some(
          Person(
            "Lili",
            Some(
              Person(
                "Katie",
                None
              )
            )
          )
        )
      )

    println("HERE: " + ScalaJack.write[Person[Boolean]](v.asInstanceOf[Person[Boolean]]))

    // println("HERE: " + ScalaJack.write(Person("Greg", Foom('z'), Some(Person("Lili", Foom('x'), None)))))

    // val now = System.currentTimeMillis()
    // for i <- 1 to 10000000 do ScalaJack.write(List(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9)))
    // val now2 = System.currentTimeMillis()
    // println("Macro-based: " + (now2 - now))

    // val person = Person("David", 16)
    // val minor = MinorPerson.make(person) match
    //   case Right(r) => r
    //   case _        => throw new Exception("boom")

    // val p = SampleNeo(NonEmptyString("Greg"), "MVP", minor)
    // val js = ScalaJack.write(x)
    // println(js)

    /*
    val x = Blah("foo", WeekDay.Fri)
    val js = ScalaJack.write(x)
    println(js)

    val inst = ScalaJack.read[Blah](js)
    println(inst)
     */

  catch {
    case t: Throwable =>
      println(s"BOOM ($t): " + t.getMessage)
      t.printStackTrace
  }
