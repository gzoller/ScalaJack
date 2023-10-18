package co.blocke.scalajack
package run

import co.blocke.scala_reflection.*

object RunMe extends App:

  // val p = Person("Greg", 57, List(false, true, true), Colors.Blue, "Fred".asInstanceOf[BigName])

  // println(Codec.write(p)(using json.JsonConfig(tryFailureHandling = json.TryOption.ERR_MSG_STRING)))

  val animal: Dog = Dog("fido", 4, 2, Some(Dog("Mindy", 4, 0, None)))
  println(Codec.write(animal)(using json.JsonConfig()))

  // println(RType.of[Person].pretty)

// println(RType.of[Person].pretty)
/*


val SJConfig = .... (JSON or Binary fmt specified in config)

val sj = ScalaJack(using SJConfig)

sj.write(person)
sj.read[Person](js): Either[Error,Person]


JsonFlavor:

def write[T](t: T): String = ${ writeImpl[T]('t) }

def writeImpl[T:Type](t: Expr[T])(using quotes: Quotes): Expr[String] =
    import quotes.reflect.*


 */
