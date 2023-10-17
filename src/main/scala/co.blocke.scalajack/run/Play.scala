package co.blocke.scalajack
package run

import co.blocke.scala_reflection.* 

object RunMe extends App:

    val p = Person("Greg", 57, List(false,true,true))

    val i: Array[Int] = Array(1,2,3)

    println(Codec.write(p))

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