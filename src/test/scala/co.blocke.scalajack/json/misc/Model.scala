package co.blocke.scalajack
package json
package misc

import java.util.Optional
import scala.util.*

case class Person(name: String, age: Int)

case class OptionHolder[T](
    a: Option[T], // straight Option
    b: (Option[T], String), // tuple w/Option
    c: List[Option[T]], // Seq of Option
    d: Map[Int, Option[T]], // Map of Option
    e: T | Option[T], // Union of Option (R)
    f: Option[T] | T, // Union of Option (L)
    g: Option[Option[T]], // Nested Option
    h: Option[Person], // Option of Class
    i: Either[T, Option[T]], // Either of Option (R)
    j: Either[Option[T], T] // Either of Option (L)
)

case class TryHolder[T](a: Try[T])
case class TryHolder2[T](a: Seq[Try[T]], b: (Try[T], Try[T]))

case class LRHolder[T, U](a: Seq[T | U], b: (T | U, T | U))
case class EitherHolder[T](a: Either[T, String], b: Either[String, T])

case class ComplexEither[T](a: Option[Either[String, Option[T]]])

case class AliasHolder[T](a: T, b: List[T], c: Map[T, String], d: Map[String, T])
case class AliasHolder2[T](a: T, b: List[T], c: Map[String, T])

case class StringHolder(a: String)
