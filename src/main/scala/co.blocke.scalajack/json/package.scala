package co.blocke.scalajack
package json

import scala.util.Failure
import scala.quoted.{Expr, Quotes, Type}

opaque type RawJson = String

val BUFFER_EXCEEDED: Char = 7 // Old "BELL" ASCII value, used as a marker when we've run off the end of the known world
val END_OF_STRING: Char = 3

inline def lastPart(n: String) = n.split('.').last.stripSuffix("$")

val random = new scala.util.Random()
def scramble(hash: Int): String =
  val last5 = f"$hash%05d".takeRight(5)
  val digits = (1 to 5).map(_ => random.nextInt(10))
  if digits(0) % 2 == 0 then s"${last5(0)}${digits(0)}${last5(1)}${digits(1)}${last5(2)}-${digits(2)}${last5(3)}${digits(3)}-${last5(4)}${digits(4)}A"
  else s"${digits(0)}${last5(0)}${digits(1)}${last5(1)}${digits(2)}-${last5(2)}${digits(3)}${last5(3)}-${digits(4)}${last5(4)}B"

def descrambleTest(in: String, hash: Int): Boolean =
  val last5 = f"$hash%05d".takeRight(5)
  in.last match
    case 'A' if in.length == 13 => "" + in(0) + in(2) + in(4) + in(7) + in(10) == last5
    case 'B' if in.length == 13 => "" + in(1) + in(3) + in(6) + in(8) + in(11) == last5
    case _                      => false

def ofOption[T](xs: Option[Expr[T]])(using Type[T])(using q: Quotes): Expr[Option[T]] =
  import q.reflect.*
  if xs.isEmpty then Expr(None) else '{ Some(${ xs.get }) }
