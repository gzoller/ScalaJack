package co.blocke.scalajack

import scala.quoted.{Expr, Quotes, Type}
import co.blocke.scala_reflection.reflect.rtypeRefs.*
import co.blocke.scala_reflection.RTypeRef
import java.util.Optional

inline def lastPart(n: String) = n.split('.').last.stripSuffix("$")
inline def allButLastPart(n: String) =
  val l = n.lastIndexOf('.')
  if l >= 0 then n.substring(0, l)
  else n

val random = new scala.util.Random()
def scramble(hash: Int): String =
  val last5 = f"$hash%05d".takeRight(5)
  val digits = (1 to 5).map(_ => random.nextInt(10))
  if digits(0) % 2 == 0 then s"${last5(0)}${digits(0)}${last5(1)}${digits(1)}${last5(2)}-${digits(2)}${last5(3)}${digits(3)}-${last5(4)}${digits(4)}A"
  else s"${digits(0)}${last5(0)}${digits(1)}${last5(1)}${digits(2)}-${last5(2)}${digits(3)}${last5(3)}-${digits(4)}${last5(4)}B"

def descramble(in: String, hash: Int): Boolean =
  val last5 = f"$hash%05d".takeRight(5)
  in.last match
    case 'A' if in.length == 13 => "" + in(0) + in(2) + in(4) + in(7) + in(10) == last5
    case 'B' if in.length == 13 => "" + in(1) + in(3) + in(6) + in(8) + in(11) == last5
    case _                      => false

import scala.annotation.*
import scala.quoted.{Expr, Quotes}
case class jsLabel(name: String) extends StaticAnnotation

// XML Support
case class xmlLabel(name: String) extends StaticAnnotation // rename a field
case class xmlEntryLabel(name: String) extends StaticAnnotation // label for Map entries
case class xmlAsAttribute() extends StaticAnnotation
case class xmlStruct() extends StaticAnnotation // remove field wrapper -- Don't use this! Horrible, invalid XML!

def liftStringOptionMap(map: Map[String, Option[String]])(using quotes: Quotes): Expr[Map[String, Option[String]]] =
  val entries: List[Expr[(String, Option[String])]] = map.toList.map {
    case (key, Some(value)) => '{ (${ Expr(key) }, Some(${ Expr(value) })) }
    case (key, None)        => '{ (${ Expr(key) }, None) }
  }
  val listExpr: Expr[List[(String, Option[String])]] = Expr.ofList(entries)
  '{ Map.apply[String, Option[String]]($listExpr*) }

def ofOption[T](xs: Option[Expr[T]])(using Type[T])(using q: Quotes): Expr[Option[T]] =
  if xs.isEmpty then '{ None }
  else '{ Some(${ xs.get }) }

// Java variant of ofOption
def ofOptional[T](xs: Optional[Expr[T]])(using Type[T])(using q: Quotes): Expr[Optional[T]] =
  if xs.isPresent then '{ Optional.of(${ xs.get }) }
  else '{ Optional.empty }

def testValidMapKey(testRef: RTypeRef[?]): Boolean =
  val isValid = testRef match
    case _: PrimitiveRef                       => true
    case _: TimeRef                            => true
    case _: NetRef                             => true
    case c: ScalaClassRef[?] if c.isValueClass => true
    case _: EnumRef[?]                         => true
    case a: AliasRef[?]                        => testValidMapKey(a.unwrappedType)
    case t: TraitRef[?] if t.childrenAreObject => true
    case _                                     => false
  if !isValid then throw new TypeError(s"For JSON or XML serialization, map keys must be a simple type. ${testRef.name} is too complex.")
  isValid

trait JsonDefault[T]:
  def default: T
