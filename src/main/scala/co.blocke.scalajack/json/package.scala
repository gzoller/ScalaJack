package co.blocke.scalajack
package json

import scala.util.Failure
import scala.quoted.{Expr, Quotes, Type}
import java.util.Optional
import co.blocke.scala_reflection.reflect.rtypeRefs.*
import co.blocke.scala_reflection.RTypeRef

val BUFFER_EXCEEDED: Char = 7 // Old "BELL" ASCII value, used as a marker when we've run off the end of the known world
val END_OF_STRING: Char = 3

def ofOption[T](xs: Option[Expr[T]])(using Type[T])(using q: Quotes): Expr[Option[T]] =
  import q.reflect.*
  if xs.isEmpty then '{ None }
  else '{ Some(${ xs.get }) }

// Java variant of ofOption
def ofOptional[T](xs: Optional[Expr[T]])(using Type[T])(using q: Quotes): Expr[Optional[T]] =
  import q.reflect.*
  if xs.isEmpty then '{ Optional.empty }
  else '{ Optional.of(${ xs.get }) }

def liftRTypeRefMap(map: Map[String, List[RTypeRef[?]]])(using quotes: Quotes): Expr[Map[String, List[RTypeRef[?]]]] =
  import quotes.reflect.*
  val entries: List[Expr[(String, List[RTypeRef[?]])]] =
    map.toList.map { case (key, valueList) =>
      val keyExpr = Expr(key)
      val valueExpr = Expr.ofList(valueList.map(_.expr.asExprOf[RTypeRef[?]]))
      '{ $keyExpr -> $valueExpr }
    }
  val mapExpr = Expr.ofList(entries)
  '{ Map.from($mapExpr) }
