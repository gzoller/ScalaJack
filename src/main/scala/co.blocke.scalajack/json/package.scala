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

def liftStringMap(map: Map[String, String])(using quotes: Quotes): Expr[Map[String, String]] = {
  import quotes.reflect.*

  val entries: List[Expr[(String, String)]] = map.toList
    .map { case (k, v) =>
      Expr(k) -> Expr(v)
    }
    .map { case (kExpr, vExpr) =>
      '{ $kExpr -> $vExpr }
    }
  val liftedList = Expr.ofList(entries)
  '{ Map.from($liftedList) }
}

def liftStringListMap(map: Map[String, List[String]])(using quotes: Quotes): Expr[Map[String, List[String]]] =
  import quotes.reflect.*

  val entries: List[Expr[(String, List[String])]] = map.toList.map { case (k, v) =>
    val keyExpr = Expr(k)
    val valueExpr = Expr.ofList(v.map(Expr(_)))
    '{ $keyExpr -> $valueExpr }
  }
  val listExpr: Expr[List[(String, List[String])]] = Expr.ofList(entries)
  '{ Map.from($listExpr) }

def liftStringOptionMap(map: Map[String, Option[String]])(using quotes: Quotes): Expr[Map[String, Option[String]]] =
  import quotes.reflect.*

  val entries: List[Expr[(String, Option[String])]] = map.toList.map {
    case (key, Some(value)) => '{ (${ Expr(key) }, Some(${ Expr(value) })) }
    case (key, None)        => '{ (${ Expr(key) }, None) }
  }
  val listExpr: Expr[List[(String, Option[String])]] = Expr.ofList(entries)
  '{ Map.apply[String, Option[String]]($listExpr*) }

def allUniqueFields(fieldHashMap: Map[String, List[String]]): Option[Map[String, String]] =
  if fieldHashMap.forall(_._2.size == 1) then Some(fieldHashMap.view.mapValues(_.head).toMap)
  else None

// Support annotation @Change to change field names
private inline def changeFieldName(fr: FieldInfoRef): String = fr.annotations.get("co.blocke.scalajack.Change").flatMap(_.get("name")).getOrElse(fr.name)
