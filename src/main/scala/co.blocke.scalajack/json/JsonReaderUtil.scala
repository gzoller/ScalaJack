package co.blocke.scalajack
package json

import scala.quoted.*
import co.blocke.scala_reflection.reflect.rtypeRefs.*
import co.blocke.scala_reflection.{Clazzes, RTypeRef, TypedName}

object JsonReaderUtil:

  def classInstantiator[T: Type](ref: ClassRef[T])(using Quotes): Expr[Map[String, ?] => T] =
    import quotes.reflect.*
    val sym = TypeRepr.of[T].classSymbol.get
    '{ (fieldMap: Map[String, ?]) =>
      ${
        val tree = Apply(
          Select.unique(New(TypeIdent(sym)), "<init>"),
          ref.fields.map { f =>
            f.fieldRef.refType match
              case '[t] =>
                '{ fieldMap(${ Expr(f.name) }).asInstanceOf[t] }.asTerm
          }
        )
        tree.asExpr.asExprOf[T]
      }
    }

    /*
  def tupleInstantiator[T: Type](ref: TupleRef[T])(using Quotes): Expr[List[?] => T] =
    import quotes.reflect.*
    val sym = TypeRepr.of[T].classSymbol.get
    '{ (untyped: List[?]) =>
      ${
        val tree = Apply(
          // Must correctly type the tuple, both the class type params and the individual values
          TypeApply(
            Select.unique(New(TypeIdent(sym)), "<init>"),
            ref.tupleRefs.map { r =>
              r.refType match
                case '[s] =>
                  TypeTree.of[s]
            }
          ),
          ref.tupleRefs.zipWithIndex.map { (f, j) =>
            f.refType match
              case '[t] =>
                val jExpr = Expr(j)
                '{ untyped($jExpr).asInstanceOf[t] }.asTerm
          }
        )
        tree.asExpr.asExprOf[T]
      }
    }
     */

  // def classParseMap[T: Type](ref: ClassRef[T], root: ReaderModule)(using q: Quotes)(using
  //     cache: scala.collection.mutable.HashMap[Expr[TypedName], Expr[(JsonConfig, JsonParser) => Either[ParseError, ?]]]
  // ): Expr[JsonParser => Map[String, (JsonConfig, JsonParser) => Either[ParseError, ?]]] =
  //   import Clazzes.*
  //   '{ (parser: JsonParser) =>
  //     val daList = ${
  //       val fieldList = ref.fields.map(f =>
  //         f.fieldRef.refType match
  //           case '[m] =>
  //             val fn = root.readerFn[m](f.fieldRef.asInstanceOf[RTypeRef[m]])
  //             '{
  //               ${ Expr(f.name) } -> $fn
  //             }
  //       )
  //       Expr.ofList(fieldList)
  //     }
  //     daList.toMap
  //   }
