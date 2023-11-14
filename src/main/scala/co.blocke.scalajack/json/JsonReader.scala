package co.blocke.scalajack
package json

import co.blocke.scala_reflection.{RTypeRef, TypedName}
import co.blocke.scala_reflection.reflect.rtypeRefs.*
import parser.*
import scala.quoted.*
import scala.collection.mutable.HashMap
import scala.util.{Failure, Success, Try}
import scala.collection.Factory

object JsonReader:

  def refRead[T](
      ref: RTypeRef[T]
  )(using q: Quotes, tt: Type[T]): Expr[Instruction] =
    import quotes.reflect.*

    ref match
      case r: PrimitiveRef[?] if r.family == PrimFamily.Boolish   => '{ BooleanInstruction() }
      case r: PrimitiveRef[?] if r.family == PrimFamily.Stringish => '{ StringInstruction() }
      case r: PrimitiveRef[?] if r.family == PrimFamily.Longish   => '{ IntInstruction() }

      case r: SeqRef[?] =>
        r.elementRef.refType match
          case '[e] =>
            r.refType match
              case '[t] =>
                val elementInstruction = refRead[e](r.elementRef.asInstanceOf[RTypeRef[e]])
                '{ SeqInstruction[e, t]($elementInstruction)(using ${ Expr.summon[Factory[e, t]].get }) }

      case r: ScalaClassRef[?] =>
        r.refType match
          case '[t] =>
            val fieldInstructions = Expr.ofList(
              r.fields
                .map { f =>
                  f.fieldRef.refType match
                    case '[e] =>
                      (Expr(f.name), refRead[e](f.fieldRef.asInstanceOf[RTypeRef[e]]))
                }
                .map(u => Expr.ofTuple(u))
            )
            val instantiator = JsonReaderUtil.classInstantiator[t](r.asInstanceOf[ClassRef[t]])
            '{ ScalaClassInstruction[t]($fieldInstructions.toMap, $instantiator) }
