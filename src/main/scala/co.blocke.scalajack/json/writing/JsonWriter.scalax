package co.blocke.scalajack
package json
package writing

import co.blocke.scala_reflection.{RTypeRef, TypedName}
import co.blocke.scala_reflection.reflect.rtypeRefs.*
import co.blocke.scala_reflection.rtypes.*
import scala.quoted.*
import scala.collection.mutable.Map as MMap
import scala.util.Failure

object JsonWriter:

  private def shouldTestForOkToWrite(r: RTypeRef[?]): Boolean =
    r match
      case _: OptionRef[?]    => true
      case _: LeftRightRef[?] => true
      case _: TryRef[?]       => true
      case _                  => false

  // Tests whether we should write something or not--mainly in the case of Option, or wrapped Option
  // Affected types: Option, java.util.Optional, Left/Right, Try/Failure
  private def isOkToWrite(a: Any, cfg: JsonConfig) =
    a match
      case None if !cfg.noneAsNull                                    => false
      case o: java.util.Optional[?] if o.isEmpty && !cfg.noneAsNull   => false
      case Left(None) if !cfg.noneAsNull                              => false
      case Right(None) if !cfg.noneAsNull                             => false
      case Failure(_) if cfg.tryFailureHandling == TryOption.NO_WRITE => false
      case _                                                          => true

  def refRead[T](
      ref: RTypeRef[T]
  )(using q: Quotes, tt: Type[T]): Expr[(T, JsonOutput, JsonConfig) => String] =
    import quotes.reflect.*
    '{ (a: T, out: JsonOutput, cfg: JsonConfig) =>
      ${ refWrite(ref, '{ a }, '{ out }, '{ cfg })(using MMap.empty[TypedName, RTypeRef[?]]) }.result
    }

  private def refWrite[T](
      ref: RTypeRef[T],
      aE: Expr[T],
      outE: Expr[JsonOutput],
      cfgE: Expr[JsonConfig],
      isMapKey: Boolean = false
  )(using classesSeen: MMap[TypedName, RTypeRef[?]])(using q: Quotes, tt: Type[T]): Expr[JsonOutput] =
    import quotes.reflect.*

    ref match
      case t: BooleanRef => '{ $outE.value($aE) }
      case t: IntRef     => '{ $outE.value($aE) }
      case t: StringRef  => '{ $outE.value($aE) }

      case t: SeqRef[?] =>
        if isMapKey then throw new JsonError("Seq instances cannot be map keys")
        t.elementRef.refType match
          case '[e] =>
            val bE = aE.asInstanceOf[Expr[Seq[e]]] // pull type-cast into complie-time for performance gains
            '{
              if $aE == null then $outE.burpNull()
              else
                $outE.startArray()
                $bE.foreach { one =>
                  ${
                    if shouldTestForOkToWrite(t.elementRef) then // A lot of drama to avoid 1 'if' stmt--it matters for max performance
                      '{
                        if isOkToWrite(one, $cfgE) then ${ refWrite[e](t.elementRef.asInstanceOf[RTypeRef[e]], '{ one }, outE, cfgE) }
                      }
                    else refWrite[e](t.elementRef.asInstanceOf[RTypeRef[e]], '{ one }, outE, cfgE)
                  }
                }
                $outE.endArray()
            }

      case t: ScalaClassRef[?] =>
        classesSeen.put(t.typedName, t)
        val isCase = Expr(t.isCaseClass)
        '{
          // Experiment (works!) to generate def fn(in:T, sb: JsonOut).  The goal is to pre-calc all the ugly stuff so that the
          // macro generated is as pure/fast as possible.
          //
          // def foo(in: T): Unit =
          //   ${
          //     Expr.ofList(t.fields.map { f =>
          //       '{ println("Field: " + ${ Select.unique('in.asTerm, f.name).asExprOf[Any] }) }
          //     })
          //   }
          if $aE == null then $outE.burpNull()
          else
            $outE.startObject()
            ${
              Expr.ofList(t.fields.map { f =>
                f.fieldRef.refType match
                  case '[e] =>
                    val fieldValue = Select.unique(aE.asTerm, f.name).asExprOf[e]
                    val name = Expr(f.name)
                    if shouldTestForOkToWrite(f.fieldRef) then // A lot of drama to avoid 1 'if' stmt--it matters for max performance
                      '{
                        if isOkToWrite($fieldValue, $cfgE) then
                          $outE.label($name)
                          ${ refWrite[e](f.fieldRef.asInstanceOf[RTypeRef[e]], fieldValue, outE, cfgE) }
                      }
                    else
                      '{
                        $outE.label($name)
                        ${ refWrite[e](f.fieldRef.asInstanceOf[RTypeRef[e]], fieldValue, outE, cfgE) }
                      }
              })
            }
            $outE.endObject()
        }
