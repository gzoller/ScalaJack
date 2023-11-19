package co.blocke.scalajack
package json
package writing

import co.blocke.scala_reflection.{RTypeRef, TypedName}
import co.blocke.scala_reflection.reflect.rtypeRefs.*
import scala.quoted.*
import scala.collection.mutable.{Map=>MMap}

object JW:

  private def shouldTestForOkToWrite( r: RTypeRef[_] ): Boolean =
    r match 
      case _: OptionRef[?]    => true
      case _: LeftRightRef[?] => true
      case _: TryRef[?]       => true
      case _                  => false

  def refRead[T](
      ref: RTypeRef[T]
  )(using q: Quotes, tt: Type[T]): Expr[(T, StringBuilder, JsonConfig)=>String] = 
    import quotes.reflect.*
    '{ 
      (a: T, sb: StringBuilder, cfg: JsonConfig) => ${refWrite(ref, '{a}, '{sb}, '{cfg})(using MMap.empty[TypedName,RTypeRef[?]])}.toString
    }

  private def refWrite[T](
    ref: RTypeRef[T], 
    aE: Expr[T], 
    sbE: Expr[StringBuilder], 
    cfgE: Expr[JsonConfig], 
    isMapKey: Boolean = false
  )(using classesSeen: MMap[TypedName, RTypeRef[?]])(using q: Quotes, tt: Type[T]) : Expr[StringBuilder] = 
    import quotes.reflect.*
    
    ref match
      case t: PrimitiveRef[?] if t.family == PrimFamily.Stringish => 
        '{ if $aE == null then $sbE.append("null") else $sbE.append(s"\"${$aE}\"") }

      case t: PrimitiveRef[?] =>
        val isNullable = Expr(t.isNullable)
        if isMapKey then
          '{
            if $isNullable && $aE == null then $sbE.append("\"null\"")
            else $sbE.append(s"\"${$aE.toString}\"")
          }
        else '{ if $isNullable && $aE == null then $sbE.append("null") else $sbE.append($aE.toString) }

      case t: SeqRef[?] =>
        if isMapKey then throw new JsonError("Seq instances cannot be map keys")
        t.elementRef.refType match
          case '[e] =>
            val bE = aE.asInstanceOf[Expr[Seq[e]]]  // pull type-cast into complie-time for performance gains
            '{
              if $aE == null then $sbE.append("null")
              else
                $sbE.append('[')
                val sbLen = $sbE.length()
                
                $bE.foreach { one =>
                  ${
                    if shouldTestForOkToWrite(t.elementRef) then // A lot of drama to avoid 1 'if' stmt--it matters for max performance
                      '{
                        if isOkToWrite(one, $cfgE) then
                          ${ refWrite[e](t.elementRef.asInstanceOf[RTypeRef[e]], '{ one }, sbE, cfgE) }
                          $sbE.append(',')
                      }
                    else
                      '{
                      ${ refWrite[e](t.elementRef.asInstanceOf[RTypeRef[e]], '{ one }, sbE, cfgE) }
                      $sbE.append(',')
                      }
                  }
                }
                if sbLen == $sbE.length() then $sbE.append(']')
                else $sbE.setCharAt($sbE.length() - 1, ']')
            }

      case t: ScalaClassRef[?] =>
        classesSeen.put(t.typedName, t)
        val isCase = Expr(t.isCaseClass)
        '{
          if $aE == null then $sbE.append("null")
          else
            $sbE.append('{')
            val sbLen = $sbE.length()
            ${
              Expr.ofList(t.fields.map{ f =>
                f.fieldRef.refType match
                  case '[e] =>
                    val fieldValue = Select.unique(aE.asTerm, f.name).asExprOf[e]
                    val name = Expr(f.name)
                    if shouldTestForOkToWrite(f.fieldRef) then // A lot of drama to avoid 1 'if' stmt--it matters for max performance
                      '{
                          if isOkToWrite($fieldValue, $cfgE) then
                            $sbE.append(s""""${$name}":""")
                            ${ refWrite[e](f.fieldRef.asInstanceOf[RTypeRef[e]], fieldValue, sbE, cfgE) }
                            $sbE.append(',')
                          else $sbE
                      }
                    else
                      '{
                          $sbE.append(s""""${$name}":""")
                          ${ refWrite[e](f.fieldRef.asInstanceOf[RTypeRef[e]], fieldValue, sbE, cfgE) }
                          $sbE.append(',')
                      }
              })
            }
            // write out any non-constructor fields (non-case "plain" classes)
            if ! $isCase && $cfgE.writeNonConstructorFields then
              ${
                Expr.ofList(t.nonConstructorFields.map{ f =>
                  f.fieldRef.refType match
                    case '[e] =>
                      val fieldValue = Select.unique(aE.asTerm, f.getterLabel).asExprOf[e]
                      val name = Expr(f.name)
                      if shouldTestForOkToWrite(f.fieldRef) then // A lot of drama to avoid 1 'if' stmt--it matters for max performance
                        '{
                          if isOkToWrite($fieldValue, $cfgE) then
                            $sbE.append(s"\"${$name}\":")
                            ${ refWrite[e](f.fieldRef.asInstanceOf[RTypeRef[e]], fieldValue, sbE, cfgE) }
                            $sbE.append(',')
                          else $sbE
                        }
                      else 
                        '{
                          $sbE.append(s"\"${$name}\":")
                          ${ refWrite[e](f.fieldRef.asInstanceOf[RTypeRef[e]], fieldValue, sbE, cfgE) }
                          $sbE.append(',')
                        }
                })
              }
            if sbLen == $sbE.length() then $sbE.append('}')
            else $sbE.setCharAt($sbE.length() - 1, '}')
        }