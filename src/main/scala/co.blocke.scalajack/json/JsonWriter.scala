package co.blocke.scalajack
package json

import co.blocke.scala_reflection.RTypeRef
import co.blocke.scala_reflection.reflect.rtypeRefs.*
import co.blocke.scala_reflection.rtypes.*
import scala.quoted.*

object JsonWriter:

  def writeJsonFn[T](rtRef: RTypeRef[T])(using tt: Type[T], q: Quotes): Expr[(T, StringBuilder, JsonConfig) => StringBuilder] =
    import quotes.reflect.*
    rtRef match
      case rt: PrimitiveRef[?] if rt.isStringish =>
        '{ (a: T, sb: StringBuilder, cfg: JsonConfig) =>
          sb.append('"')
          sb.append(a.toString)
          sb.append('"')
        }
      case rt: PrimitiveRef[?] =>
        '{ (a: T, sb: StringBuilder, cfg: JsonConfig) => sb.append(a.toString) }

      case rt: AliasRef[?] =>
        writeJsonFn[rt.T](rt.unwrappedType.asInstanceOf[RTypeRef[rt.T]])(using Type.of[rt.T])

      case rt: ArrayRef[?] =>
        rt.elementRef.refType match
          case '[t] =>
            val elementFn = writeJsonFn[t](rt.elementRef.asInstanceOf[RTypeRef[t]])
            '{ (a: T, sb: StringBuilder, cfg: JsonConfig) =>
              sb.append('[')
              a.asInstanceOf[Array[?]].map { e =>
                $elementFn(e.asInstanceOf[t], sb, cfg: JsonConfig)
                sb.append(',')
              }
              sb.setCharAt(sb.length() - 1, ']')
            }

      case rt: ClassRef[?] =>
        val fieldFns = rt.fields.map { f =>
          f.fieldRef.refType match
            case '[t] => (writeJsonFn[t](f.fieldRef.asInstanceOf[RTypeRef[t]]), f)
        }
        '{ (a: T, sb: StringBuilder, cfg: JsonConfig) =>
          sb.append('{')
          ${
            val stmts = fieldFns.map { case (fn, field) =>
              field.fieldRef.refType match
                case '[t] =>
                  '{
                    sb.append(${ Expr(field.name) })
                    sb.append(':')
                    val fieldValue = ${
                      Select.unique('{ a }.asTerm, field.name).asExpr
                    }
                    val fn2 = $fn.asInstanceOf[(t, StringBuilder, JsonConfig) => StringBuilder]
                    fn2(fieldValue.asInstanceOf[t], sb, cfg: JsonConfig)
                    sb.append(',')
                  }
            }
            Expr.ofList(stmts)
          }
          sb.setCharAt(sb.length() - 1, '}')
        }

      case rt: SeqRef[?] =>
        rt.elementRef.refType match
          case '[t] =>
            val elementFn = writeJsonFn[t](rt.elementRef.asInstanceOf[RTypeRef[t]])
            '{ (a: T, sb: StringBuilder, cfg: JsonConfig) =>
              sb.append('[')
              a.asInstanceOf[Seq[?]].map { e =>
                $elementFn(e.asInstanceOf[t], sb, cfg: JsonConfig)
                sb.append(',')
              }
              sb.setCharAt(sb.length() - 1, ']')
            }

      case rt: EnumRef[?] =>
        val expr = rt.expr
        '{
          val rtype = $expr.asInstanceOf[EnumRType[?]]
          (a: T, sb: StringBuilder, cfg: JsonConfig) =>
            val enumAsId = cfg.enumsAsIds match
              case '*'                                               => true
              case aList: List[String] if aList.contains(rtype.name) => true
              case _                                                 => false
            if enumAsId then sb.append(rtype.ordinal(a.toString).getOrElse(throw new JsonError(s"Value $a is not a valid enum value for ${rtype.name}")))
            else
              sb.append('"')
              sb.append(a.toString)
              sb.append('"')
        }
