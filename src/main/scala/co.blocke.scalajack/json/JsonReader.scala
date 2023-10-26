package co.blocke.scalajack
package json

import co.blocke.scala_reflection.reflect.rtypeRefs.*
import co.blocke.scala_reflection.reflect.*
import co.blocke.scala_reflection.{Clazzes, RTypeRef, TypedName}
import co.blocke.scala_reflection.rtypes.*
import co.blocke.scala_reflection.Liftables.TypedNameToExpr
import scala.quoted.*
import scala.collection.Factory
import co.blocke.scala_reflection.RType
import scala.jdk.CollectionConverters.*
import scala.collection.mutable.HashMap
import scala.util.{Failure, Success, Try}

object JsonReader:

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

  def refFn[T](ref: RTypeRef[T])(using q: Quotes, tt: Type[T])(using cache: HashMap[Expr[TypedName], Expr[(JsonConfig, JsonParser) => Either[ParseError, ?]]]): Expr[(JsonConfig, JsonParser) => Either[ParseError, T]] =
    import Clazzes.*
    import quotes.reflect.*

    ref match
      case t: PrimitiveRef[?] if t.name == BOOLEAN_CLASS =>
        '{ (j: JsonConfig, p: JsonParser) => p.expectBoolean(j, p).map(_.asInstanceOf[T]) }
      case t: PrimitiveRef[?] if t.name == BYTE_CLASS =>
        '{ (j: JsonConfig, p: JsonParser) => p.expectLong(j, p).map(_.toByte.asInstanceOf[T]) }
      case t: PrimitiveRef[?] if t.name == CHAR_CLASS =>
        '{ (j: JsonConfig, p: JsonParser) =>
          p.expectString(j, p)
            .flatMap(s =>
              s.toArray.headOption match
                case Some(c) => Right(c.asInstanceOf[T])
                case None    => Left(JsonParseError(s"Cannot convert value '$s' into a Char."))
            )
        }
      case t: PrimitiveRef[?] if t.name == DOUBLE_CLASS =>
        '{ (j: JsonConfig, p: JsonParser) => p.expectDouble(j, p).map(_.asInstanceOf[T]) }
      case t: PrimitiveRef[?] if t.name == FLOAT_CLASS =>
        '{ (j: JsonConfig, p: JsonParser) => p.expectDouble(j, p).map(_.toFloat.asInstanceOf[T]) }
      case t: PrimitiveRef[?] if t.name == INT_CLASS =>
        '{ (j: JsonConfig, p: JsonParser) => p.expectLong(j, p).map(_.toInt.asInstanceOf[T]) }
      case t: PrimitiveRef[?] if t.name == LONG_CLASS =>
        '{ (j: JsonConfig, p: JsonParser) => p.expectLong(j, p).map(_.asInstanceOf[T]) }
      case t: PrimitiveRef[?] if t.name == SHORT_CLASS =>
        '{ (j: JsonConfig, p: JsonParser) => p.expectLong(j, p).map(_.toShort.asInstanceOf[T]) }
      case t: PrimitiveRef[T] if t.name == STRING_CLASS =>
        '{ (j: JsonConfig, p: JsonParser) => p.expectString(j, p).map(_.asInstanceOf[T]) }

      case t: SeqRef[T] =>
        t.refType match
          case '[s] =>
            t.elementRef.refType match
              case '[e] =>
                val subFn = refFn[e](t.elementRef.asInstanceOf[RTypeRef[e]]).asInstanceOf[Expr[(JsonConfig, JsonParser) => Either[ParseError, e]]]
                '{ (j: JsonConfig, p: JsonParser) =>
                  p.expectList[e](j, $subFn).map(_.to(${ Expr.summon[Factory[e, T]].get })) // Convert List to whatever the target type should be
                }
      case t: ArrayRef[T] =>
        t.refType match
          case '[s] =>
            t.elementRef.refType match
              case '[e] =>
                val subFn = refFn[e](t.elementRef.asInstanceOf[RTypeRef[e]]).asInstanceOf[Expr[(JsonConfig, JsonParser) => Either[ParseError, e]]]
                '{ (j: JsonConfig, p: JsonParser) =>
                  p.expectList[e](j, $subFn).map(_.to(${ Expr.summon[Factory[e, T]].get })) // Convert List to whatever the target type should be
                }

      case t: ScalaOptionRef[T] =>
        t.refType match
          case '[s] =>
            t.optionParamType.refType match
              case '[e] =>
                val subFn = refFn[e](t.optionParamType.asInstanceOf[RTypeRef[e]]).asInstanceOf[Expr[(JsonConfig, JsonParser) => Either[ParseError, e]]]
                val isNullable = Expr(t.optionParamType.isNullable)
                '{ (j: JsonConfig, p: JsonParser) =>
                  p.nullCheck match {
                    case true if j.noneAsNull         => Right(None.asInstanceOf[T])
                    case true if j.forbidNullsInInput => Left(JsonParseError(s"Forbidden 'null' value received at position [${p.getPos}]"))
                    case true if ! $isNullable        => Left(JsonParseError(s"Null value given for non-nullable value type at position [${p.getPos}]"))
                    case true                         => Right(Some(null).asInstanceOf[T])
                    case false                        => $subFn(j, p).map(v => Some(v).asInstanceOf[T])
                  }
                }

      case t: ScalaEnumRef[T] =>
        t.refType match
          case '[s] =>
            val rtypeExpr = t.expr
            '{ (j: JsonConfig, p: JsonParser) =>
              val rtype = $rtypeExpr.asInstanceOf[ScalaEnumRType[T]]
              j.enumsAsIds match
                case '*' =>
                  p.expectLong(j, p).flatMap { v =>
                    val fromOrdinalMethod = Class.forName(rtype.name).getMethod("fromOrdinal", classOf[Int])
                    scala.util.Try(fromOrdinalMethod.invoke(null, v.toInt).asInstanceOf[T]) match
                      case Success(v2) => Right(v2)
                      case Failure(e)  => Left(JsonParseError(s"No enum value in ${rtype.name} for ordinal value '$v'"))
                  }
                case enumList: List[String] if enumList.contains(rtype.name) =>
                  p.expectLong(j, p).flatMap { v =>
                    val fromOrdinalMethod = Class.forName(rtype.name).getMethod("fromOrdinal", classOf[Int])
                    scala.util.Try(fromOrdinalMethod.invoke(null, v.toInt).asInstanceOf[T]) match
                      case Success(v2) => Right(v2)
                      case Failure(e)  => Left(JsonParseError(s"No enum value in ${rtype.name} for ordinal value '$v'"))
                  }
                case _ =>
                  p.expectString(j, p).flatMap { v =>
                    val valueOfMethod = Class.forName(rtype.name).getMethod("valueOf", classOf[String])
                    scala.util.Try(valueOfMethod.invoke(null, v).asInstanceOf[T]) match
                      case Success(v2) => Right(v2)
                      case Failure(e)  => Left(JsonParseError(s"No enum value in ${rtype.name} for value '$v'"))
                  }
            }

      case t: ScalaClassRef[T] =>
        t.refType match
          case '[s] =>
            // IDEA:  Somewhere at this level (globally tho) create a seenBefore cache.  Pass this cache to classParseMap() and don't
            // descend on any SelfRef's found.  Do something else (TBD) with these that, when run, looks up the right fn from cache.
            val parseTable = classParseMap[T](t)
            val instantiator = classInstantiator[T](t)
            val classFn = '{ (j: JsonConfig, p: JsonParser) =>
              val rtype = ${ t.expr }.asInstanceOf[ScalaClassRType[T]]
              val presetFieldValues = scala.collection.mutable.HashMap.empty[String, Any]
              rtype.fields.foreach(f =>
                if f.fieldType.clazz == Clazzes.OptionClazz then presetFieldValues.put(f.name, None)
                else if f.asInstanceOf[ScalaFieldInfo].defaultValueAccessorName.isDefined then
                  val (companion, accessor) = f.asInstanceOf[ScalaFieldInfo].defaultValueAccessorName.get

                  // Have to use Java reflection here to get default value--Scala compiler won't have access to companion
                  // or accessor if we do a ${} block, and using compiler staging would murder performance.
                  val defaultValue = {
                    val c = Class.forName(companion)
                    val cons = c.getDeclaredConstructor()
                    cons.setAccessible(true)
                    val m = c.getMethod(accessor)
                    m.setAccessible(true)
                    m.invoke(cons.newInstance())
                  }
                  presetFieldValues.put(f.name, defaultValue)
              )
              val classFieldMap = $parseTable(p) // Map[String, JsonConfig => Either[ParseError, ?]]
              p.expectClass[T](j, classFieldMap, $instantiator, presetFieldValues)
            }
            cache.put(Expr(t.typedName), classFn)
            classFn

      case t: AliasRef[T] =>
        t.refType match
          case '[s] =>
            t.unwrappedType.refType match
              case '[e] =>
                val subFn = refFn[e](t.unwrappedType.asInstanceOf[RTypeRef[e]]).asInstanceOf[Expr[(JsonConfig, JsonParser) => Either[ParseError, e]]]
                '{ (j: JsonConfig, p: JsonParser) => $subFn(j, p).asInstanceOf[Either[co.blocke.scalajack.json.ParseError, T]] }

      case t: SelfRefRef[T] =>
        t.refType match
          case '[s] =>
            val className = Expr(t.typedName.toString)
            '{ (j: JsonConfig, p: JsonParser) =>
              val cname = $className
              p.cache.get(cname.asInstanceOf[TypedName]) match
                case Some(fn) => fn(j, p).asInstanceOf[Either[co.blocke.scalajack.json.ParseError, T]]
                case None     => Left(ParseError(s"Expected self-ref class $cname but none found in cache at position [${p.getPos}]"))
            }

      // TODO:
      // * Enumeration
      // * Java Primitives
      // * Java Classes
      // * Java Collections
      // * Java Enums
      // * Non-case Scala classes
      // * Map
      // * Scala2Ref
      // * SealedTraitRef
      // * SelfRefRef
      // * TraitRef
      // * TryRef
      // * TupleRef

  // -----------------------------------

  def classParseMap[T: Type](ref: ClassRef[T])(using q: Quotes)(using
      cache: scala.collection.mutable.HashMap[Expr[TypedName], Expr[(JsonConfig, JsonParser) => Either[ParseError, ?]]]
  ): Expr[JsonParser => Map[String, (JsonConfig, JsonParser) => Either[ParseError, ?]]] =
    import Clazzes.*
    '{ (parser: JsonParser) =>
      val daList = ${
        val fieldList = ref.fields.map(f =>
          f.fieldRef.refType match
            case '[m] =>
              val fn = refFn[m](f.fieldRef.asInstanceOf[RTypeRef[m]])
              '{
                ${ Expr(f.name) } -> $fn
              }
        )
        Expr.ofList(fieldList)
      }
      daList.toMap
    }
