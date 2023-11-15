package co.blocke.scalajack
package json

import co.blocke.scala_reflection.{RTypeRef, TypedName}
import co.blocke.scala_reflection.rtypes.{OptionRType, ScalaFieldInfo}
import co.blocke.scala_reflection.reflect.rtypeRefs.*
import scala.quoted.*
import scala.collection.mutable.HashMap
import scala.util.{Failure, Success, Try}
import scala.collection.Factory

/** We depart from ZIO-Json here.  ZIO-Json uses implicits to marshal the right JsonDecoder.  This works great for primitive types
  * but I had issues trying to get it to work with macros.  Since we have all the necessary elements to explicitly provide
  * decoders for "constructed" types (collections, classes, ...) we just provided them explicitly.
  */
object JsonReader:

  def refRead[T](
      ref: RTypeRef[T]
  )(using q: Quotes, tt: Type[T]): Expr[JsonDecoder[T]] =
    import quotes.reflect.*

    ref match
      case r: PrimitiveRef[?] =>
        Expr.summon[JsonDecoder[T]].getOrElse(throw JsonTypeError("No JsonDecoder defined for type " + TypeRepr.of[T].typeSymbol.name))

      case r: SeqRef[?] =>
        r.refType match
          case '[t] =>
            r.elementRef.refType match
              case '[e] =>
                val elemDecoder = Expr.summon[JsonDecoder[e]].getOrElse(refRead[e](r.elementRef.asInstanceOf[RTypeRef[e]]))
                '{
                  JsonDecoder.seq[e]($elemDecoder) map (_.to(${ Expr.summon[Factory[e, T]].get }))
                }

      case r: ScalaClassRef[?] =>
        val fieldNames = Expr(r.fields.map(_.name).toArray)
        val fieldDecoders = Expr.ofList(
          r.fields.map(f =>
            f.fieldRef.refType match
              case '[e] =>
                Expr.summon[JsonDecoder[e]].getOrElse(refRead[e](f.fieldRef.asInstanceOf[RTypeRef[e]]))
          )
        )
        val instantiator = JsonReaderUtil.classInstantiator[T](r.asInstanceOf[ClassRef[T]])
        val optionalFields = Expr(r.fields.zipWithIndex.collect { case (f, i) if f.fieldRef.isInstanceOf[OptionRef[_]] => i }.toArray)
        val fieldsE = Expr.ofList(r.fields.asInstanceOf[List[ScalaFieldInfoRef]].map(_.expr))

        // TODO:  See if we can't get the default values at compile-time.  Mix with null/None for a predefined value array:
        // Use the Select.... business to fire the default accessors and get an Expr[Any] value back, that we can unscrable in '{}

        '{
          val preloadedFieldValues: Array[Any] = $fieldsE
            .asInstanceOf[List[ScalaFieldInfo]]
            .map(_ match
              case optFld if optFld.fieldType.isInstanceOf[OptionRType[_]] => None
              case defFld if defFld.defaultValueAccessorName.isDefined =>
                val (companion, accessor) = defFld.defaultValueAccessorName.get
                // Have to use Java reflection here to get default value--Scala compiler won't have access to companion
                // or accessor if we do a ${} block, and using compiler staging would murder performance.
                {
                  val c = Class.forName(companion)
                  val cons = c.getDeclaredConstructor()
                  cons.setAccessible(true)
                  val m = c.getMethod(accessor)
                  m.setAccessible(true)
                  m.invoke(cons.newInstance())
                }
              case _ => null
            )
            .toArray
          ClassDecoder[T]($fieldNames, $fieldDecoders.toArray, $instantiator, preloadedFieldValues)
        }

      // We'd love to pass this in--preloaded with defaults/None values
      // val fieldValues = new Array[Any](fields.length)
