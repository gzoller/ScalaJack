package co.blocke.scalajack
package json

import co.blocke.scala_reflection.{RTypeRef, TypedName}
import co.blocke.scala_reflection.rtypes.{OptionRType, ScalaFieldInfo}
import co.blocke.scala_reflection.reflect.rtypeRefs.*
import scala.quoted.*
import scala.collection.mutable.HashMap
import scala.util.{Failure, Success, Try}
import scala.collection.Factory

/** We depart from ZIO-Json here.  ZIO-Json uses implicits to marshal the right sj.  This works great for primitive types
  * but I had issues trying to get it to work with macros.  Since we have all the necessary elements to explicitly provide
  * decoders for "constructed" types (collections, classes, ...) we just provided them explicitly.
  */
object JsonReader:

  def refRead[T](
      ref: RTypeRef[T]
  )(using q: Quotes, tt: Type[T]): Expr[sj[T]] =
    import quotes.reflect.*

    ref match
      case r: PrimitiveRef[?] =>
        Expr.summon[sj[T]].getOrElse(throw JsonTypeError("No sj defined for type " + TypeRepr.of[T].typeSymbol.name))

      case r: SeqRef[?] =>
        r.refType match
          case '[t] =>
            r.elementRef.refType match
              case '[e] =>
                val elemDecoder = Expr.summon[sj[e]].getOrElse(refRead[e](r.elementRef.asInstanceOf[RTypeRef[e]]))
                '{
                  sj.seq[e]($elemDecoder) map (_.to(${ Expr.summon[Factory[e, T]].get }))
                }

      case r: ScalaClassRef[?] =>
        val fieldNames = Expr(r.fields.map(_.name).toArray)
        val fieldDecoders = Expr.ofList(
          r.fields.map(f =>
            f.fieldRef.refType match
              case '[e] =>
                Expr.summon[sj[e]].getOrElse(refRead[e](f.fieldRef.asInstanceOf[RTypeRef[e]]))
          )
        )
        val instantiator = JsonReaderUtil.classInstantiator[T](r.asInstanceOf[ClassRef[T]])
        val optionalFields = Expr(r.fields.zipWithIndex.collect { case (f, i) if f.fieldRef.isInstanceOf[OptionRef[_]] => i }.toArray)
        val fieldsE = Expr.ofList(r.fields.asInstanceOf[List[ScalaFieldInfoRef]].map(_.expr))

        // Constructor argument list, preloaded with optional 'None' values and any default values specified
        val preloaded = Expr
          .ofList(r.fields.map { f =>
            val scalaF = f.asInstanceOf[ScalaFieldInfoRef]
            if scalaF.defaultValueAccessorName.isDefined then
              r.refType match
                case '[t] =>
                  val tpe = TypeRepr.of[t].widen
                  val sym = tpe.typeSymbol
                  val companionBody = sym.companionClass.tree.asInstanceOf[ClassDef].body
                  val companion = Ref(sym.companionModule)
                  companionBody
                    .collect {
                      case defaultMethod @ DefDef(name, _, _, _) if name.startsWith("$lessinit$greater$default$" + (f.index + 1)) =>
                        companion.select(defaultMethod.symbol).appliedToTypes(tpe.typeArgs).asExpr
                    }
                    .headOption
                    .getOrElse(Expr(null.asInstanceOf[Boolean]))
            else if scalaF.fieldRef.isInstanceOf[OptionRef[_]] then Expr(None)
            else Expr(null.asInstanceOf[Int])
          })

        '{ ClassDecoder[T]($fieldNames, $fieldDecoders.toArray, $instantiator, $preloaded.toArray) }
