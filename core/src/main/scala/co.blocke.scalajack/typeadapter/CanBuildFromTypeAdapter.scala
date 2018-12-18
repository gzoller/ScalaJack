package co.blocke.scalajack
package typeadapter

import model._
import util.{ Reflection, Path }
import scala.collection.generic.CanBuildFrom
import scala.collection.{ GenMapLike, GenTraversableOnce, mutable }

object CanBuildFromTypeAdapterFactory extends TypeAdapterFactory {

  override def typeAdapterOf[T](next: TypeAdapterFactory)(implicit context: Context, tt: TypeTag[T]): TypeAdapter[T] =
    if (tt.tpe <:< typeOf[GenTraversableOnce[_]]) {
      val requiredClassSymbol = tt.tpe.typeSymbol.asClass
      val companionSymbol = requiredClassSymbol.companion.asModule
      val companionType = companionSymbol.info

      // Examples in comments reference Scala's List[A] type.

      val methods = for (member <- companionType.members if member.isMethod) yield member.asMethod

      // `implicit def canBuildFrom[A]: CanBuildFrom[Coll, A, List[A]] = ...`
      val implicitConversions = for (method <- methods if method.isImplicit && method.paramLists.flatten.isEmpty && method.returnType <:< typeOf[CanBuildFrom[_, _, _]]) yield method

      val matchingTypeAdapters = implicitConversions flatMap { method =>
        val returnTypeAsCanBuildFrom = method.returnType.baseType(typeOf[CanBuildFrom[_, _, _]].typeSymbol)

        // typeParam == A
        //      val typeParams = method.typeParams

        // toType == List[A]
        val toType = returnTypeAsCanBuildFrom.typeArgs(2)

        val typeParamSubstitutions: List[(Symbol, Type)] = method.typeParams.flatMap { typeParam =>
          // typeParam == A
          // optionalTypeArg == Some(String)
          val optionalTypeArg = Reflection.solveForNeedleAfterSubstitution(
            haystackBeforeSubstitution = toType,
            haystackAfterSubstitution  = tt.tpe.baseType(toType.typeSymbol),
            needleBeforeSubstitution   = typeParam.asType.toType)
          optionalTypeArg.map(typeArg => typeParam -> typeArg)
        }

        // elementTypeBeforeSubstitution == A
        val elementTypeBeforeSubstitution = returnTypeAsCanBuildFrom.typeArgs(1)

        // elementTypeAfterSubstitution == String
        val elementTypeAfterSubstitution = elementTypeBeforeSubstitution.substituteTypes(typeParamSubstitutions.map(_._1), typeParamSubstitutions.map(_._2))

        val companionInstance = reflectModule(companionSymbol).instance
        val canBuildFrom = reflect(companionInstance).reflectMethod(method).apply()

        if (tt.tpe <:< typeOf[GenMapLike[_, _, _]] && elementTypeAfterSubstitution <:< typeOf[(_, _)]) {
          val keyType = elementTypeAfterSubstitution.typeArgs(0)
          val keyTypeAdapter = context.typeAdapter(keyType)
          //        val keyTypeAdapter = context.typeAdapter(keyType) match {
          //          case kta: OptionTypeAdapter[_] => kta.noneAsEmptyString // output "" for None for map keys
          //          case kta                       => kta
          //        }
          val valueTypeAdapter = context.typeAdapter(elementTypeAfterSubstitution.typeArgs(1))

          Some(CanBuildMapTypeAdapter(
            canBuildFrom.asInstanceOf[CanBuildFrom[Any, Any, GenMapLike[Any, Any, Any] with Null]],
            keyTypeAdapter.asInstanceOf[TypeAdapter[Any]],
            valueTypeAdapter.asInstanceOf[TypeAdapter[Any]]))
        } else {
          // elementTypeAdapter == TypeAdapter[String]
          val elementTypeAdapter = context.typeAdapter(elementTypeAfterSubstitution) // This dies for Map!

          Some(CanBuildFromTypeAdapter[Any, GenTraversableOnce[Any]](canBuildFrom.asInstanceOf[CanBuildFrom[Any, Any, GenTraversableOnce[Any]]], elementTypeAdapter.asInstanceOf[TypeAdapter[Any]]))
        }
      }

      matchingTypeAdapters.headOption.map(_.asInstanceOf[TypeAdapter[T]]).getOrElse(next.typeAdapterOf[T])
    } else {
      next.typeAdapterOf[T]
    }
}

case class CanBuildMapTypeAdapter[Key, Value, To >: Null <: GenMapLike[Key, Value, To]](
    canBuildFrom:     CanBuildFrom[_, (Key, Value), To],
    keyTypeAdapter:   TypeAdapter[Key],
    valueTypeAdapter: TypeAdapter[Value]) extends TypeAdapter[To] {

  def read(reader: Reader): To = reader.readMap[Key, Value, To](canBuildFrom, keyTypeAdapter, valueTypeAdapter)
}

case class CanBuildFromTypeAdapter[Elem, To >: Null <: GenTraversableOnce[Elem]](canBuildFrom: CanBuildFrom[_, Elem, To], elementTypeAdapter: TypeAdapter[Elem]) extends TypeAdapter[To] {

  def read(reader: Reader): To = reader.readArray[Elem, To](canBuildFrom, elementTypeAdapter)
}
