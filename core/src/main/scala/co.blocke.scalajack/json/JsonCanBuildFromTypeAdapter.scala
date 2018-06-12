package co.blocke.scalajack
package json

// This class is needed because canonical JSON can only render object (Map, class, etc.) keys as String.
// This is a JSON requirement and not something other formats need to worry about, so this specialization
// is located here.

import co.blocke.scalajack.typeadapter.{ CanBuildFromTypeAdapter, CanBuildMapTypeAdapter, OptionTypeAdapter }

import scala.collection.{ GenMapLike, GenTraversableOnce }
import scala.collection.generic.CanBuildFrom
import scala.language.existentials
import scala.reflect.runtime.currentMirror
import scala.reflect.runtime.universe.{ Symbol, Type, TypeTag, typeOf }

object JsonCanBuildFromTypeAdapter extends TypeAdapterFactory {

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
        // returnTypeAsCanBuildFrom == CanBuildFrom[Coll, A, List[A]]
        val returnTypeAsCanBuildFrom = method.returnType.baseType(typeOf[CanBuildFrom[_, _, _]].typeSymbol)

        // typeParam == A
        val typeParams = method.typeParams

        // toType == List[A]
        val toType = returnTypeAsCanBuildFrom.typeArgs(2)

        val typeParamSubstitutions: List[(Symbol, Type)] = typeParams flatMap { typeParam =>
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

        // elementTypeAdapter == TypeAdapter[String]
        val elementTypeAdapter = context.typeAdapter(elementTypeAfterSubstitution)

        val companionInstance = currentMirror.reflectModule(companionSymbol).instance
        val canBuildFrom = currentMirror.reflect(companionInstance).reflectMethod(method).apply()

        if (tt.tpe <:< typeOf[GenMapLike[_, _, _]] && elementTypeAfterSubstitution <:< typeOf[(_, _)]) {
          val keyType = elementTypeAfterSubstitution.typeArgs(0)

          val stringTypeAdapter = context.typeAdapterOf[String]

          val keyTypeAdapter =
            if (keyType =:= typeOf[String]) {
              stringTypeAdapter
            } else {
              val refinedKeyTypeAdapter = context.typeAdapter(keyType) match {
                case kta: OptionTypeAdapter[_] => kta.noneAsEmptyString // output "" for None for map keys
                case kta                       => kta
              }
              ComplexMapKeyTypeAdapter(new Tokenizer(), stringTypeAdapter, refinedKeyTypeAdapter)
            }

          val valueTypeAdapter = context.typeAdapter(elementTypeAfterSubstitution.typeArgs(1))
          Some(CanBuildMapTypeAdapter(canBuildFrom.asInstanceOf[CanBuildFrom[Any, Any, GenMapLike[Any, Any, Any] with Null]], keyTypeAdapter.asInstanceOf[TypeAdapter[Any]], valueTypeAdapter.asInstanceOf[TypeAdapter[Any]]))
        } else {
          Some(CanBuildFromTypeAdapter(null, null, canBuildFrom.asInstanceOf[CanBuildFrom[Any, Any, GenTraversableOnce[Any]]], elementTypeAdapter.asInstanceOf[TypeAdapter[Any]]))
        }
      }

      matchingTypeAdapters.headOption.map(_.asInstanceOf[TypeAdapter[T]]).getOrElse(next.typeAdapterOf[T])
    } else {
      next.typeAdapterOf[T]
    }

}