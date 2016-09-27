package co.blocke.scalajack
package typeadapter

import scala.collection.GenTraversableOnce
import scala.collection.generic.CanBuildFrom
import scala.language.existentials
import scala.reflect.runtime.universe.{ Type, typeOf }
import scala.reflect.runtime.currentMirror

object CanBuildFromTypeAdapter extends TypeAdapterFactory {

  override def typeAdapter(tpe: Type, context: Context): Option[TypeAdapter[_]] =
    if (tpe <:< typeOf[GenTraversableOnce[_]]) {
      val requiredClassSymbol = tpe.typeSymbol.asClass

      val companionSymbol = requiredClassSymbol.companion.asModule
      val companionType = companionSymbol.info

      // Examples in comments reference Scala's List[A] type.

      val allImplicitMethods = for (member ← companionType.members if member.isMethod && member.isImplicit) yield member.asMethod

      // `implicit def canBuildFrom[A]: CanBuildFrom[Coll, A, List[A]] = ...`
      val allImplicitConversions = for (method ← allImplicitMethods if method.typeParams.size == 1 && method.paramLists.isEmpty && method.returnType <:< typeOf[CanBuildFrom[_, _, _]]) yield method

      val matchingTypeAdapters = allImplicitConversions flatMap { method ⇒
        // returnTypeAsCanBuildFrom == CanBuildFrom[Coll, A, List[A]]
        val returnTypeAsCanBuildFrom = method.returnType.baseType(typeOf[CanBuildFrom[_, _, _]].typeSymbol)

        // typeParam == A
        val typeParam = method.typeParams.head

        // elementTypeBeforeSubstitution == A
        val elementTypeBeforeSubstitution = returnTypeAsCanBuildFrom.typeArgs(1)

        // toType == List[A]
        val toType = returnTypeAsCanBuildFrom.typeArgs(2)

        // Does List[A].typeConstructor =:= List[String].typeConstructor ?
        if (toType.typeConstructor =:= tpe.baseType(toType.typeSymbol).typeConstructor) {
          // optionalTypeArg == Some(String)
          val optionalTypeArg = Reflection.solveForNeedleAfterSubstitution(
            haystackBeforeSubstitution = toType,
            haystackAfterSubstitution  = tpe.baseType(toType.typeSymbol),
            needleBeforeSubstitution   = typeParam.asType.toType
          )

          optionalTypeArg map { typeArg ⇒
            // typeParam == A
            // typeArg == String
            // elementTypeBeforeSubstitution == A
            // elementTypeAfterSubstitution == String
            val elementTypeAfterSubstitution = elementTypeBeforeSubstitution.substituteTypes(List(typeParam), List(typeArg))

            // elementTypeAdapter == TypeAdapter[String]
            val elementTypeAdapter = context.typeAdapter(elementTypeAfterSubstitution)

            val companionInstance = currentMirror.reflectModule(companionSymbol).instance
            val methodMirror = currentMirror.reflect(companionInstance).reflectMethod(method)

            val canBuildFrom = methodMirror()

            CanBuildFromTypeAdapter(canBuildFrom.asInstanceOf[CanBuildFrom[Any, Any, GenTraversableOnce[Any]]], elementTypeAdapter.asInstanceOf[TypeAdapter[Any]])
          }
        } else {
          None
        }
      }

      matchingTypeAdapters.headOption
    } else {
      None
    }

}

case class CanBuildFromTypeAdapter[Elem, To >: Null <: GenTraversableOnce[Elem]](
    canBuildFrom:       CanBuildFrom[_, Elem, To],
    elementTypeAdapter: TypeAdapter[Elem]
) extends TypeAdapter[To] {

  override def read(reader: Reader): To =
    reader.peek match {
      case TokenType.Null ⇒
        reader.readNull()

      case TokenType.BeginArray ⇒
        val builder = canBuildFrom()

        reader.beginArray()

        while (reader.hasMoreElements) {
          val element = elementTypeAdapter.read(reader)
          builder += element
        }

        reader.endArray()

        builder.result()
    }

  override def write(value: To, writer: Writer): Unit =
    if (value == null) {
      writer.writeNull()
    } else {
      writer.beginArray()

      for (element ← value) {
        elementTypeAdapter.write(element, writer)
      }

      writer.endArray()
    }

}
