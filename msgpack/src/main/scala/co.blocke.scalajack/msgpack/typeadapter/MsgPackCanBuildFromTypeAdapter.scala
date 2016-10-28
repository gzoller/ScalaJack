package co.blocke.scalajack
package msgpack
package typeadapter

import co.blocke.scalajack.json.Tokenizer
import co.blocke.scalajack.typeadapter.OptionTypeAdapter

import scala.collection.{ GenMapLike, GenTraversableOnce }
import scala.collection.generic.CanBuildFrom
import scala.language.existentials
import scala.reflect.runtime.currentMirror
import scala.reflect.runtime.universe.{ Symbol, Type, typeOf }

object MsgPackCanBuildFromTypeAdapter extends TypeAdapterFactory {

  override def typeAdapter(tpe: Type, context: Context): Option[TypeAdapter[_]] =
    if (tpe <:< typeOf[GenTraversableOnce[_]]) {
      val requiredClassSymbol = tpe.typeSymbol.asClass

      val companionSymbol = requiredClassSymbol.companion.asModule
      val companionType = companionSymbol.info

      // Examples in comments reference Scala's List[A] type.

      val methods = for (member ← companionType.members if member.isMethod) yield member.asMethod

      // `implicit def canBuildFrom[A]: CanBuildFrom[Coll, A, List[A]] = ...`
      val implicitConversions = for (method ← methods if method.isImplicit && method.paramLists.flatten.isEmpty && method.returnType <:< typeOf[CanBuildFrom[_, _, _]]) yield method

      val matchingTypeAdapters = implicitConversions flatMap { method ⇒
        // returnTypeAsCanBuildFrom == CanBuildFrom[Coll, A, List[A]]
        val returnTypeAsCanBuildFrom = method.returnType.baseType(typeOf[CanBuildFrom[_, _, _]].typeSymbol)

        // typeParam == A
        val typeParams = method.typeParams

        // toType == List[A]
        val toType = returnTypeAsCanBuildFrom.typeArgs(2)

        val typeParamSubstitutions: List[(Symbol, Type)] = typeParams flatMap { typeParam ⇒
          // typeParam == A
          // optionalTypeArg == Some(String)
          val optionalTypeArg = Reflection.solveForNeedleAfterSubstitution(
            haystackBeforeSubstitution = toType,
            haystackAfterSubstitution  = tpe.baseType(toType.typeSymbol),
            needleBeforeSubstitution   = typeParam.asType.toType
          )
          optionalTypeArg.map(typeArg ⇒ typeParam → typeArg)
        }

        // elementTypeBeforeSubstitution == A
        val elementTypeBeforeSubstitution = returnTypeAsCanBuildFrom.typeArgs(1)

        // elementTypeAfterSubstitution == String
        val elementTypeAfterSubstitution = elementTypeBeforeSubstitution.substituteTypes(typeParamSubstitutions.map(_._1), typeParamSubstitutions.map(_._2))

        // elementTypeAdapter == TypeAdapter[String]
        val elementTypeAdapter = context.typeAdapter(elementTypeAfterSubstitution)

        val companionInstance = currentMirror.reflectModule(companionSymbol).instance
        val canBuildFrom = currentMirror.reflect(companionInstance).reflectMethod(method).apply()

        if (tpe <:< typeOf[GenMapLike[_, _, _]] && elementTypeAfterSubstitution <:< typeOf[(_, _)]) {
          val keyType = elementTypeAfterSubstitution.typeArgs(0)
          val keyTypeAdapter = context.typeAdapter(keyType) match {
            case kta: OptionTypeAdapter[_] ⇒ kta.emptyVersion // output "" for None for map keys
            case kta                       ⇒ kta
          }
          val valueTypeAdapter = context.typeAdapter(elementTypeAfterSubstitution.typeArgs(1))

          Some(MsgPackCanBuildMapTypeAdapter(canBuildFrom.asInstanceOf[CanBuildFrom[Any, Any, GenMapLike[Any, Any, Any] with Null]], keyTypeAdapter.asInstanceOf[TypeAdapter[Any]], valueTypeAdapter.asInstanceOf[TypeAdapter[Any]]))
        } else {
          Some(MsgPackCanBuildFromTypeAdapter(canBuildFrom.asInstanceOf[CanBuildFrom[Any, Any, GenTraversableOnce[Any]]], elementTypeAdapter.asInstanceOf[TypeAdapter[Any]]))
        }
      }

      matchingTypeAdapters.headOption
    } else {
      None
    }

}

case class MsgPackCanBuildMapTypeAdapter[Key, Value, To >: Null <: GenMapLike[Key, Value, To]](
    canBuildFrom:     CanBuildFrom[_, (Key, Value), To],
    keyTypeAdapter:   TypeAdapter[Key],
    valueTypeAdapter: TypeAdapter[Value]
) extends TypeAdapter[To] {

  override def read(reader: Reader): To =
    reader.peek match {
      case TokenType.BeginObject ⇒
        val builder = canBuildFrom()

        reader.beginObject()

        while (reader.hasMoreMembers) {
          val key = keyTypeAdapter.read(reader)
          val value = valueTypeAdapter.read(reader)
          builder += key → value
        }

        reader.endObject()

        builder.result()

      case TokenType.Null ⇒
        reader.readNull()
    }

  override def write(map: To, writer: Writer): Unit =
    if (map == null) {
      writer.writeNull()
    } else {
      val numMembers = map.foldLeft(0)((acc, m) => if (m._2 == None) acc else acc + 1)
      writer.asInstanceOf[MsgPackWriter].beginObject(numMembers)

      map foreach {
        case (key, value) if (value != None) ⇒
          keyTypeAdapter.write(key, writer)
          valueTypeAdapter.write(value, writer)
        case _ ⇒ // do nothing
      }

      writer.endObject()
    }

}

case class MsgPackCanBuildFromTypeAdapter[Elem, To >: Null <: GenTraversableOnce[Elem]](
    canBuildFrom:       CanBuildFrom[_, Elem, To],
    elementTypeAdapter: TypeAdapter[Elem]
) extends TypeAdapter[To] {

  override def read(reader: Reader): To =
    reader.peek match {
      case TokenType.BeginArray ⇒
        val builder = canBuildFrom()

        reader.beginArray()

        while (reader.hasMoreElements) {
          val element = elementTypeAdapter.read(reader)
          builder += element
        }

        reader.endArray()

        builder.result()

      case TokenType.Null ⇒
        reader.readNull()
    }

  override def write(value: To, writer: Writer): Unit =
    if (value == null) {
      writer.writeNull()
    } else {
      // TODO: For Tuples we need to render null for None, so we actually have to pre-render with a synthetic
      // writer to see what will be output "", or something else, and not count the "" values.
      val numMembers = value.foldLeft(0)((acc, m) => if (m == None) acc else acc + 1)
      writer.asInstanceOf[MsgPackWriterLike].beginArray(numMembers)

      for (element ← value) {
        if (element != None)
          elementTypeAdapter.write(element, writer)
      }

      writer.endArray()
    }

}
