package co.blocke.scalajack
package typeadapter

import model._
import util.{ Path, Reflection }

import scala.collection.generic.CanBuildFrom
import scala.collection.mutable.Builder
import scala.collection._

case class CanBuildFromTypeAdapterFactory(override val enumsAsInt: Boolean) extends CanBuildFromTypeAdapterFactoryPrototype {
  val stringifyMapKeys = false // Json stringifies, other wire formats might not
}

trait CanBuildFromTypeAdapterFactoryPrototype extends TypeAdapterFactory {

  val stringifyMapKeys: Boolean
  val enumsAsInt: Boolean

  override def typeAdapterOf[T](next: TypeAdapterFactory)(implicit context: Context, tt: TypeTag[T]): TypeAdapter[T] =
    if (tt.tpe <:< typeOf[GenTraversableOnce[_]]) {
      val requiredClassSymbol = tt.tpe.typeSymbol.asClass
      val companionSymbol = requiredClassSymbol.companion.asModule
      val companionType = companionSymbol.info

      // Examples in comments reference Scala's List[A] type.

      val methods = for (member <- companionType.members if member.isMethod) yield member.asMethod

      // `implicit def canBuildFrom[A]: CanBuildFrom[Coll, A, List[A]] = ...`
      val implicitConversions = for (method <- methods if method.isImplicit && method.paramLists.flatten.isEmpty && method.returnType <:< typeOf[CanBuildFrom[_, _, _]]) yield method

      val method = implicitConversions.head
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

      val finalTA = if (tt.tpe <:< typeOf[GenMapLike[_, _, _]] && elementTypeAfterSubstitution <:< typeOf[(_, _)]) {
        val keyType = elementTypeAfterSubstitution.typeArgs(0)
        val keyTypeAdapter = context.typeAdapter(keyType)
        val valueTypeAdapter = context.typeAdapter(elementTypeAfterSubstitution.typeArgs(1))

        // Wrap Map keys in a StringWrapTypeAdapter?
        val finalKeyTypeAdapter =
          if (!stringifyMapKeys
            || keyTypeAdapter.isInstanceOf[Stringish]
            || keyType <:< typeOf[Enumeration#Value] && !enumsAsInt
            || keyType =:= typeOf[Any]
            || (keyTypeAdapter.isInstanceOf[OptionTypeAdapter[_]] && keyTypeAdapter.asInstanceOf[OptionTypeAdapter[_]].valueIsStringish()))
            keyTypeAdapter
          else
            new StringWrapTypeAdapter(keyTypeAdapter)

        val finalValueTypeAdapter = if (elementTypeAfterSubstitution.typeArgs(1) <:< typeOf[Option[_]])
          valueTypeAdapter.asInstanceOf[OptionTypeAdapter[_]].convertNullToNone()
        else
          valueTypeAdapter

        buildMapTA(companionInstance, method, finalKeyTypeAdapter, finalValueTypeAdapter)
      } else {
        val elementTypeAdapter = context.typeAdapter(elementTypeAfterSubstitution) // This dies for Map!
        buildListTA(companionInstance, method, elementTypeAdapter)
      }

      finalTA.asInstanceOf[TypeAdapter[T]]
    } else {
      next.typeAdapterOf[T]
    }

  // These two bits of wonderment here are to extract the specific Key, Value, and Elem types so they're clearly defined
  // when the CanBuildFromTypeAdapters are constructed.  Otherwise we'd just have Any, which is unhelpful.
  private def buildMapTA[Key, Value, To >: Null <: scala.collection.GenMapLike[Key, Value, To]](companionInstance: Any, method: MethodSymbol, keyTypeAdapter: TypeAdapter[Key], valueTypeAdapter: TypeAdapter[Value])(implicit keyTT: TypeTag[Key]) = {
    val canBuildFrom = reflect(companionInstance).reflectMethod(method).apply().asInstanceOf[CanBuildFrom[_, (Key, Value), To]]
    val keyIsOptional = keyTypeAdapter.isInstanceOf[OptionTypeAdapter[_]] || (keyTypeAdapter.isInstanceOf[StringWrapTypeAdapter[_]] && keyTypeAdapter.asInstanceOf[StringWrapTypeAdapter[_]].wrappedTypeAdapter.isInstanceOf[OptionTypeAdapter[_]])
    val valueIsOptional = valueTypeAdapter.isInstanceOf[OptionTypeAdapter[_]] || (valueTypeAdapter.isInstanceOf[StringWrapTypeAdapter[_]] && valueTypeAdapter.asInstanceOf[StringWrapTypeAdapter[_]].wrappedTypeAdapter.isInstanceOf[OptionTypeAdapter[_]])

    CanBuildMapTypeAdapter(
      canBuildFrom,
      keyIsOptional,
      valueIsOptional,
      keyTypeAdapter,
      valueTypeAdapter)
  }

  private def buildListTA[Elem, To >: Null <: GenTraversableOnce[Elem]](companionInstance: Any, method: MethodSymbol, elemTypeAdapter: TypeAdapter[Elem]) = {
    val canBuildFrom = reflect(companionInstance).reflectMethod(method).apply().asInstanceOf[CanBuildFrom[_, Elem, To]]
    val elemIsOptional = elemTypeAdapter.isInstanceOf[OptionTypeAdapter[_]]
    CanBuildFromTypeAdapter(
      canBuildFrom,
      elemIsOptional,
      elemTypeAdapter)
  }
}

case class CanBuildMapTypeAdapter[Key, Value, To >: Null <: GenMapLike[Key, Value, To]](
    canBuildFrom:     CanBuildFrom[_, (Key, Value), To],
    keyIsOptional:    Boolean,
    valueIsOptional:  Boolean,
    keyTypeAdapter:   TypeAdapter[Key],
    valueTypeAdapter: TypeAdapter[Value])(implicit keyTT: TypeTag[Key]) extends TypeAdapter[To] {

  def read[WIRE](path: Path, reader: Transceiver[WIRE]): To = reader.readMap[Key, Value, To](path, canBuildFrom, keyTypeAdapter, valueTypeAdapter)
  def write[WIRE](t: To, writer: Transceiver[WIRE], out: Builder[Any, WIRE], isMapKey: Boolean): Unit = {
    val filterKey = {
      if (keyIsOptional)
        t.asInstanceOf[GenMap[Key, Value]].filterNot { case (k, v) => k == None }
      else
        t
    }
    val filterValue = {
      if (valueIsOptional)
        filterKey.asInstanceOf[GenMap[Key, Value]].filterNot { case (k, v) => v == None }
      else
        filterKey
    }
    writer.writeMap(filterValue.asInstanceOf[GenMap[Key, Value]], keyTypeAdapter, valueTypeAdapter, out)
  }
}

case class CanBuildFromTypeAdapter[Elem, To >: Null <: GenTraversableOnce[Elem]](
    canBuildFrom:       CanBuildFrom[_, Elem, To],
    elemIsOptional:     Boolean,
    elementTypeAdapter: TypeAdapter[Elem]) extends TypeAdapter[To] {
  def read[WIRE](path: Path, reader: Transceiver[WIRE]): To = reader.readArray[Elem, To](path, canBuildFrom, elementTypeAdapter)
  def write[WIRE](t: To, writer: Transceiver[WIRE], out: Builder[Any, WIRE], isMapKey: Boolean): Unit =
    if (elemIsOptional)
      writer.writeArray(t.asInstanceOf[GenIterable[Elem]].filterNot(_ == None), elementTypeAdapter, out)
    else
      writer.writeArray(t.asInstanceOf[GenIterable[Elem]], elementTypeAdapter, out)
}
