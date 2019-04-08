package co.blocke.scalajack
package typeadapter

import model._
import util.{ Path, Reflection }

import scala.collection.mutable.Builder
import scala.collection._

case class CanBuildFromTypeAdapterFactory(jackFlavor: JackFlavor[_], enumsAsInt: Boolean, stringifyMapKeys: Boolean = false) extends TypeAdapterFactory {

  override def typeAdapterOf[T](next: TypeAdapterFactory)(implicit context: Context, tt: TypeTag[T]): TypeAdapter[T] =
    if (tt.tpe <:< typeOf[GenTraversableOnce[_]]) {
      val requiredClassSymbol = tt.tpe.typeSymbol.asClass
      val companionSymbol = requiredClassSymbol.companion.asModule
      val companionType = companionSymbol.info

      // Examples in comments reference Scala's List[A] type.

      val methods = for (member <- companionType.members if member.isMethod) yield member.asMethod

      // `implicit def canBuildFrom[A]: CanBuildFrom[Coll, A, List[A]] = ...`
      val implicitConversions = for (method <- methods if method.paramLists.flatten.isEmpty && method.returnType <:< typeOf[Builder[_, _]]) yield method

      val method = implicitConversions.head
      val returnTypeAsCanBuildFrom = method.returnType.baseType(typeOf[Builder[_, _]].typeSymbol)

      // typeParam == A
      //      val typeParams = method.typeParams

      // toType == List[A]
      val toType = returnTypeAsCanBuildFrom.typeArgs(1)
      //      val toType = returnTypeAsCanBuildFrom.typeArgs(2)

      val typeParamSubstitutions: List[(Symbol, Type)] = method.typeParams.flatMap { typeParam =>
        // typeParam == A
        // optionalTypeArg == Some(String)
        val optionalTypeArg = Reflection.solveForNeedleAfterSubstitution(
          haystackBeforeSubstitution = toType,
          haystackAfterSubstitution  = tt.tpe,
          needleBeforeSubstitution   = typeParam.asType.toType)
        optionalTypeArg.map(typeArg => typeParam -> typeArg)
      }

      // elementTypeBeforeSubstitution == A
      val elementTypeBeforeSubstitution = returnTypeAsCanBuildFrom.typeArgs(0)

      // elementTypeAfterSubstitution == String
      val elementTypeAfterSubstitution = elementTypeBeforeSubstitution.substituteTypes(typeParamSubstitutions.map(_._1), typeParamSubstitutions.map(_._2))

      val companionInstance = reflectModule(companionSymbol).instance

      val finalTA = if (tt.tpe <:< typeOf[Map[_, _]] && elementTypeAfterSubstitution <:< typeOf[(_, _)]) {
        //    val finalTA = if (IsMap(tt.tpe) && elementTypeAfterSubstitution <:< typeOf[(_, _)]) {
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
            jackFlavor.stringWrapTypeAdapterFactory(keyTypeAdapter)

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
  private def buildMapTA[Key, Value, To](companionInstance: Any, method: MethodSymbol, keyTypeAdapter: TypeAdapter[Key], valueTypeAdapter: TypeAdapter[Value])(implicit keyTT: TypeTag[Key]) = {
    val builderFactory = reflect(companionInstance).reflectMethod(method)
    // Note: We include Any here because Any *could* be an Option, so we must include it as a possibility
    val keyIsOptional =
      keyTypeAdapter.isInstanceOf[OptionTypeAdapter[_]] ||
        (keyTypeAdapter.isInstanceOf[StringWrapTypeAdapter[_]] && keyTypeAdapter.asInstanceOf[StringWrapTypeAdapter[_]].wrappedTypeAdapter.isInstanceOf[OptionTypeAdapter[_]]) ||
        keyTypeAdapter.isInstanceOf[AnyTypeAdapter]

    val valueIsOptional =
      valueTypeAdapter.isInstanceOf[OptionTypeAdapter[_]] ||
        (valueTypeAdapter.isInstanceOf[StringWrapTypeAdapter[_]] && valueTypeAdapter.asInstanceOf[StringWrapTypeAdapter[_]].wrappedTypeAdapter.isInstanceOf[OptionTypeAdapter[_]]) ||
        valueTypeAdapter.isInstanceOf[AnyTypeAdapter]

    CanBuildMapTypeAdapter(
      builderFactory,
      keyIsOptional,
      valueIsOptional,
      keyTypeAdapter,
      valueTypeAdapter)
  }

  private def buildListTA[Elem, To >: Null <: GenTraversableOnce[Elem]](companionInstance: Any, method: MethodSymbol, elemTypeAdapter: TypeAdapter[Elem]) = {
    val builderFactory = reflect(companionInstance).reflectMethod(method)
    val elemIsOptional = elemTypeAdapter.isInstanceOf[OptionTypeAdapter[_]]
    CanBuildFromTypeAdapter(
      builderFactory,
      elemIsOptional,
      elemTypeAdapter)
  }
}

case class CanBuildMapTypeAdapter[Key, Value, To <: Map[Key, Value]]( //) >: Null <: GenMapLike[Key, Value, To]](
    builderFactory:   MethodMirror, // Builds a Builder[(Key, Value), To] when applied
    keyIsOptional:    Boolean,
    valueIsOptional:  Boolean,
    keyTypeAdapter:   TypeAdapter[Key],
    valueTypeAdapter: TypeAdapter[Value])(implicit keyTT: TypeTag[Key]) extends TypeAdapter[To] with Collectionish {

  def read[WIRE](path: Path, reader: Reader[WIRE]): To =
    reader.readMap[Key, Value, To](path, builderFactory, keyTypeAdapter, valueTypeAdapter)

  def write[WIRE](t: To, writer: Writer[WIRE], out: Builder[WIRE, WIRE], isMapKey: Boolean): Unit = {
    val filterKey = {
      if (keyIsOptional && t != null)
        t.asInstanceOf[Map[Key, Value]].filterNot { case (k, v) => k == None }
      else
        t
    }
    val filterValue = {
      if (valueIsOptional && t != null)
        filterKey.asInstanceOf[Map[Key, Value]].filterNot { case (k, v) => v == None }
      else
        filterKey
    }
    writer.writeMap(filterValue.asInstanceOf[Map[Key, Value]], keyTypeAdapter, valueTypeAdapter, out)
  }
}

case class CanBuildFromTypeAdapter[Elem, To](
    builderFactory:     MethodMirror, // Builds a Builder[Elem, To] when applied
    elemIsOptional:     Boolean,
    elementTypeAdapter: TypeAdapter[Elem]) extends TypeAdapter[To] with Collectionish {

  def read[WIRE](path: Path, reader: Reader[WIRE]): To =
    reader.readArray[Elem, To](path, builderFactory, elementTypeAdapter)

  def write[WIRE](t: To, writer: Writer[WIRE], out: Builder[WIRE, WIRE], isMapKey: Boolean): Unit =
    if (elemIsOptional)
      writer.writeArray(t.asInstanceOf[Iterable[Elem]].filterNot(_ == None), elementTypeAdapter, out)
    else
      writer.writeArray(t.asInstanceOf[Iterable[Elem]], elementTypeAdapter, out)
}
