package co.blocke.scalajack
package compat

import json.StringWrapTypeAdapter

import scala.reflect.runtime.universe._
import scala.collection.{ mutable, _ }
import util.Reflection
import model._
import typeadapter._

case class CollectionTypeAdapterFactory(
    jackFlavor: JackFlavor[_],
    enumsAsInt: Boolean)
  extends TypeAdapterFactory {

  override def typeAdapterOf[T](next: TypeAdapterFactory)(
      implicit
      taCache: TypeAdapterCache,
      tt:      TypeTag[T]
  ): TypeAdapter[T] =
    if (tt.tpe <:< typeOf[GenTraversableOnce[_]]) {
      val requiredClassSymbol = tt.tpe.typeSymbol.asClass
      val companionSymbol = requiredClassSymbol.companion.asModule
      val companionType = companionSymbol.info

      // Examples in comments reference Scala's List[A] type.

      val methods = for (member <- companionType.members if member.isMethod)
        yield member.asMethod

      //TODO: See what kinds of things are being selected from methods to populate implicitConversions.
      //TODO: In 2.13 RC3, this isn't working but I need a reference to compare to...

      // `implicit def canBuildFrom[A]: CanBuildFrom[Coll, A, List[A]] = ...`
      val implicitConversions =
        for (
          method <- methods if method.paramLists.flatten.isEmpty && method.returnType <:< typeOf[mutable.Builder[_, _]]
        ) yield method

      val method = implicitConversions.head
      val returnTypeAsBuilder =
        method.returnType.baseType(typeOf[mutable.Builder[_, _]].typeSymbol)

      // typeParam == A
      //      val typeParams = method.typeParams

      // toType == List[A]
      val toType = returnTypeAsBuilder.typeArgs(1)
      //      val toType = returnTypeAsBuilder.typeArgs(2)

      val typeParamSubstitutions: List[(Symbol, Type)] =
        method.typeParams.flatMap { typeParam =>
          // typeParam == A
          // optionalTypeArg == Some(String)
          val optionalTypeArg = Reflection.solveForNeedleAfterSubstitution(
            haystackBeforeSubstitution = toType,
            haystackAfterSubstitution  = tt.tpe,
            needleBeforeSubstitution   = typeParam.asType.toType
          )
          optionalTypeArg.map(typeArg => typeParam -> typeArg)
        }

      // elementTypeBeforeSubstitution == A
      val elementTypeBeforeSubstitution = returnTypeAsBuilder.typeArgs.head

      // elementTypeAfterSubstitution == String
      val elementTypeAfterSubstitution =
        elementTypeBeforeSubstitution.substituteTypes(
          typeParamSubstitutions.map(_._1),
          typeParamSubstitutions.map(_._2)
        )

      val companionInstance = reflectModule(companionSymbol).instance

      val finalTA =
        if (tt.tpe <:< typeOf[Map[_, _]] && elementTypeAfterSubstitution <:< typeOf[(_, _)]) {
          //    val finalTA = if (IsMap(tt.tpe) && elementTypeAfterSubstitution <:< typeOf[(_, _)]) {
          val keyType = elementTypeAfterSubstitution.typeArgs.head
          val keyTypeAdapter = taCache.typeAdapter(keyType)
          val valueTypeAdapter =
            taCache.typeAdapter(elementTypeAfterSubstitution.typeArgs(1))

          // Wrap Map keys in a StringWrapTypeAdapter?
          val finalKeyTypeAdapter =
            if (keyType =:= typeOf[Any])
              jackFlavor.anyMapKeyTypeAdapter
            else if (!jackFlavor.stringifyMapKeys
              || keyTypeAdapter.isInstanceOf[Stringish]
              || keyType <:< typeOf[Enumeration#Value] && !enumsAsInt
              || keyTypeAdapter
              .isInstanceOf[ValueClassTypeAdapter[_, _]] && keyTypeAdapter
              .asInstanceOf[ValueClassTypeAdapter[_, _]]
              .sourceTypeAdapter
              .isInstanceOf[Stringish]
              || (keyTypeAdapter
                .isInstanceOf[OptionTypeAdapter[_]] && keyTypeAdapter
                .asInstanceOf[OptionTypeAdapter[_]]
                .valueIsStringish()))
              keyTypeAdapter
            else
              jackFlavor.stringWrapTypeAdapterFactory(keyTypeAdapter)

          val finalValueTypeAdapter =
            if (elementTypeAfterSubstitution.typeArgs(1) <:< typeOf[Option[_]])
              valueTypeAdapter
                .asInstanceOf[OptionTypeAdapter[_]]
                .convertNullToNone()
            else
              valueTypeAdapter

          buildMapTA(
            companionInstance,
            method,
            finalKeyTypeAdapter,
            finalValueTypeAdapter
          )
        } else {
          val elementTypeAdapter = taCache.typeAdapter(
            elementTypeAfterSubstitution
          ) // This dies for Map!
          buildListTA(companionInstance, method, elementTypeAdapter)
        }

      finalTA.asInstanceOf[TypeAdapter[T]]
    } else {
      next.typeAdapterOf[T]
    }

  // These two bits of wonderment here are to extract the specific Key, Value, and Elem types so they're clearly defined
  // when the CanBuildFromTypeAdapters are constructed.  Otherwise we'd just have Any, which is unhelpful.
  private def buildMapTA[Key, Value, To](
      companionInstance: Any,
      method:            MethodSymbol,
      keyTypeAdapter:    TypeAdapter[Key],
      valueTypeAdapter:  TypeAdapter[Value]
  )(implicit keyTT: TypeTag[Key]) = {
    val builderFactory = reflect(companionInstance).reflectMethod(method)
    // Note: We include Any here because Any *could* be an Option, so we must include it as a possibility
    val keyIsOptionalOrAny =
      keyTypeAdapter.isInstanceOf[OptionTypeAdapter[_]] ||
        (keyTypeAdapter.isInstanceOf[StringWrapTypeAdapter[_]] && keyTypeAdapter
          .asInstanceOf[StringWrapTypeAdapter[_]]
          .wrappedTypeAdapter
          .isInstanceOf[OptionTypeAdapter[_]]) ||
          keyTypeAdapter == jackFlavor.anyMapKeyTypeAdapter

    val valueIsOptionalOrAny = valueTypeAdapter
      .isInstanceOf[OptionTypeAdapter[_]] ||
      valueTypeAdapter.isInstanceOf[AnyTypeAdapter]

    MapLikeTypeAdapter(
      builderFactory,
      keyIsOptionalOrAny,
      valueIsOptionalOrAny,
      keyTypeAdapter,
      valueTypeAdapter
    )
  }

  private def buildListTA[Elem, To >: Null <: GenTraversableOnce[Elem]](
      companionInstance: Any,
      method:            MethodSymbol,
      elemTypeAdapter:   TypeAdapter[Elem]
  ) = {
    val builderFactory = reflect(companionInstance).reflectMethod(method)
    val elemIsOptional = elemTypeAdapter.isInstanceOf[OptionTypeAdapter[_]]
    ListLikeTypeAdapter(builderFactory, elemIsOptional, elemTypeAdapter)
  }
}

case class MapLikeTypeAdapter[Key, Value, To <: Map[Key, Value]](
    builderFactory:       MethodMirror, // Builds a Builder[(Key, Value), To] when applied
    keyIsOptionalOrAny:   Boolean,
    valueIsOptionalOrAny: Boolean,
    keyTypeAdapter:       TypeAdapter[Key],
    valueTypeAdapter:     TypeAdapter[Value]
) extends TypeAdapter[To]
  with Collectionish {
  def read(parser: Parser): To =
    if (parser.peekForNull)
      null.asInstanceOf[To]
    else
      parser.expectMap(
        keyTypeAdapter,
        valueTypeAdapter,
        builderFactory().asInstanceOf[mutable.Builder[(Key, Value), To]]
      )

  def write[WIRE](
      t:      To,
      writer: Writer[WIRE],
      out:    mutable.Builder[WIRE, WIRE]): Unit = {
    val filterKey = {
      if (keyIsOptionalOrAny && t != null)
        t.asInstanceOf[Map[Key, Value]].filterNot { case (k, v) => k == None }
      else
        t
    }
    val filterValue = {
      if (valueIsOptionalOrAny && t != null)
        filterKey.asInstanceOf[Map[Key, Value]].filterNot {
          case (k, v) => v == None
        }
      else
        filterKey
    }
    writer.writeMap(
      filterValue.asInstanceOf[Map[Key, Value]],
      keyTypeAdapter,
      valueTypeAdapter,
      out
    )
  }
}

case class ListLikeTypeAdapter[Elem, To](
    builderFactory:     MethodMirror, // Builds a Builder[Elem, To] when applied
    elemIsOptional:     Boolean,
    elementTypeAdapter: TypeAdapter[Elem]
) extends TypeAdapter[To]
  with Collectionish {

  def read(parser: Parser): To =
    if (parser.peekForNull)
      null.asInstanceOf[To]
    else
      parser.expectList(
        elementTypeAdapter,
        builderFactory().asInstanceOf[mutable.Builder[Elem, To]]
      )

  def write[WIRE](
      t:      To,
      writer: Writer[WIRE],
      out:    mutable.Builder[WIRE, WIRE]): Unit =
    if (elemIsOptional)
      writer.writeArray(
        t.asInstanceOf[Iterable[Elem]].filterNot(_ == None),
        elementTypeAdapter,
        out
      )
    else
      writer.writeArray(t.asInstanceOf[Iterable[Elem]], elementTypeAdapter, out)
}
