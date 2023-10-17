package co.blocke.scalajack
package model

import typeadapter._
import scala.collection.mutable
import co.blocke.scala_reflection._

trait JackFlavor[WIRE] extends ViewSplice: // extends Filterable[WIRE] with ViewSplice {

  type WIRE_TYPE = WIRE

  def parse(input: WIRE): Parser

  val defaultHint: String                                = "_hint"
  val stringifyMapKeys: Boolean                          = false
  val hintMap: Map[String, String]                       = Map.empty[String, String]
  val hintValueModifiers: Map[String, HintValueModifier] = Map.empty[String, HintValueModifier]
  val typeValueModifier: HintValueModifier               = DefaultHintModifier
  val enumsAsInt: Boolean                                = false
  val customAdapters: List[TypeAdapterFactory]           = List.empty[TypeAdapterFactory]
  val parseOrElseMap: Map[Class[_], RType]               = Map.empty[Class[_], RType]
  val permissivesOk: Boolean                             = false

  lazy val taCache: TypeAdapterCache = bakeCache()

  def bakeCache(): TypeAdapterCache = 
    val permissives =
      if (permissivesOk)
        List(
          PermissiveBigDecimalTypeAdapterFactory,
          PermissiveBigIntTypeAdapterFactory,
          PermissiveBooleanTypeAdapterFactory,
          PermissiveByteTypeAdapterFactory,
          PermissiveDoubleTypeAdapterFactory,
          PermissiveFloatTypeAdapterFactory,
          PermissiveIntTypeAdapterFactory,
          PermissiveLongTypeAdapterFactory,
          PermissiveShortTypeAdapterFactory,
          PermissiveJavaBigDecimalTypeAdapterFactory,
          PermissiveJavaBigIntegerTypeAdapterFactory,
          PermissiveJavaBooleanTypeAdapterFactory,
          PermissiveJavaByteTypeAdapterFactory,
          PermissiveJavaDoubleTypeAdapterFactory,
          PermissiveJavaFloatTypeAdapterFactory,
          PermissiveJavaIntTypeAdapterFactory,
          PermissiveJavaLongTypeAdapterFactory,
          PermissiveJavaNumberTypeAdapterFactory,
          PermissiveJavaShortTypeAdapterFactory
        )
      else
        List.empty[TypeAdapterFactory]

    // Need this (w/o parseOrElseFactories) because resolving the parse-or-else type adapters will cause an endless loop!
    val stage1TC = TypeAdapterCache(
      this,
      permissives ::: customAdapters ::: TypeAdapterCache.StandardFactories
    )
    
    // ParseOrElse functionality
    val parseOrElseFactories: List[TypeAdapterFactory] = parseOrElseMap.map {
      case (attemptedTypeClass, fallbackType) => 
        new TypeAdapterFactory {
          def matches(concrete: RType): Boolean = concrete.infoClass == attemptedTypeClass
        
          def makeTypeAdapter(concrete: RType)(implicit taCache: TypeAdapterCache): TypeAdapter[_] = 
            FallbackTypeAdapter( stage1TC.typeAdapterOf(concrete), stage1TC.typeAdapterOf(fallbackType) )
        }
    }.toList

    TypeAdapterCache(
      this,
      parseOrElseFactories ::: stage1TC.factories
    )

  final inline def read[T](input: WIRE): T =
    val typeAdapter = taCache.typeAdapterOf[T]
    _read(input, typeAdapter)

  def _read[T](input: WIRE, typeAdapter: TypeAdapter[T]): T

  final inline def render[T](t: T): WIRE =
    val typeAdapter = taCache.typeAdapterOf[T]
    _render(t, typeAdapter)

  def _render[T](t: T, typeAdapter: TypeAdapter[T]): WIRE

  // These is so pervasively handy, let's just pre-stage it for easy access
  lazy val stringTypeAdapter: TypeAdapter[String] = taCache.typeAdapterOf[String]
  lazy val anyTypeAdapter: TypeAdapter[Any]       = taCache.typeAdapterOf[Any]

  // Look up any custom hint label for given type, and if none then use default
  def getHintLabelFor(tpe: RType): String = hintMap.getOrElse(tpe.name, defaultHint)

  def stringWrapTypeAdapterFactory[T](
      wrappedTypeAdapter: TypeAdapter[T],
      emptyStringOk: Boolean = true
    ): TypeAdapter[T]
  def maybeStringWrapTypeAdapterFactory[T](
      wrappedTypeAdapter: TypeAdapter[T],
      emptyStringOk: Boolean = true
    ): TypeAdapter[T]

  def enumsAsInts(): JackFlavor[WIRE]
  def allowPermissivePrimitives(): JackFlavor[WIRE]
  def parseOrElse(poe: (RType, RType)*): JackFlavor[WIRE]
  def withAdapters(ta: TypeAdapterFactory*): JackFlavor[WIRE]
  def withDefaultHint(hint: String): JackFlavor[WIRE]
  def withHints(h: (RType, String)*): JackFlavor[WIRE]
  def withHintModifiers(hm: (RType, HintValueModifier)*): JackFlavor[WIRE]
  def withTypeValueModifier(tm: HintValueModifier): JackFlavor[WIRE]

  // Need WIRE-specific Builder instance.  By default this is StringBuilder.  Mongo will overwrite this.
  def getBuilder: mutable.Builder[WIRE, WIRE] =
    StringBuilder()
      .asInstanceOf[mutable.Builder[WIRE, WIRE]]
