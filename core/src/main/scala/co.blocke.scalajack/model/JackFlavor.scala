package co.blocke.scalajack
package model

import co.blocke.scalajack.compat
import typeadapter._

import scala.collection.mutable
import scala.reflect.runtime.universe._

/**
 * Special type-locked version of JackFlavor.  The normal read/render in JackFlavor reflect on the type
 * of T, which takes valuable time.  If you have repeated operations on a single type you can create JackFlavorFor,
 * which is already locked into type T (doesn't need to look it up for read/render).  This results in a 20%+ boost
 * in performance in some cases.
 * @tparam WIRE
 * @tparam T
 */
trait JackFlavorFor[WIRE, T] extends JackFlavor[WIRE] {
  def read(input: WIRE): T
  def render(t: T): WIRE
}

trait JackFlavor[WIRE] extends Filterable[WIRE] with ViewSplice {

  def read[T](input: WIRE)(implicit tt: TypeTag[T]): T
  def render[T](t: T)(implicit tt: TypeTag[T]): WIRE
  def forType[T](implicit tt: TypeTag[T]): JackFlavorFor[WIRE, T]

  def parse(input: WIRE): Parser

  val defaultHint: String = "_hint"
  val stringifyMapKeys: Boolean = false
  val hintMap: Map[Type, String] = Map.empty[Type, String]
  val hintValueModifiers: Map[Type, HintValueModifier] =
    Map.empty[Type, HintValueModifier]
  val typeValueModifier: HintValueModifier = DefaultHintModifier
  val enumsAsInt: Boolean = false
  val customAdapters: List[TypeAdapterFactory] = List.empty[TypeAdapterFactory]
  val parseOrElseMap: Map[Type, Type] = Map.empty[Type, Type]
  val permissivesOk: Boolean = false

  lazy val taCache: TypeAdapterCache = bakeCache()

  def bakeCache(): TypeAdapterCache = {
    val intermediateContext = TypeAdapterCache(
      this,
      customAdapters ::: co.blocke.scalajack.compat
        .CollectionTypeAdapterFactory(this, enumsAsInt) ::
        TypeAdapterCache.StandardFactories
    )

    // ParseOrElse functionality
    val parseOrElseFactories = parseOrElseMap.map {
      case (attemptedType, fallbackType @ _) =>
        val attemptedTypeAdapter =
          intermediateContext.typeAdapter(attemptedType)
        val fallbackTypeAdapter = intermediateContext.typeAdapter(fallbackType)

        new TypeAdapterFactory {
          override def typeAdapterOf[T](next: TypeAdapterFactory)(
              implicit
              taCache: TypeAdapterCache,
              typeTag: TypeTag[T]
          ): TypeAdapter[T] =
            if (typeTag.tpe =:= attemptedType) {
              val primary = attemptedTypeAdapter.asInstanceOf[TypeAdapter[T]]
              FallbackTypeAdapter[T, T](
                () =>
                  taCache, // We use an accessor function here because taCache isn't baked at this point!
                Some(primary),
                fallbackType
              )
            } else {
              next.typeAdapterOf[T]
            }
        }
    }.toList

    println("Factories: " + parseOrElseFactories)

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

    intermediateContext.copy(
      factories = parseOrElseFactories ::: permissives ::: intermediateContext.factories
    )
  }

  def enumsAsInts(): JackFlavor[WIRE]

  // These is so pervasively handy, let's just pre-stage it for easy access
  lazy val stringTypeAdapter: TypeAdapter[String] =
    taCache.typeAdapterOf[String]
  lazy val anyTypeAdapter: TypeAdapter[Any] = taCache.typeAdapterOf[Any]
  lazy val anyMapKeyTypeAdapter: TypeAdapter[Any] = taCache.typeAdapterOf[Any]

  // Look up any custom hint label for given type, and if none then use default
  def getHintLabelFor(tpe: Type): String = hintMap.getOrElse(tpe, defaultHint)

  def stringWrapTypeAdapterFactory[T](
      wrappedTypeAdapter: TypeAdapter[T],
      emptyStringOk:      Boolean        = true
  )(implicit tt: TypeTag[T]): TypeAdapter[T]

  def allowPermissivePrimitives(): JackFlavor[WIRE]
  def parseOrElse(poe: (Type, Type)*): JackFlavor[WIRE]
  def withAdapters(ta: TypeAdapterFactory*): JackFlavor[WIRE]
  def withDefaultHint(hint: String): JackFlavor[WIRE]
  def withHints(h: (Type, String)*): JackFlavor[WIRE]
  def withHintModifiers(hm: (Type, HintValueModifier)*): JackFlavor[WIRE]
  def withTypeValueModifier(tm: HintValueModifier): JackFlavor[WIRE]

  // Need WIRE-specific Builder instance.  By default this is StringBuilder.  Mongo will overwrite this.
  def getBuilder: mutable.Builder[WIRE, WIRE] =
    co.blocke.scalajack.compat
      .StringBuilder()
      .asInstanceOf[mutable.Builder[WIRE, WIRE]]
}
