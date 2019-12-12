package co.blocke.scalajack
package json4s

import co.blocke.scalajack.json4s
import model._
import typeadapter.AnyMapKeyTypeAdapter
import org.json4s._

import scala.collection.mutable
import scala.reflect.runtime.universe._

/**
 * This class is a cut'n paste copy of JsonFlavor with some mods to lock in a type.  There's currently an
 * unfortunate amount of boilerplate copying between this class and JsonFlavor, but it facilitates a clean
 * user experience--smooth API for ScalaJack:  val fooSerializer = sj.forType[Foo];  fooSerializer.read(input)
 */
case class Json4sFlavorFor[J](
    ta:                              TypeAdapter[J],
    override val defaultHint:        String                       = "_hint",
    override val permissivesOk:      Boolean                      = false,
    override val customAdapters:     List[TypeAdapterFactory]     = List.empty[TypeAdapterFactory],
    override val hintMap:            Map[Type, String]            = Map.empty[Type, String],
    override val hintValueModifiers: Map[Type, HintValueModifier] = Map.empty[Type, HintValueModifier],
    override val typeValueModifier:  HintValueModifier            = DefaultHintModifier,
    override val parseOrElseMap:     Map[Type, Type]              = Map.empty[Type, Type],
    override val enumsAsInt:         Boolean                      = false
) extends JackFlavorFor[JValue, J] {

  def read(js: JValue): J = ta.read(json4s.Json4sParser(js, this))

  def render(t: J): JValue = {
    val sb = JValueBuilder()
    ta.write(t, writer, sb)
    sb.result()
  }

  def forType[U](implicit tu: TypeTag[U]): JackFlavorFor[JValue, U] =
    this
      .copy(ta = taCache.typeAdapter(tu.tpe.dealias))
      .asInstanceOf[JackFlavorFor[JValue, U]]

  // $COVERAGE-OFF$All this is carbon-copy from JsonFlavor, which has test coverage.
  def read[T](js: JValue)(implicit tt: TypeTag[T]): T = {
    val parser = Json4sParser(js, this)
    taCache.typeAdapter(tt.tpe.dealias).read(parser).asInstanceOf[T]
  }

  def render[T](t: T)(implicit tt: TypeTag[T]): JValue = {
    val sb = json4s.JValueBuilder()
    taCache
      .typeAdapter(tt.tpe.dealias)
      .asInstanceOf[TypeAdapter[T]]
      .write(t, writer, sb)
    sb.result()
  }

  def parse(input: JValue): Parser = Json4sParser(input, this)

  private val writer = Json4sWriter()

  override val stringifyMapKeys: Boolean = true
  override lazy val anyMapKeyTypeAdapter: AnyMapKeyTypeAdapter =
    typeadapter.AnyMapKeyTypeAdapter(this, anyTypeAdapter)

  def allowPermissivePrimitives(): JackFlavor[JValue] =
    throw new ScalaJackError("Permissive primitives not supported for Json4s")
  def enumsAsInts(): JackFlavor[JValue] = this.copy(enumsAsInt = true)
  def parseOrElse(poe: (Type, Type)*): JackFlavor[JValue] =
    this.copy(parseOrElseMap = this.parseOrElseMap ++ poe)
  def withAdapters(ta: TypeAdapterFactory*): JackFlavor[JValue] =
    this.copy(customAdapters = this.customAdapters ++ ta.toList)
  def withDefaultHint(hint: String): JackFlavor[JValue] =
    this.copy(defaultHint = hint)
  def withHints(h: (Type, String)*): JackFlavor[JValue] =
    this.copy(hintMap = this.hintMap ++ h)
  def withHintModifiers(hm: (Type, HintValueModifier)*): JackFlavor[JValue] =
    this.copy(hintValueModifiers = this.hintValueModifiers ++ hm)
  def withTypeValueModifier(tm: HintValueModifier): JackFlavor[JValue] =
    this.copy(typeValueModifier = tm)

  def stringWrapTypeAdapterFactory[T](
      wrappedTypeAdapter: TypeAdapter[T],
      emptyStringOk:      Boolean        = true
  )(implicit tt: TypeTag[T]): TypeAdapter[T] =
    StringWrapTypeAdapter(wrappedTypeAdapter)

  override def getBuilder: mutable.Builder[JValue, JValue] =
    json4s.JValueBuilder()
  // $COVERAGE-ON$
}

case class Json4sFlavor(
    override val defaultHint:        String                       = "_hint",
    override val permissivesOk:      Boolean                      = false,
    override val customAdapters:     List[TypeAdapterFactory]     = List.empty[TypeAdapterFactory],
    override val hintMap:            Map[Type, String]            = Map.empty[Type, String],
    override val hintValueModifiers: Map[Type, HintValueModifier] = Map.empty[Type, HintValueModifier],
    override val typeValueModifier:  HintValueModifier            = DefaultHintModifier,
    override val parseOrElseMap:     Map[Type, Type]              = Map.empty[Type, Type],
    override val enumsAsInt:         Boolean                      = false
) extends JackFlavor[JValue] {

  def forType[U](implicit tu: TypeTag[U]): JackFlavorFor[JValue, U] =
    Json4sFlavorFor(
      taCache.typeAdapter(tu.tpe.dealias).asInstanceOf[TypeAdapter[U]],
      defaultHint,
      permissivesOk,
      customAdapters,
      hintMap,
      hintValueModifiers,
      typeValueModifier,
      parseOrElseMap,
      enumsAsInt
    )

  def read[T](js: JValue)(implicit tt: TypeTag[T]): T = {
    val parser = Json4sParser(js, this)
    taCache.typeAdapter(tt.tpe.dealias).read(parser).asInstanceOf[T]
  }

  def render[T](t: T)(implicit tt: TypeTag[T]): JValue = {
    val sb = json4s.JValueBuilder()
    taCache
      .typeAdapter(tt.tpe.dealias)
      .asInstanceOf[TypeAdapter[T]]
      .write(t, writer, sb)
    sb.result()
  }

  def parse(input: JValue): Parser = Json4sParser(input, this)

  private val writer = Json4sWriter()

  override val stringifyMapKeys: Boolean = true
  override lazy val anyMapKeyTypeAdapter: AnyMapKeyTypeAdapter =
    typeadapter.AnyMapKeyTypeAdapter(this, anyTypeAdapter)

  def allowPermissivePrimitives(): JackFlavor[JValue] =
    throw new ScalaJackError("Permissive primitives not supported for Json4s")
  def enumsAsInts(): JackFlavor[JValue] = this.copy(enumsAsInt = true)
  def parseOrElse(poe: (Type, Type)*): JackFlavor[JValue] =
    this.copy(parseOrElseMap = this.parseOrElseMap ++ poe)
  def withAdapters(ta: TypeAdapterFactory*): JackFlavor[JValue] =
    this.copy(customAdapters = this.customAdapters ++ ta.toList)
  def withDefaultHint(hint: String): JackFlavor[JValue] =
    this.copy(defaultHint = hint)
  def withHints(h: (Type, String)*): JackFlavor[JValue] =
    this.copy(hintMap = this.hintMap ++ h)
  def withHintModifiers(hm: (Type, HintValueModifier)*): JackFlavor[JValue] =
    this.copy(hintValueModifiers = this.hintValueModifiers ++ hm)
  def withTypeValueModifier(tm: HintValueModifier): JackFlavor[JValue] =
    this.copy(typeValueModifier = tm)

  def stringWrapTypeAdapterFactory[T](
      wrappedTypeAdapter: TypeAdapter[T],
      emptyStringOk:      Boolean        = true
  )(implicit tt: TypeTag[T]): TypeAdapter[T] =
    StringWrapTypeAdapter(wrappedTypeAdapter)

  override def getBuilder: mutable.Builder[JValue, JValue] =
    json4s.JValueBuilder()
}
