package co.blocke.scalajack
package mongo

import model._
import typeadapter._

import org.bson._

import scala.collection.mutable
import scala.reflect.runtime.universe._

case class MongoFlavorFor[J](
    ta:                              TypeAdapter[J],
    override val defaultHint:        String                       = "_hint",
    override val permissivesOk:      Boolean                      = false,
    override val customAdapters:     List[TypeAdapterFactory]     = List.empty[TypeAdapterFactory],
    override val hintMap:            Map[Type, String]            = Map.empty[Type, String],
    override val hintValueModifiers: Map[Type, HintValueModifier] = Map.empty[Type, HintValueModifier],
    override val typeValueModifier:  HintValueModifier            = DefaultHintModifier,
    override val parseOrElseMap:     Map[Type, Type]              = Map.empty[Type, Type],
    override val enumsAsInt:         Boolean                      = false
) extends JackFlavorFor[BsonValue, J] {

  def read(input: BsonValue): J = ta.read(BsonParser(input, this))
  def render(t: J): BsonValue = {
    val sb = co.blocke.scalajack.compat.BsonBuilder()
    ta.write(t, writer, sb)
    sb.result()
  }

  // $COVERAGE-OFF$All this is carbon-copy from JsonFlavor, which has test coverage.
  override val stringifyMapKeys: Boolean = true

  override lazy val anyMapKeyTypeAdapter: AnyMapKeyTypeAdapter =
    typeadapter.AnyMapKeyTypeAdapter(this, anyTypeAdapter)

  def allowPermissivePrimitives(): JackFlavor[BsonValue] =
    this.copy(permissivesOk = true)
  def enumsAsInts(): JackFlavor[BsonValue] = this.copy(enumsAsInt = true)
  def parseOrElse(poe: (Type, Type)*): JackFlavor[BsonValue] =
    this.copy(parseOrElseMap = this.parseOrElseMap ++ poe)
  def withAdapters(ta: TypeAdapterFactory*): JackFlavor[BsonValue] =
    this.copy(customAdapters = this.customAdapters ++ ta.toList)
  def withDefaultHint(hint: String): JackFlavor[BsonValue] =
    this.copy(defaultHint = hint)
  def withHints(h: (Type, String)*): JackFlavor[BsonValue] =
    this.copy(hintMap = this.hintMap ++ h)
  def withHintModifiers(hm: (Type, HintValueModifier)*): JackFlavor[BsonValue] =
    this.copy(hintValueModifiers = this.hintValueModifiers ++ hm)
  def withTypeValueModifier(tm: HintValueModifier): JackFlavor[BsonValue] =
    this.copy(typeValueModifier = tm)

  def stringWrapTypeAdapterFactory[T](
      wrappedTypeAdapter: TypeAdapter[T],
      emptyStringOk:      Boolean        = true
  )(implicit tt: TypeTag[T]): TypeAdapter[T] =
    new StringWrapTypeAdapter(wrappedTypeAdapter)

  override def bakeCache(): TypeAdapterCache = {
    val dads = super.bakeCache()
    dads.copy(
      factories = List(
        ObjectIdTypeAdapter,
        OffsetDateTimeTypeAdapter,
        ZonedDateTimeTypeAdapter
      ) ++ dads.factories
    )
  }

  private val writer = MongoWriter(anyTypeAdapter)

  def parse(input: BsonValue): Parser = BsonParser(input, this)

  def forType[U](implicit tu: TypeTag[U]): JackFlavorFor[BsonValue, U] =
    this
      .copy(ta = taCache.typeAdapter(tu.tpe.dealias))
      .asInstanceOf[JackFlavorFor[BsonValue, U]]

  def read[T](input: BsonValue)(implicit tt: TypeTag[T]): T = {
    val parser = BsonParser(input, this)
    taCache.typeAdapter(tt.tpe.dealias).read(parser).asInstanceOf[T]
  }

  def render[T](t: T)(implicit tt: TypeTag[T]): BsonValue = {
    val sb = co.blocke.scalajack.compat.BsonBuilder()
    taCache
      .typeAdapter(tt.tpe.dealias)
      .asInstanceOf[TypeAdapter[T]]
      .write(t, writer, sb)
    sb.result()
  }
  // $COVERAGE-ON$
}

case class MongoFlavor(
    override val defaultHint:        String                       = "_hint",
    override val permissivesOk:      Boolean                      = false,
    override val customAdapters:     List[TypeAdapterFactory]     = List.empty[TypeAdapterFactory],
    override val hintMap:            Map[Type, String]            = Map.empty[Type, String],
    override val hintValueModifiers: Map[Type, HintValueModifier] = Map.empty[Type, HintValueModifier],
    override val typeValueModifier:  HintValueModifier            = DefaultHintModifier,
    override val parseOrElseMap:     Map[Type, Type]              = Map.empty[Type, Type],
    override val enumsAsInt:         Boolean                      = false
) extends JackFlavor[BsonValue] {

  override val stringifyMapKeys: Boolean = true

  override lazy val anyMapKeyTypeAdapter: AnyMapKeyTypeAdapter =
    typeadapter.AnyMapKeyTypeAdapter(this, anyTypeAdapter)

  def allowPermissivePrimitives(): JackFlavor[BsonValue] =
    this.copy(permissivesOk = true)
  def enumsAsInts(): JackFlavor[BsonValue] = this.copy(enumsAsInt = true)
  def parseOrElse(poe: (Type, Type)*): JackFlavor[BsonValue] =
    this.copy(parseOrElseMap = this.parseOrElseMap ++ poe)
  def withAdapters(ta: TypeAdapterFactory*): JackFlavor[BsonValue] =
    this.copy(customAdapters = this.customAdapters ++ ta.toList)
  def withDefaultHint(hint: String): JackFlavor[BsonValue] =
    this.copy(defaultHint = hint)
  def withHints(h: (Type, String)*): JackFlavor[BsonValue] =
    this.copy(hintMap = this.hintMap ++ h)
  def withHintModifiers(hm: (Type, HintValueModifier)*): JackFlavor[BsonValue] =
    this.copy(hintValueModifiers = this.hintValueModifiers ++ hm)
  def withTypeValueModifier(tm: HintValueModifier): JackFlavor[BsonValue] =
    this.copy(typeValueModifier = tm)

  def stringWrapTypeAdapterFactory[T](
      wrappedTypeAdapter: TypeAdapter[T],
      emptyStringOk:      Boolean        = true
  )(implicit tt: TypeTag[T]): TypeAdapter[T] =
    new StringWrapTypeAdapter(wrappedTypeAdapter)

  override def bakeCache(): TypeAdapterCache = {
    val dads = super.bakeCache()
    dads.copy(
      factories = List(
        ObjectIdTypeAdapter,
        OffsetDateTimeTypeAdapter,
        ZonedDateTimeTypeAdapter
      ) ++ dads.factories
    )
  }

  private val writer = MongoWriter(anyTypeAdapter)

  def parse(input: BsonValue): Parser = BsonParser(input, this)

  def forType[U](implicit tu: TypeTag[U]): JackFlavorFor[BsonValue, U] =
    MongoFlavorFor(
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

  def read[T](input: BsonValue)(implicit tt: TypeTag[T]): T = {
    val parser = BsonParser(input, this)
    taCache.typeAdapter(tt.tpe.dealias).read(parser).asInstanceOf[T]
  }

  def render[T](t: T)(implicit tt: TypeTag[T]): BsonValue = {
    val sb = co.blocke.scalajack.compat.BsonBuilder()
    taCache
      .typeAdapter(tt.tpe.dealias)
      .asInstanceOf[TypeAdapter[T]]
      .write(t, writer, sb)
    sb.result()
  }

  override def getBuilder: mutable.Builder[BsonValue, BsonValue] =
    co.blocke.scalajack.compat.BsonBuilder()
}
