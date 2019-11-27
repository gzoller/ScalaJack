package co.blocke.scalajack
package delimited

import model._
import scala.reflect.runtime.universe._

/**
  * This class is a cut'n paste copy of DelimitedFlavor with some mods to lock in a type.  There's currently an
  * unfortunate amount of boilerplate copying between this class and DelimitedFlavor, but it facilitates a clean
  * user experience--smooth API for ScalaJack:  val fooSerializer = sj.forType[Foo];  fooSerializer.read(input)
  */
case class DelimitedFlavorFor[J](
    delimiter: Char = ',',
    ta: TypeAdapter[J],
    override val defaultHint: String = "_hint",
    override val permissivesOk: Boolean = false,
    override val customAdapters: List[TypeAdapterFactory] = List.empty[TypeAdapterFactory],
    override val hintMap: Map[Type, String] = Map.empty[Type, String],
    override val hintValueModifiers: Map[Type, HintValueModifier] = Map.empty[Type, HintValueModifier],
    override val typeValueModifier: HintValueModifier = DefaultHintModifier,
    override val parseOrElseMap: Map[Type, Type] = Map.empty[Type, Type],
    override val enumsAsInt: Boolean = false
) extends JackFlavorFor[DELIMITED, J] {

  def read[T](input: DELIMITED)(implicit tt: TypeTag[T]): T = {
    val parser = DelimitedParser(
      delimiter,
      DELIM_PREFIX + input,
      this
    )
    taCache.typeAdapter(tt.tpe.dealias).read(parser).asInstanceOf[T]
  }

  def read(input: DELIMITED): J =
    ta.read(
      DelimitedParser(
        delimiter,
        DELIM_PREFIX + input,
        this
      )
    )
  def render(t: J): DELIMITED = {
    val sb = co.blocke.scalajack.compat.StringBuilder()
    ta.write(t, writer, sb)
    sb.result()
  }
  def forType[U](implicit tu: TypeTag[U]): JackFlavorFor[DELIMITED, U] =
    this
      .copy(ta = taCache.typeAdapter(tu.tpe.dealias))
      .asInstanceOf[JackFlavorFor[DELIMITED, U]]

  def render[T](t: T)(implicit tt: TypeTag[T]): DELIMITED = {
    val sb = co.blocke.scalajack.compat.StringBuilder()
    taCache
      .typeAdapter(tt.tpe.dealias)
      .asInstanceOf[TypeAdapter[T]]
      .write(t, writer, sb)
    sb.result()
  }

  // $COVERAGE-OFF$All this is carbon-copy from DelimitedFlavor, which has test coverage.
  def parse(input: DELIMITED): Parser = DelimitedParser(delimiter, input, this)

  private val writer = DelimitedWriter(delimiter)

//  override val stringifyMapKeys: Boolean = true
//  override lazy val anyMapKeyTypeAdapter =
//    typeadapter.AnyMapKeyTypeAdapter(this, anyTypeAdapter)

  def allowPermissivePrimitives(): JackFlavor[String] =
    throw new ScalaJackError("Not available for delimited encoding")
  def enumsAsInts(): JackFlavor[String] = this.copy(enumsAsInt = true)
  def parseOrElse(poe: (Type, Type)*): JackFlavor[String] =
    this.copy(parseOrElseMap = this.parseOrElseMap ++ poe)
  def withAdapters(ta: TypeAdapterFactory*): JackFlavor[String] =
    throw new ScalaJackError("Not available for delimited encoding")
  def withDefaultHint(hint: String): JackFlavor[String] =
    throw new ScalaJackError("Not available for delimited encoding")
  def withHints(h: (Type, String)*): JackFlavor[String] =
    throw new ScalaJackError("Not available for delimited encoding")
  def withHintModifiers(hm: (Type, HintValueModifier)*): JackFlavor[String] =
    throw new ScalaJackError("Not available for delimited encoding")
  def withTypeValueModifier(tm: HintValueModifier): JackFlavor[String] =
    throw new ScalaJackError("Not available for delimited encoding")

  def stringWrapTypeAdapterFactory[T](
      wrappedTypeAdapter: TypeAdapter[T],
      emptyStringOk: Boolean = true
  )(implicit tt: TypeTag[T]): TypeAdapter[T] =
    wrappedTypeAdapter // No-Op for delimited
  // $COVERAGE-ON$
}

case class DelimitedFlavor(
    delimiter: Char = ',',
    override val defaultHint: String = "_hint",
    override val permissivesOk: Boolean = false,
    override val customAdapters: List[TypeAdapterFactory] = List.empty[TypeAdapterFactory],
    override val hintMap: Map[Type, String] = Map.empty[Type, String],
    override val hintValueModifiers: Map[Type, HintValueModifier] = Map.empty[Type, HintValueModifier],
    override val typeValueModifier: HintValueModifier = DefaultHintModifier,
    override val parseOrElseMap: Map[Type, Type] = Map.empty[Type, Type],
    override val enumsAsInt: Boolean = false
) extends JackFlavor[DELIMITED] {

  def read[T](input: DELIMITED)(implicit tt: TypeTag[T]): T = {
    val parser = DelimitedParser(
      delimiter,
      DELIM_PREFIX + input,
      this
    )
    taCache.typeAdapter(tt.tpe.dealias).read(parser).asInstanceOf[T]
  }

  def forType[U](implicit tu: TypeTag[U]): JackFlavorFor[DELIMITED, U] =
    DelimitedFlavorFor(
      delimiter,
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

  def render[T](t: T)(implicit tt: TypeTag[T]): DELIMITED = {
    val sb = co.blocke.scalajack.compat.StringBuilder()
    taCache
      .typeAdapter(tt.tpe.dealias)
      .asInstanceOf[TypeAdapter[T]]
      .write(t, writer, sb)
    sb.result()
  }

  def parse(input: DELIMITED): Parser = DelimitedParser(delimiter, input, this)

  private val writer = DelimitedWriter(delimiter)

//  override val stringifyMapKeys: Boolean = true
//  override lazy val anyMapKeyTypeAdapter =
//    typeadapter.AnyMapKeyTypeAdapter(this, anyTypeAdapter)

  def allowPermissivePrimitives(): JackFlavor[String] =
    throw new ScalaJackError("Not available for delimited encoding")
  def enumsAsInts(): JackFlavor[String] = this.copy(enumsAsInt = true)
  def parseOrElse(poe: (Type, Type)*): JackFlavor[String] =
    this.copy(parseOrElseMap = this.parseOrElseMap ++ poe)
  def withAdapters(ta: TypeAdapterFactory*): JackFlavor[String] =
    throw new ScalaJackError("Not available for delimited encoding")
  def withDefaultHint(hint: String): JackFlavor[String] =
    throw new ScalaJackError("Not available for delimited encoding")
  def withHints(h: (Type, String)*): JackFlavor[String] =
    throw new ScalaJackError("Not available for delimited encoding")
  def withHintModifiers(hm: (Type, HintValueModifier)*): JackFlavor[String] =
    throw new ScalaJackError("Not available for delimited encoding")
  def withTypeValueModifier(tm: HintValueModifier): JackFlavor[String] =
    throw new ScalaJackError("Not available for delimited encoding")

  def stringWrapTypeAdapterFactory[T](
      wrappedTypeAdapter: TypeAdapter[T],
      emptyStringOk: Boolean = true
  )(implicit tt: TypeTag[T]): TypeAdapter[T] =
    wrappedTypeAdapter // No-Op for delimited

  override def bakeCache(): TypeAdapterCache = {
    val dads = super.bakeCache()
    dads.copy(factories = List(DelimitedEitherTypeAdapterFactory, DelimitedOptionTypeAdapterFactory) ++ dads.factories)
  }
}
