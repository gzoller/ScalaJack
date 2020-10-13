package co.blocke.scalajack
package delimited

import model._
import co.blocke.scala_reflection.RType
import typeadapter.{MaybeStringWrapTypeAdapter, StringWrapTypeAdapter}

opaque type DELIMITED = String
val DELIM_PREFIX: Char = 2

case class DelimitedFlavor(
    delimiter:                       Char                           = ',',
    override val defaultHint:        String                         = "_hint",
    override val permissivesOk:      Boolean                        = false,
    override val customAdapters:     List[TypeAdapterFactory]       = List.empty[TypeAdapterFactory],
    override val hintMap:            Map[String, String]            = Map.empty[String, String],
    override val hintValueModifiers: Map[String, HintValueModifier] = Map.empty[String, HintValueModifier],
    override val typeValueModifier:  HintValueModifier              = DefaultHintModifier,
    override val parseOrElseMap:     Map[Class[_], RType]           = Map.empty[Class[_], RType],
    override val enumsAsInt:         Boolean                        = false
) extends JackFlavor[DELIMITED] {

  def _read[T](input: DELIMITED, typeAdapter: TypeAdapter[T]): T =
    val parser = DelimitedParser(delimiter, s"$DELIM_PREFIX$input".asInstanceOf[DELIMITED], this)
    typeAdapter.read(parser)

  def _render[T](t: T, typeAdapter: TypeAdapter[T]): DELIMITED =
    val sb = StringBuilder[DELIMITED]()
    typeAdapter.write(t, writer, sb)
    sb.result().asInstanceOf[DELIMITED]

  def parse(input: DELIMITED): Parser = DelimitedParser(delimiter, input, this)

  private val writer = DelimitedWriter(delimiter)

  def allowPermissivePrimitives(): JackFlavor[DELIMITED] =
    throw new ScalaJackError("Not available for delimited encoding")
  def enumsAsInts(): JackFlavor[DELIMITED] = this.copy(enumsAsInt = true)
  def parseOrElse(poe: (RType, RType)*): JackFlavor[DELIMITED] =
    this.copy(parseOrElseMap = this.parseOrElseMap ++ poe.map{(p,oe) => p.infoClass->oe})
  def withAdapters(ta: TypeAdapterFactory*): JackFlavor[DELIMITED] =
    throw new ScalaJackError("Not available for delimited encoding")
  def withDefaultHint(hint: String): JackFlavor[DELIMITED] =
    throw new ScalaJackError("Not available for delimited encoding")
  def withHints(h: (RType, String)*): JackFlavor[DELIMITED] =
    throw new ScalaJackError("Not available for delimited encoding")
  def withHintModifiers(hm: (RType, HintValueModifier)*): JackFlavor[DELIMITED] =
    throw new ScalaJackError("Not available for delimited encoding")
  def withTypeValueModifier(tm: HintValueModifier): JackFlavor[DELIMITED] =
    throw new ScalaJackError("Not available for delimited encoding")

  def stringWrapTypeAdapterFactory[T](
      wrappedTypeAdapter: TypeAdapter[T],
      emptyStringOk:      Boolean        = true
    ): TypeAdapter[T] =
    wrappedTypeAdapter // No-Op for delimited

  override def bakeCache(): TypeAdapterCache = {
    val dads = super.bakeCache()
    dads.copy(
      factories = DelimitedEitherTypeAdapterFactory ::
        DelimitedOptionTypeAdapterFactory :: dads.factories
    )
  }

  def maybeStringWrapTypeAdapterFactory[T](
    wrappedTypeAdapter: TypeAdapter[T],
    emptyStringOk: Boolean = true
  ): TypeAdapter[T] =
    MaybeStringWrapTypeAdapter(this, wrappedTypeAdapter, emptyStringOk)
}