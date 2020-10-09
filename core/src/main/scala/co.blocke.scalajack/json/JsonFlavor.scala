package co.blocke.scalajack
package json

import model._
import co.blocke.scala_reflection.RType
import typeadapter.{MaybeStringWrapTypeAdapter, StringWrapTypeAdapter}

opaque type JSON = String

case class JsonFlavor(
  override val defaultHint:        String                           = "_hint",
  override val permissivesOk:      Boolean                          = false,
  override val customAdapters:     List[TypeAdapterFactory]         = List.empty[TypeAdapterFactory],
  override val hintMap:            Map[String, String]              = Map.empty[String, String],
  override val hintValueModifiers: Map[String, HintValueModifier]   = Map.empty[String, HintValueModifier],
  override val typeValueModifier:  HintValueModifier                = DefaultHintModifier,
  override val parseOrElseMap:     Map[Class[_], RType]             = Map.empty[Class[_], RType],
  override val enumsAsInt:         Boolean                          = false
) extends JackFlavor[JSON] {

  def _read[T](input: JSON, typeAdapter: TypeAdapter[T]): T =
    val parser = JsonParser(input, this)
    typeAdapter.read(parser).asInstanceOf[T]

  def _render[T](t: T, typeAdapter: TypeAdapter[T]): JSON =
    val sb = StringBuilder[JSON]()
    typeAdapter.write(t, writer, sb)
    sb.result()

  def parse(input: JSON): Parser = JsonParser(input, this)

  private val writer = JsonWriter() 

  override val stringifyMapKeys: Boolean = true

  def allowPermissivePrimitives(): JackFlavor[JSON] =
    this.copy(permissivesOk = true)
  def enumsAsInts(): JackFlavor[JSON] = this.copy(enumsAsInt = true)
  def parseOrElse(poe: (RType, RType)*): JackFlavor[JSON] =
    this.copy(parseOrElseMap = this.parseOrElseMap ++ poe.map{(p,oe) => p.infoClass->oe})
  def withAdapters(ta: TypeAdapterFactory*): JackFlavor[JSON] =
    this.copy(customAdapters = this.customAdapters ++ ta.toList)
  def withDefaultHint(hint: String): JackFlavor[JSON] =
    this.copy(defaultHint = hint)
  def withHints(h: (RType, String)*): JackFlavor[JSON] =
    this.copy(hintMap = this.hintMap ++ h.map{(rt,hint) => rt.name->hint})
  def withHintModifiers(hm: (RType, HintValueModifier)*): JackFlavor[JSON] =
    this.copy(hintValueModifiers = this.hintValueModifiers ++ hm.map{(rt,hintM) => rt.name->hintM})
  def withTypeValueModifier(tm: HintValueModifier): JackFlavor[JSON] =
    this.copy(typeValueModifier = tm)

  def stringWrapTypeAdapterFactory[T](
      wrappedTypeAdapter: TypeAdapter[T],
      emptyStringOk:      Boolean        = true
  ): TypeAdapter[T] =
    StringWrapTypeAdapter(wrappedTypeAdapter, emptyStringOk)

  def maybeStringWrapTypeAdapterFactory[T](
      wrappedTypeAdapter: TypeAdapter[T],
      emptyStringOk: Boolean = true
  ): TypeAdapter[T] =
    MaybeStringWrapTypeAdapter(this, wrappedTypeAdapter, emptyStringOk)
}
