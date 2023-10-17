package co.blocke.scalajack
package json4s

import model._
import typeadapter.MaybeStringWrapTypeAdapter
import org.json4s._

import scala.collection.mutable
import co.blocke.scala_reflection.RType


case class Json4sFlavor(
    override val defaultHint:        String                         = "_hint",
    override val permissivesOk:      Boolean                        = false,
    override val customAdapters:     List[TypeAdapterFactory]       = List.empty[TypeAdapterFactory],
    override val hintMap:            Map[String, String]            = Map.empty[String, String],
    override val hintValueModifiers: Map[String, HintValueModifier] = Map.empty[String, HintValueModifier],
    override val typeValueModifier:  HintValueModifier              = DefaultHintModifier,
    override val parseOrElseMap:     Map[Class[_], RType]           = Map.empty[Class[_], RType],
    override val enumsAsInt:         Boolean                        = false
) extends JackFlavor[JValue] {

  def _read[T](input: JValue, typeAdapter: TypeAdapter[T]): T =
    val parser = Json4sParser(input, this)
    typeAdapter.read(parser).asInstanceOf[T]

  def _render[T](t: T, typeAdapter: TypeAdapter[T]): JValue =
    val sb = json4s.JValueBuilder()
    typeAdapter.write(t, writer, sb)
    sb.result()

  def parse(input: JValue): Parser = Json4sParser(input, this)

  private val writer = Json4sWriter()

  override val stringifyMapKeys: Boolean = true

  def allowPermissivePrimitives(): JackFlavor[JValue] =
    throw new ScalaJackError("Permissive primitives not supported for Json4s")
  def enumsAsInts(): JackFlavor[JValue] = this.copy(enumsAsInt = true)
  def parseOrElse(poe: (RType, RType)*): JackFlavor[JValue] =
    this.copy(parseOrElseMap = this.parseOrElseMap ++ poe.map{(p,oe) => p.infoClass->oe})
  def withAdapters(ta: TypeAdapterFactory*): JackFlavor[JValue] =
    this.copy(customAdapters = this.customAdapters ++ ta.toList)
  def withDefaultHint(hint: String): JackFlavor[JValue] =
    this.copy(defaultHint = hint)
  def withHints(h: (RType, String)*): JackFlavor[JValue] =
    this.copy(hintMap = this.hintMap ++ h.map{(rt,hint) => rt.name->hint})
  def withHintModifiers(hm: (RType, HintValueModifier)*): JackFlavor[JValue] =
    this.copy(hintValueModifiers = this.hintValueModifiers ++ hm.map{(rt,hintM) => rt.name->hintM})
  def withTypeValueModifier(tm: HintValueModifier): JackFlavor[JValue] =
    this.copy(typeValueModifier = tm)

  def stringWrapTypeAdapterFactory[T](
      wrappedTypeAdapter: TypeAdapter[T],
      emptyStringOk:      Boolean        = true
  ): TypeAdapter[T] =
    StringWrapTypeAdapter(wrappedTypeAdapter)

  override def getBuilder: mutable.Builder[JValue, JValue] =
    json4s.JValueBuilder()

  def maybeStringWrapTypeAdapterFactory[T](
    wrappedTypeAdapter: TypeAdapter[T],
    emptyStringOk: Boolean = true
  ): TypeAdapter[T] =
    MaybeStringWrapTypeAdapter(this, wrappedTypeAdapter, emptyStringOk)
}
