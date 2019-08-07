package co.blocke.scalajack
package schema

import java.util

import model._
import json._
import typeadapter.CanBuildFromTypeAdapterFactory
import java.util.concurrent.ConcurrentHashMap

case class SchemaFlavor(
    override val defaultHint:        String                       = "_hint",
    override val permissivesOk:      Boolean                      = false,
    override val customAdapters:     List[TypeAdapterFactory]     = List.empty[TypeAdapterFactory],
    override val hintMap:            Map[Type, String]            = Map.empty[Type, String],
    override val hintValueModifiers: Map[Type, HintValueModifier] = Map.empty[Type, HintValueModifier],
    override val typeValueModifier:  Option[HintValueModifier]    = None,
    override val parseOrElseMap:     Map[Type, Type]              = Map.empty[Type, Type],
    override val enumsAsInt:         Boolean                      = false
) extends JackFlavor[String] {

  override val stringifyMapKeys: Boolean = true

  def stringWrapTypeAdapterFactory[T](
      wrappedTypeAdapter: TypeAdapter[T]
  ): TypeAdapter[T] = new JsonStringWrapTypeAdapter(wrappedTypeAdapter)

  def withAdapters(ta: TypeAdapterFactory*): JackFlavor[String] =
    this.copy(customAdapters = this.customAdapters ++ ta.toList)
  def withDefaultHint(hint: String): JackFlavor[String] =
    this.copy(defaultHint = hint)
  def withHints(h: (Type, String)*): JackFlavor[String] =
    this.copy(hintMap = this.hintMap ++ h)
  def withHintModifiers(hm: (Type, HintValueModifier)*): JackFlavor[String] =
    this.copy(hintValueModifiers = this.hintValueModifiers ++ hm)
  def withTypeValueModifier(tm: HintValueModifier): JackFlavor[String] =
    this.copy(typeValueModifier = Some(tm))
  def parseOrElse(poe: (Type, Type)*): JackFlavor[String] =
    this.copy(parseOrElseMap = this.parseOrElseMap ++ poe)
  def allowPermissivePrimitives(): JackFlavor[String] =
    this.copy(permissivesOk = true)
  def enumsAsInts(): JackFlavor[String] = this.copy(enumsAsInt = true)

  protected override def bakeContext(): Context =
    new Context(
      CanBuildFromTypeAdapterFactory(this, enumsAsInt) +: super
        .bakeContext()
        .factories
    )

  private lazy val wrappedJsonFlavor =
    JsonFlavor(defaultHint, permissivesOk, customAdapters, hintMap, hintValueModifiers, typeValueModifier, parseOrElseMap, enumsAsInt)

  private val schemaEntries = new ConcurrentHashMap[Type, JsonSchema]
  private val schemaEntryFactory = SchemaEntryFactory(context)

  def parse(wire: String): Reader[String] =
    JsonReader(
      this,
      wire,
      JsonTokenizer().tokenize(wire).asInstanceOf[util.ArrayList[JsonToken]]
    )

  def render[T](t: T)(implicit tt: TypeTag[T]): String = {
    val schema = schemaEntries.computeIfAbsent(tt.tpe, schemaEntryFactory)
    if (schema.isValid(t))
      wrappedJsonFlavor.render(t)
    else
      throw new Exception("Boom")
  }

  def schemaFor[T](implicit tt: TypeTag[T]): String = wrappedJsonFlavor.render(schemaEntries.computeIfAbsent(tt.tpe, schemaEntryFactory))
}
