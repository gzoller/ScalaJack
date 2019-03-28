package co.blocke.scalajack
package json

import model._
import compat.StringBuilder

import java.util.ArrayList

object JsonFlavorMaker extends FlavorMaker {
  type WIRE = String
  def make(): JackFlavor[String] = new JsonFlavor()
}

case class JsonFlavor(
    override val defaultHint:        String                       = "_hint",
    override val permissivesOk:      Boolean                      = false,
    override val customAdapters:     List[TypeAdapterFactory]     = List.empty[TypeAdapterFactory],
    override val hintMap:            Map[Type, String]            = Map.empty[Type, String],
    override val hintValueModifiers: Map[Type, HintValueModifier] = Map.empty[Type, HintValueModifier],
    override val typeValueModifier:  Option[HintValueModifier]    = None,
    override val parseOrElseMap:     Map[Type, Type]              = Map.empty[Type, Type],
    override val enumsAsInt:         Boolean                      = false) extends JackFlavor[String] {

  override val stringifyMapKeys: Boolean = true

  def withAdapters(ta: TypeAdapterFactory*): JackFlavor[String] = this.copy(customAdapters = this.customAdapters ++ ta.toList)
  def withDefaultHint(hint: String): JackFlavor[String] = this.copy(defaultHint = hint)
  def withHints(h: (Type, String)*): JackFlavor[String] = this.copy(hintMap = this.hintMap ++ h)
  def withHintModifiers(hm: (Type, HintValueModifier)*): JackFlavor[String] = this.copy(hintValueModifiers = this.hintValueModifiers ++ hm)
  def withTypeValueModifier(tm: HintValueModifier): JackFlavor[String] = this.copy(typeValueModifier = Some(tm))
  def parseOrElse(poe: (Type, Type)*): JackFlavor[String] = this.copy(parseOrElseMap = this.parseOrElseMap ++ poe)
  def allowPermissivePrimitives(): JackFlavor[String] = this.copy(permissivesOk = true)
  def enumsAsInts(): JackFlavor[String] = this.copy(enumsAsInt = true)

  protected override def bakeContext(): Context =
    new Context(JsonCanBuildFromTypeAdapterFactory(enumsAsInt) +: super.bakeContext().factories)

  private val writer = JsonWriter(this)

  def parse(wire: String): Reader[String] = JsonReader(this, wire, JsonTokenizer().tokenize(wire).asInstanceOf[ArrayList[JsonToken]]) //JsonTransciever(wire, context, stringTypeAdapter, this)

  def render[T](t: T)(implicit tt: TypeTag[T]): String = {
    val sb = StringBuilder()
    context.typeAdapter(tt.tpe).asInstanceOf[TypeAdapter[T]].write(t, writer, sb, false)
    sb.result()
  }
}

