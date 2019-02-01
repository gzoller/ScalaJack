package co.blocke.scalajack
package json

import model.{ JackFlavor, _ }

import scala.collection.mutable.Builder

object JsonFlavorMaker {
  def apply[N, WIRE](): () => JackFlavor[N, WIRE] = (() => {
    val jf: JackFlavor[N, WIRE] = JsonFlavor().asInstanceOf[JackFlavor[N, WIRE]]
    jf
  })
}

case class JsonFlavor[N](
    override val defaultHint:        String                       = "_hint",
    override val permissivesOk:      Boolean                      = false,
    override val customAdapters:     List[TypeAdapterFactory]     = List.empty[TypeAdapterFactory],
    override val hintMap:            Map[Type, String]            = Map.empty[Type, String],
    override val hintValueModifiers: Map[Type, HintValueModifier] = Map.empty[Type, HintValueModifier],
    override val parseOrElseMap:     Map[Type, Type]              = Map.empty[Type, Type],
    secondLookParsing:               Boolean                      = false)(implicit tt: TypeTag[N]) extends JackFlavor[N, String] {

  println("== NEW ==")
  //  val tokenizer = JsonTokenizer()

  //  def forType[N2](implicit tt: TypeTag[N2]): JackFlavor[N2, String] = JsonFlavor[N2]()
  //  val nativeTypeAdapter: TypeAdapter[N] = context.typeAdapterOf[N]

  override val stringifyMapKeys: Boolean = true

  def withAdapters(ta: TypeAdapterFactory*): JackFlavor[N, String] = this.copy(customAdapters = this.customAdapters ++ ta.toList)
  def withDefaultHint(hint: String): JackFlavor[N, String] = this.copy(defaultHint = hint)
  def withHints(h: (Type, String)*): JackFlavor[N, String] = this.copy(hintMap = this.hintMap ++ h)
  def withHintModifiers(hm: (Type, HintValueModifier)*): JackFlavor[N, String] = this.copy(hintValueModifiers = this.hintValueModifiers ++ hm)
  def withSecondLookParsing(): JackFlavor[N, String] = this.copy(secondLookParsing = true)
  def parseOrElse(poe: (Type, Type)*): JackFlavor[N, String] = this.copy(parseOrElseMap = this.parseOrElseMap ++ poe)
  def allowPermissivePrimitives(): JackFlavor[N, String] = this.copy(permissivesOk = true)

  protected override def bakeContext(): Context =
    new Context(JsonCanBuildFromTypeAdapterFactory +: super.bakeContext().factories)

  def parse(wire: String): Transceiver[String] = JsonTransciever(wire, context, stringTypeAdapter, this)

  def render[T](t: T)(implicit tt: TypeTag[T]): String = {
    val sb = new StringBuilder().asInstanceOf[Builder[Any, String]]
    context.typeAdapter(tt.tpe).asInstanceOf[TypeAdapter[T]].write(t, JsonTransciever("", context, stringTypeAdapter, this), sb, false)
    sb.result()
  }
}

case class JsonTransciever(
    json:              String,
    context:           Context,
    stringTypeAdapter: TypeAdapter[String],
    jackFlavor:        JackFlavor[_, String]) extends Transceiver[String] with JsonReader with JsonWriter {
  val tokenizer: Tokenizer[String] = JsonTokenizer()
}
