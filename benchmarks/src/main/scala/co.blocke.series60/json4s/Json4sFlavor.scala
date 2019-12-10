package co.blocke.series60
package json4s

import model._
import java.util.ArrayList

import co.blocke.series60.compat.JValueBuilder
import org.json4s._
import co.blocke.series60.util.Path

case class Json4sFlavor(
    override val defaultHint:        String                       = "_hint",
    override val permissivesOk:      Boolean                      = false,
    override val customAdapters:     List[TypeAdapterFactory]     = List.empty[TypeAdapterFactory],
    override val hintMap:            Map[Type, String]            = Map.empty[Type, String],
    override val hintValueModifiers: Map[Type, HintValueModifier] = Map.empty[Type, HintValueModifier],
    override val typeValueModifier:  Option[HintValueModifier]    = None,
    override val parseOrElseMap:     Map[Type, Type]              = Map.empty[Type, Type],
    override val enumsAsInt:         Boolean                      = false,
    delimiter:                       Char                         = ',') extends JackFlavor[JValue] {

  override val stringifyMapKeys: Boolean = true

  // $COVERAGE-OFF$Identical to JsonFlavor code==no need to double-test
  def withAdapters(ta: TypeAdapterFactory*): JackFlavor[JValue] = this.copy(customAdapters = this.customAdapters ++ ta.toList)
  def withDefaultHint(hint: String): JackFlavor[JValue] = this.copy(defaultHint = hint)
  def withHints(h: (Type, String)*): JackFlavor[JValue] = this.copy(hintMap = this.hintMap ++ h)
  def withHintModifiers(hm: (Type, HintValueModifier)*): JackFlavor[JValue] = this.copy(hintValueModifiers = this.hintValueModifiers ++ hm)
  def withTypeValueModifier(tm: HintValueModifier): JackFlavor[JValue] = this.copy(typeValueModifier = Some(tm))
  def parseOrElse(poe: (Type, Type)*): JackFlavor[JValue] = this.copy(parseOrElseMap = this.parseOrElseMap ++ poe)
  def allowPermissivePrimitives(): JackFlavor[JValue] = this.copy(permissivesOk = true)
  def enumsAsInts(): JackFlavor[JValue] = this.copy(enumsAsInt = true)
  // $COVERAGE-ON$

  def stringWrapTypeAdapterFactory[T](wrappedTypeAdapter: TypeAdapter[T]): TypeAdapter[T] = new StringWrapTypeAdapter(wrappedTypeAdapter)

  private val writer = Json4sWriter(this)

  def parse(wire: JValue): model.Reader[JValue] = Json4sReader(this, wire, JValueTokenizer().tokenize(wire).asInstanceOf[ArrayList[JValueToken]])

  override def read[T](wire: JValue)(implicit tt: TypeTag[T]): T = {
    val p = parse(wire)
    context.typeAdapter(tt.tpe.dealias).read(Path.Root, p).asInstanceOf[T]
  }

  def render[T](t: T)(implicit tt: TypeTag[T]): JValue = {
    val typeAdapter = context.typeAdapter(tt.tpe.dealias).asInstanceOf[TypeAdapter[T]]
    val builder = JValueBuilder()
    typeAdapter.write(t, writer, builder, false)
    builder.result()
  }
}

