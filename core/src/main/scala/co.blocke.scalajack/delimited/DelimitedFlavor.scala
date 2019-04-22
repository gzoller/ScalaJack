package co.blocke.scalajack
package delimited

import model._
import compat.StringBuilder
import java.util.ArrayList
import java.lang.{ UnsupportedOperationException => UOE }

import co.blocke.scalajack.json.JsonStringWrapTypeAdapter
import util.Path

case class DelimitedFlavor(
    delimiter:                       Char                         = ',',
    override val defaultHint:        String                       = "_hint",
    override val permissivesOk:      Boolean                      = false,
    override val customAdapters:     List[TypeAdapterFactory]     = List.empty[TypeAdapterFactory],
    override val hintMap:            Map[Type, String]            = Map.empty[Type, String],
    override val hintValueModifiers: Map[Type, HintValueModifier] = Map.empty[Type, HintValueModifier],
    override val typeValueModifier:  Option[HintValueModifier]    = None,
    override val parseOrElseMap:     Map[Type, Type]              = Map.empty[Type, Type],
    override val enumsAsInt:         Boolean                      = false
) extends JackFlavor[String] {

  // $COVERAGE-OFF$Never used for delimited format -- no maps supported so no map keys or string wrapping
  def stringWrapTypeAdapterFactory[T](wrappedTypeAdapter: TypeAdapter[T]): TypeAdapter[T] = new JsonStringWrapTypeAdapter(wrappedTypeAdapter)
  // $COVERAGE-ON$

  def withAdapters(ta: TypeAdapterFactory*): JackFlavor[String] = throw new UOE("Not available for CSV encoding")
  def withDefaultHint(hint: String): JackFlavor[String] = throw new UOE("Not available for CSV encoding")
  def withHints(h: (Type, String)*): JackFlavor[String] = throw new UOE("Not available for CSV encoding")
  def withHintModifiers(hm: (Type, HintValueModifier)*): JackFlavor[String] = throw new UOE("Not available for CSV encoding")
  def withTypeValueModifier(tm: HintValueModifier): JackFlavor[String] = throw new UOE("Not available for CSV encoding")
  def parseOrElse(poe: (Type, Type)*): JackFlavor[String] = this.copy(parseOrElseMap = this.parseOrElseMap ++ poe)
  def allowPermissivePrimitives(): JackFlavor[String] = throw new UOE("Not available for CSV encoding")
  def enumsAsInts(): JackFlavor[String] = this.copy(enumsAsInt = true)

  protected override def bakeContext(): Context =
    new Context(Seq(
      DelimitedOptionTypeAdapterFactory,
      DelimitedEitherTypeAdapterFactory,
      DelimitedEnumerationTypeAdapterFactory) ++: super.bakeContext().factories)

  private val writer = DelimitedWriter(delimiter, this)

  def parse(wire: String): Reader[String] = DelimitedReader(this, wire, DelimitedTokenizer(delimiter).tokenize(wire).asInstanceOf[ArrayList[DelimitedToken]])

  override def read[T](wire: String)(implicit tt: TypeTag[T]): T = {
    val p = parse(wire)
    context.typeAdapter(tt.tpe.dealias).read(Path.Root, p).asInstanceOf[T]
  }

  def render[T](t: T)(implicit tt: TypeTag[T]): String = {
    val sb = StringBuilder()
    context.typeAdapter(tt.tpe.dealias).asInstanceOf[TypeAdapter[T]].write(t, writer, sb, false)
    sb.result()
  }
}

