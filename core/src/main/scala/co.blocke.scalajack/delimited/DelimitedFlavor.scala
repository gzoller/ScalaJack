package co.blocke.scalajack
package delimited

import model._
import compat.StringBuilder
import typeadapter.CanBuildFromTypeAdapterFactory
import java.util.ArrayList
import java.lang.{ UnsupportedOperationException => UOE }

import util.Path

object DelimitedFlavor extends FlavorMaker {
  type WIRE = String
  def make(): JackFlavor[String] = new DelimitedFlavorImpl()
}
case class DelimitedFlavor(delim: Char) extends FlavorMaker {
  type WIRE = String
  def make(): JackFlavor[String] = new DelimitedFlavorImpl(delimiter = delim)
}

case class DelimitedFlavorImpl(
    override val defaultHint:        String                       = "_hint",
    override val permissivesOk:      Boolean                      = false,
    override val customAdapters:     List[TypeAdapterFactory]     = List.empty[TypeAdapterFactory],
    override val hintMap:            Map[Type, String]            = Map.empty[Type, String],
    override val hintValueModifiers: Map[Type, HintValueModifier] = Map.empty[Type, HintValueModifier],
    override val typeValueModifier:  Option[HintValueModifier]    = None,
    override val parseOrElseMap:     Map[Type, Type]              = Map.empty[Type, Type],
    override val enumsAsInt:         Boolean                      = false,
    delimiter:                       Char                         = ',') extends JackFlavor[String] {

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
      DelimitedEnumerationTypeAdapterFactory,
      CanBuildFromTypeAdapterFactory(enumsAsInt)) ++: super.bakeContext().factories)

  private val writer = DelimitedWriter(delimiter, this)

  def parse(wire: String): Reader[String] = DelimitedReader(this, wire, DelimitedTokenizer(delimiter).tokenize(wire).asInstanceOf[ArrayList[DelimitedToken]])

  override def read[T](wire: String)(implicit tt: TypeTag[T]): T = {
    val p = parse(wire)
    context.typeAdapter(tt.tpe).read(Path.Root, p).asInstanceOf[T]
  }

  def render[T](t: T)(implicit tt: TypeTag[T]): String = {
    val sb = StringBuilder()
    context.typeAdapter(tt.tpe).asInstanceOf[TypeAdapter[T]].write(t, writer, sb, false)
    sb.result()
  }
}

