package co.blocke.scalajack
package yaml

import model._
import co.blocke.scala_reflection.RType
import typeadapter.MaybeStringWrapTypeAdapter

import scala.collection.mutable
import org.snakeyaml.engine.v2.api.DumpSettings
import org.snakeyaml.engine.v2.api.lowlevel.{Present, Serialize}


opaque type YAML = String

case class YamlFlavor(
    override val defaultHint:        String                         = "_hint",
    override val permissivesOk:      Boolean                        = false,
    override val customAdapters:     List[TypeAdapterFactory]       = List.empty[TypeAdapterFactory],
    override val hintMap:            Map[String, String]            = Map.empty[String, String],
    override val hintValueModifiers: Map[String, HintValueModifier] = Map.empty[String, HintValueModifier],
    override val typeValueModifier:  HintValueModifier              = DefaultHintModifier,
    override val parseOrElseMap:     Map[Class[_], RType]           = Map.empty[Class[_], RType],
    override val enumsAsInt:         Boolean                        = false
) extends JackFlavor[YAML] {

  private val settings   = DumpSettings.builder().setIndent(2).build()
  private val serializer = new Serialize(settings)
  private val presenter  = new Present(settings)

  def _read[T](input: YAML, typeAdapter: TypeAdapter[T]): T =
    val parser = YamlParser(input, this)
    typeAdapter.read(parser).asInstanceOf[T]

  def _render[T](t: T, typeAdapter: TypeAdapter[T]): YAML =
    val sb = YamlBuilder()
    typeAdapter.write(t, writer, sb)
    presenter.emitToString(serializer.serializeOne(sb.result()).iterator).asInstanceOf[YAML]

  def parse(input: YAML): Parser = YamlParser(input, this)

  private val writer = YamlWriter() //(this)

  override val stringifyMapKeys: Boolean = true

  def allowPermissivePrimitives(): JackFlavor[YAML] =
    throw new ScalaJackError("Not available for YAML encoding")
  def enumsAsInts(): JackFlavor[YAML] = this.copy(enumsAsInt = true)
  def parseOrElse(poe: (RType, RType)*): JackFlavor[YAML] =
    this.copy(parseOrElseMap = this.parseOrElseMap ++ poe.map{(p,oe) => p.infoClass->oe})
  def withAdapters(ta: TypeAdapterFactory*): JackFlavor[YAML] =
    this.copy(customAdapters = this.customAdapters ++ ta.toList)
  def withDefaultHint(hint: String): JackFlavor[YAML] =
    this.copy(defaultHint = hint)
  def withHints(h: (RType, String)*): JackFlavor[YAML] =
    this.copy(hintMap = this.hintMap ++ h.map{(rt,hint) => rt.name->hint})
  def withHintModifiers(hm: (RType, HintValueModifier)*): JackFlavor[YAML] =
    this.copy(hintValueModifiers = this.hintValueModifiers ++ hm.map{(rt,hintM) => rt.name->hintM})
  def withTypeValueModifier(tm: HintValueModifier): JackFlavor[YAML] =
    this.copy(typeValueModifier = tm)

  def stringWrapTypeAdapterFactory[T](
      wrappedTypeAdapter: TypeAdapter[T],
      emptyStringOk: Boolean = true
  ): TypeAdapter[T] = wrappedTypeAdapter // no need to stringify--YAML handles complex key values!

  def maybeStringWrapTypeAdapterFactory[T](
    wrappedTypeAdapter: TypeAdapter[T],
    emptyStringOk: Boolean = true
  ): TypeAdapter[T] = wrappedTypeAdapter // no need to stringify--YAML handles complex key values!

  // OK... for YAML I've done a bad thing here and lied.  The JackFlavor trait requires Builder[WIRE,WIRE],
  // which is Builder[YAML,YAML].  But the internals of snakeyaml require me to use a Builder[Node,Node].
  // Correctly untangling this would be a huge amount of work...and unnecessary.  Both producer and consumer
  // of this Builder know and expect Builder[Node,Node], so it was expedient to just lie to the trait and
  // say this is a Builder[YAML,YAML] to make the compiler happy.   Sue me.
  override def getBuilder: mutable.Builder[YAML, YAML] = YamlBuilder().asInstanceOf[mutable.Builder[YAML, YAML]]
}