package co.blocke.scalajack
package yaml

import model._
import typeadapter.AnyMapKeyTypeAdapter

import scala.reflect.runtime.universe._
import scala.collection.mutable
import org.snakeyaml.engine.v2.api.DumpSettings
import org.snakeyaml.engine.v2.api.lowlevel.{Present, Serialize}
import org.snakeyaml.engine.v2.nodes.Node

/**
  * This class is a cut'n paste copy of JsonFlavor with some mods to lock in a type.  There's currently an
  * unfortunate amount of boilerplate copying between this class and JsonFlavor, but it facilitates a clean
  * user experience--smooth API for ScalaJack:  val fooSerializer = sj.forType[Foo];  fooSerializer.read(input)
  */
case class YamlFlavorFor[J](
    ta: TypeAdapter[J],
    override val defaultHint: String = "_hint",
    override val permissivesOk: Boolean = false,
    override val customAdapters: List[TypeAdapterFactory] = List.empty[TypeAdapterFactory],
    override val hintMap: Map[Type, String] = Map.empty[Type, String],
    override val hintValueModifiers: Map[Type, HintValueModifier] = Map.empty[Type, HintValueModifier],
    override val typeValueModifier: HintValueModifier = DefaultHintModifier,
    override val parseOrElseMap: Map[Type, Type] = Map.empty[Type, Type],
    override val enumsAsInt: Boolean = false
) extends JackFlavorFor[YAML, J] {

  private val settings   = DumpSettings.builder().setIndent(2).build()
  private val serializer = new Serialize(settings)
  private val presenter  = new Present(settings)

  def read[T](input: YAML)(implicit tt: TypeTag[T]): T = {
    val parser = YamlParser(input, this)
    taCache.typeAdapter(tt.tpe.dealias).read(parser).asInstanceOf[T]
  }

  def read(input: YAML): J = ta.read(YamlParser(input, this))
  def render(t: J): YAML = {
    val sb = YamlBuilder()
    ta.write(t, writer, sb)
    presenter.emitToString(serializer.serializeOne(sb.result()).iterator)
  }

  def forType[U](implicit tu: TypeTag[U]): JackFlavorFor[YAML, U] =
    this
      .copy(ta = taCache.typeAdapter(tu.tpe.dealias))
      .asInstanceOf[JackFlavorFor[YAML, U]]

  def render[T](t: T)(implicit tt: TypeTag[T]): YAML = {
    val sb = YamlBuilder()
    taCache
      .typeAdapter(tt.tpe.dealias)
      .asInstanceOf[TypeAdapter[T]]
      .write(t, writer, sb)
    presenter.emitToString(serializer.serializeOne(sb.result()).iterator)
  }

  // $COVERAGE-OFF$All this is carbon-copy from JsonFlavor, which has test coverage.
  def parse(input: YAML): Parser = YamlParser(input, this)

  private val writer = YamlWriter()

  override val stringifyMapKeys: Boolean = true
  override lazy val anyMapKeyTypeAdapter: AnyMapKeyTypeAdapter =
    typeadapter.AnyMapKeyTypeAdapter(this, anyTypeAdapter)

  def allowPermissivePrimitives(): JackFlavor[String] =
    throw new ScalaJackError("Not available for YAML encoding")
  def enumsAsInts(): JackFlavor[YAML] = this.copy(enumsAsInt = true)
  def parseOrElse(poe: (Type, Type)*): JackFlavor[YAML] =
    this.copy(parseOrElseMap = this.parseOrElseMap ++ poe)
  def withAdapters(ta: TypeAdapterFactory*): JackFlavor[YAML] =
    this.copy(customAdapters = this.customAdapters ++ ta.toList)
  def withDefaultHint(hint: String): JackFlavor[YAML] =
    this.copy(defaultHint = hint)
  def withHints(h: (Type, String)*): JackFlavor[YAML] =
    this.copy(hintMap = this.hintMap ++ h)
  def withHintModifiers(hm: (Type, HintValueModifier)*): JackFlavor[YAML] =
    this.copy(hintValueModifiers = this.hintValueModifiers ++ hm)
  def withTypeValueModifier(tm: HintValueModifier): JackFlavor[YAML] =
    this.copy(typeValueModifier = tm)

  def stringWrapTypeAdapterFactory[T](
      wrappedTypeAdapter: TypeAdapter[T],
      emptyStringOk: Boolean = true
  )(implicit tt: TypeTag[T]): TypeAdapter[T] = wrappedTypeAdapter // no need to stringify--YAML handles complex key values!
  // $COVERAGE-ON$
}

case class YamlFlavor(
    override val defaultHint: String = "_hint",
    override val permissivesOk: Boolean = false,
    override val customAdapters: List[TypeAdapterFactory] = List.empty[TypeAdapterFactory],
    override val hintMap: Map[Type, String] = Map.empty[Type, String],
    override val hintValueModifiers: Map[Type, HintValueModifier] = Map.empty[Type, HintValueModifier],
    override val typeValueModifier: HintValueModifier = DefaultHintModifier,
    override val parseOrElseMap: Map[Type, Type] = Map.empty[Type, Type],
    override val enumsAsInt: Boolean = false
) extends JackFlavor[YAML] {

  private val settings   = DumpSettings.builder().setIndent(2).build()
  private val serializer = new Serialize(settings)
  private val presenter  = new Present(settings)

  def read[T](input: YAML)(implicit tt: TypeTag[T]): T = {
    val parser = YamlParser(input, this)
    taCache.typeAdapter(tt.tpe.dealias).read(parser).asInstanceOf[T]
  }

  def forType[U](implicit tu: TypeTag[U]): JackFlavorFor[YAML, U] =
    YamlFlavorFor(
      taCache.typeAdapter(tu.tpe.dealias).asInstanceOf[TypeAdapter[U]],
      defaultHint,
      permissivesOk,
      customAdapters,
      hintMap,
      hintValueModifiers,
      typeValueModifier,
      parseOrElseMap,
      enumsAsInt
    )

  def render[T](t: T)(implicit tt: TypeTag[T]): YAML = {
    val sb = YamlBuilder()
    taCache
      .typeAdapter(tt.tpe.dealias)
      .asInstanceOf[TypeAdapter[T]]
      .write(t, writer, sb)
    presenter.emitToString(serializer.serializeOne(sb.result()).iterator)
  }

  def parse(input: YAML): Parser = YamlParser(input, this)

  private val writer = YamlWriter() //(this)

  override val stringifyMapKeys: Boolean = true

  def allowPermissivePrimitives(): JackFlavor[String] =
    throw new ScalaJackError("Not available for YAML encoding")
  def enumsAsInts(): JackFlavor[YAML] = this.copy(enumsAsInt = true)
  def parseOrElse(poe: (Type, Type)*): JackFlavor[YAML] =
    this.copy(parseOrElseMap = this.parseOrElseMap ++ poe)
  def withAdapters(ta: TypeAdapterFactory*): JackFlavor[YAML] =
    this.copy(customAdapters = this.customAdapters ++ ta.toList)
  def withDefaultHint(hint: String): JackFlavor[YAML] =
    this.copy(defaultHint = hint)
  def withHints(h: (Type, String)*): JackFlavor[YAML] =
    this.copy(hintMap = this.hintMap ++ h)
  def withHintModifiers(hm: (Type, HintValueModifier)*): JackFlavor[YAML] =
    this.copy(hintValueModifiers = this.hintValueModifiers ++ hm)
  def withTypeValueModifier(tm: HintValueModifier): JackFlavor[YAML] =
    this.copy(typeValueModifier = tm)

  def stringWrapTypeAdapterFactory[T](
      wrappedTypeAdapter: TypeAdapter[T],
      emptyStringOk: Boolean = true
  )(implicit tt: TypeTag[T]): TypeAdapter[T] = wrappedTypeAdapter // no need to stringify--YAML handles complex key values!

  // OK... for YAML I've done a bad thing here and lied.  The JackFlavor trait requires Builder[WIRE,WIRE],
  // which is Builder[YAML,YAML].  But the internals of snakeyaml require me to use a Builder[Node,Node].
  // Correctly untangling this would be a huge amount of work...and unnecessary.  Both producer and consumer
  // of this Builder know and expect Builder[Node,Node], so it was expedient to just lie to the trait and
  // say this is a Builder[YAML,YAML] to make the compiler happy.   Sue me.
  override def getBuilder: mutable.Builder[YAML, YAML] = YamlBuilder().asInstanceOf[mutable.Builder[YAML, YAML]]
}
