package co.blocke.scalajack
package msgpack

import typeadapter.DerivedValueClassAdapter
import scala.reflect.runtime.universe.{ Type, TypeTag }
import org.msgpack.MessagePack

case class MsgPackFlavor(
    customAdapters: List[TypeAdapterFactory] = List.empty[TypeAdapterFactory],
    hintMap:        Map[Type, String]        = Map.empty[Type, String],
    hintModifiers:  Map[Type, HintModifier]  = Map.empty[Type, HintModifier],
    parseOrElseMap: Map[Type, Type]          = Map.empty[Type, Type],
    defaultHint:    String                   = "_hint",
    isCanonical:    Boolean                  = true
) extends ScalaJackLike[Array[Byte]] {

  def withAdapters(ta: TypeAdapterFactory*) = this.copy(customAdapters = this.customAdapters ++ ta.toList)
  def withHints(h: (Type, String)*) = this.copy(hintMap = this.hintMap ++ h)
  def withHintModifiers(hm: (Type, HintModifier)*) = this.copy(hintModifiers = this.hintModifiers ++ hm)
  def withDefaultHint(hint: String) = this.copy(defaultHint = hint)
  def parseOrElse(poe: (Type, Type)*) = this.copy(parseOrElseMap = this.parseOrElseMap ++ poe)
  def isCanonical(canonical: Boolean) = throw new UnsupportedOperationException("Not available for Dynamo formatting")

  private val msgPack = new MessagePack()

  override val context: Context = {
    val ctx = bakeContext()
    ctx.copy(factories = customAdapters ::: DerivedValueClassAdapter :: MsgPackBigDecimalTypeAdapter :: MsgPackBigIntTypeAdapter :: MsgPackCaseClassTypeAdapter :: MsgPackCanBuildFromTypeAdapter :: MsgPackPolymorphicTypeAdapterFactory(defaultHint) :: ctx.factories)
  }

  def read[T](bytes: Array[Byte])(implicit valueTypeTag: TypeTag[T]): T = {
    val tokenizer = new MsgPackTokenizer()
    val reader = tokenizer.tokenize(bytes)
    context.typeAdapterOf[T].read(reader)
  }

  def render[T](value: T)(implicit valueTypeTag: TypeTag[T]): Array[Byte] = {
    val out = new java.io.ByteArrayOutputStream()
    val writer = MsgPackWriter(msgPack, out)
    context.typeAdapterOf[T].write(value, writer)
    out.toByteArray
  }
}
