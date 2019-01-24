package co.blocke.scalajack
package json

import model._
import scala.collection.mutable.Builder

case class JsonFlavor[N](
    secondLookParsing: Boolean = false
)(implicit tt: TypeTag[N]) extends JackFlavor[N, String] {

  //  val tokenizer = JsonTokenizer()

  //  def forType[N2](implicit tt: TypeTag[N2]): JackFlavor[N2, String] = JsonFlavor[N2]()
  //  val nativeTypeAdapter: TypeAdapter[N] = context.typeAdapterOf[N]

  def withSecondLookParsing(): JackFlavor[N, String] = this.copy(secondLookParsing = true)

  private val stringTypeAdapter = context.typeAdapterOf[String]

  def parse(wire: String): Transceiver[String] = JsonTransciever(wire, context, stringTypeAdapter, secondLookParsing)

  def render[T](t: T)(implicit tt: TypeTag[T]): String = {
    val sb = new StringBuilder().asInstanceOf[Builder[Any, String]]
    context.typeAdapter(tt.tpe).asInstanceOf[TypeAdapter[T]].write(t, JsonTransciever("", context, stringTypeAdapter, secondLookParsing), sb)
    sb.result()
  }
}

case class JsonTransciever(
    json:              String,
    context:           Context,
    stringTypeAdapter: TypeAdapter[String],
    secondLookParsing: Boolean) extends Transceiver[String] with JsonReader with JsonWriter {
  val tokenizer: Tokenizer[String] = JsonTokenizer()
}
