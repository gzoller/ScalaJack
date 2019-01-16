package co.blocke.scalajack
package json

import model._
import scala.collection.mutable.Builder

case class JsonFlavor[N]()(implicit tt: TypeTag[N]) extends JackFlavor[N, String] {

  //  val tokenizer = JsonTokenizer()

  //  def forType[N2](implicit tt: TypeTag[N2]): JackFlavor[N2, String] = JsonFlavor[N2]()
  //  val nativeTypeAdapter: TypeAdapter[N] = context.typeAdapterOf[N]

  def parse(wire: String): Transceiver[String] = JsonTransciever(wire, context)

  def render[T](t: T)(implicit tt: TypeTag[T]): String = {
    val sb = new StringBuilder().asInstanceOf[Builder[Any, String]]
    context.typeAdapter(tt.tpe).asInstanceOf[TypeAdapter[T]].write(t, JsonTransciever("", context), sb)
    sb.result()
  }
}

case class JsonTransciever(json: String, context: Context) extends Transceiver[String] with JsonReader with JsonWriter {
  val tokenizer: Tokenizer[String] = JsonTokenizer()
}
