package co.blocke.scalajack
package json

import model._
import scala.collection.mutable.Builder

case class JsonFlavor[N]()(implicit tt: TypeTag[N]) extends JackFlavor[N, String] {

  val tokenizer = JsonTokenizer()

  def forType[N2](implicit tt: TypeTag[N2]): JackFlavor[N2, String] = JsonFlavor[N2]()
  val nativeTypeAdapter: TypeAdapter[N] = context.typeAdapterOf[N]

  def parse(wire: String): Transceiver = JsonReaderWriter(wire, context)

  def write[T](t: T)(implicit tt: TypeTag[T]): String = {
    val sb = new StringBuilder().asInstanceOf[Builder[Any, String]]
    context.typeAdapter(tt.tpe).asInstanceOf[TypeAdapter[T]].write(t, JsonReaderWriter("", context))(sb)
    sb.result()
  }
}

trait JsonTransciever extends Transceiver with JsonReader with JsonWriter {
  type WIRE = String
}

case class JsonReaderWriter(json: String, context: Context, tokenizer: Tokenizer[String] = JsonTokenizer()) extends JsonTransciever {
  val stringTypeAdapter: TypeAdapter[String] = context.typeAdapterOf[String]
}
