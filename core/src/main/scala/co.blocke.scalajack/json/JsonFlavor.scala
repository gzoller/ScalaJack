package co.blocke.scalajack
package json

import model._

case class JsonFlavor[N]()(implicit tt: TypeTag[N]) extends JackFlavor[N, String] {

  val tokenizer = JsonTokenizer()

  def forType[N2](implicit tt: TypeTag[N2]): JackFlavor[N2, String] = JsonFlavor[N2]()
  val nativeTypeAdapter: TypeAdapter[N] = context.typeAdapterOf[N]

  def parse(wire: String): Reader = JsonReader(wire)
}
