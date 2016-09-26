package co.blocke.scalajack
package json

import scala.collection.mutable
import scala.reflect.runtime.universe.{ Type, TypeTag }
import scala.reflect.runtime.currentMirror

case class JsonFlavor() extends JackFlavor[String] with ScalaJackLike[String] {

  def read[T](json: String)(implicit valueTypeTag: TypeTag[T]): T = {
    val tokenizer = new Tokenizer

    val source = json.toCharArray
    val reader = tokenizer.tokenize(source, 0, source.length)

    val typeAdapter = context.typeAdapterOf[T]
    typeAdapter.read(reader)
  }

  def render[T](value: T)(implicit valueTypeTag: TypeTag[T]): String = {
    val writer = new StringJsonWriter()
    val typeAdapter = context.typeAdapterOf[T]
    typeAdapter.write(value, writer)
    val jsonString = writer.jsonString
    jsonString
  }
}
