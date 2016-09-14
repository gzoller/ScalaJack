package co.blocke.scalajack.json
package typeadapter

import scala.collection.GenTraversableOnce
import scala.collection.generic.CanBuildFrom
import scala.language.existentials

case class CanBuildFromTypeAdapter[Elem, To >: Null <: GenTraversableOnce[Elem]](
    canBuildFrom:       CanBuildFrom[_, Elem, To],
    elementTypeAdapter: TypeAdapter[Elem]
) extends TypeAdapter[To] {

  override def read(reader: Reader): To =
    reader.peek match {
      case TokenType.Null ⇒
        reader.readNull()

      case TokenType.BeginArray ⇒
        val builder = canBuildFrom()

        reader.beginArray()

        while (reader.hasMoreElements) {
          val element = elementTypeAdapter.read(reader)
          builder += element
        }

        reader.endArray()

        builder.result()
    }

  override def write(value: To, writer: Writer): Unit =
    if (value == null) {
      writer.writeNull()
    } else {
      writer.beginArray()

      for (element ← value) {
        elementTypeAdapter.write(element, writer)
      }

      writer.endArray()
    }

}
