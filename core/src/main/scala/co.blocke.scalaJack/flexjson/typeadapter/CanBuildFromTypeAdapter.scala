package co.blocke.scalajack.flexjson.typeadapter

import co.blocke.scalajack.flexjson.{ Reader, TokenType, TypeAdapter, Writer }

import scala.collection.GenTraversableOnce
import scala.collection.generic.CanBuildFrom
import scala.language.existentials

case class CanBuildFromTypeAdapter[Elem, To <: GenTraversableOnce[Elem]](
    canBuildFrom:       CanBuildFrom[_, Elem, To],
    elementTypeAdapter: TypeAdapter[Elem]
) extends TypeAdapter[To] {

  override def read(reader: Reader): To = {
    val builder = canBuildFrom()

    if (reader.peek == TokenType.Null) {
      reader.readNull().asInstanceOf[To]
    } else {
      reader.beginArray()

      while (reader.hasMoreElements) {
        val element = elementTypeAdapter.read(reader)
        builder += element
      }

      reader.endArray()

      builder.result()
    }
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
