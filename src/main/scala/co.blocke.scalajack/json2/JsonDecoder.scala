package co.blocke.scalajack
package json2

import json.JsonParseError
import scala.annotation._

trait JsonDecoder[A]:
  final def decodeJson(str: CharSequence): Either[JsonParseError, A] =
    try Right(unsafeDecode(new JsonReader(str)))
    catch {
      case jpe: JsonParseError => Left(jpe)
    }

  def unsafeDecode(in: JsonReader): A

//------------------------------------------------------------

object JsonDecoder extends DecoderLowPriority1:

  def apply[A](implicit a: JsonDecoder[A]): JsonDecoder[A] = a

  // Primitive support...
  implicit val string: JsonDecoder[String] = new JsonDecoder[String] { 
    def unsafeDecode(in: JsonReader): String =
      JsonParser.parseString(in).toString
  }

  implicit val boolean: JsonDecoder[Boolean] = new JsonDecoder[Boolean] {
    def unsafeDecode(in: JsonReader): Boolean =
      JsonParser.parseBoolean(in)
  }

  implicit val int: JsonDecoder[Int]                         = number(JsonParser.parseInt, _.intValueExact())
  private def number[A](
    f: (JsonReader) => A,
    fromBigDecimal: java.math.BigDecimal => A
  ): JsonDecoder[A] =
    new JsonDecoder[A] {
      def unsafeDecode(in: JsonReader): A =
        (in.readSkipWhitespace(): @switch) match {
          case '"' =>
            val i = f(in)
            JsonParser.char(in, '"')
            i
          case _ =>
            in.retract()
            f(in)
        }
    }

  private[json2] def builder[A, T[_]](
    in: JsonReader,
    builder: scala.collection.mutable.Builder[A, T[A]]
  )(implicit A: JsonDecoder[A]): T[A] = {
    JsonParser.charWithWS(in, '[')
    var i: Int = 0
    if (JsonParser.firstArrayElement(in)) while ({
      {
        builder += A.unsafeDecode(in)
        i += 1
      }; JsonParser.nextArrayElement(in)
    }) ()
    builder.result()
  }

//------------------------------------------------------------

private trait DecoderLowPriority1:
  this: JsonDecoder.type =>

  // TODO: Experiment with other Seq *NOT* explicitly provided as separate "implicit def"s.  See if this will
  // convert them to the correct subtype.
  implicit def seq[A: JsonDecoder]: JsonDecoder[Seq[A]] = new JsonDecoder[Seq[A]] {
    def unsafeDecode(in: JsonReader): Seq[A] =
      builder(in, scala.collection.immutable.Seq.newBuilder[A])
  }

  implicit def list[A: JsonDecoder]: JsonDecoder[List[A]] = new JsonDecoder[List[A]] {
    def unsafeDecode(in: JsonReader): List[A] =
      builder(in, new scala.collection.mutable.ListBuffer[A])
  }
