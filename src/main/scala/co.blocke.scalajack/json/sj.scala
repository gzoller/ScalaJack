package co.blocke.scalajack
package json

import scala.annotation.*

trait sj[A]:
  self =>
  final def decodeJson(str: CharSequence): Either[JsonParseError, A] =
    try Right(unsafeDecode(new JsonSource(str)))
    catch {
      case jpe: JsonParseError => Left(jpe)
    }

  final def map[B](f: A => B): sj[B] =
    new sj[B] {
      def unsafeDecode(in: JsonSource): B =
        f(self.unsafeDecode(in))
    }

  def unsafeDecode(in: JsonSource): A

//------------------------------------------------------------

object sj extends DecoderLowPriority1:

  def apply[A](implicit a: sj[A]): sj[A] = a

  // Primitive support...
  implicit val string: sj[String] = new sj[String] {
    def unsafeDecode(in: JsonSource): String =
      JsonParser.parseString(in).toString
  }

  implicit val boolean: sj[Boolean] = new sj[Boolean] {
    def unsafeDecode(in: JsonSource): Boolean =
      JsonParser.parseBoolean(in)
  }

  implicit val int: sj[Int] = number(JsonParser.parseInt, _.intValueExact())
  private def number[A](
      f: (JsonSource) => A,
      fromBigDecimal: java.math.BigDecimal => A
  ): sj[A] =
    new sj[A] {
      def unsafeDecode(in: JsonSource): A =
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

  private[json] def builder[A, T[_]](
      in: JsonSource,
      elemDecoder: sj[A],
      builder: scala.collection.mutable.Builder[A, T[A]]
  ): T[A] = {
    JsonParser.charWithWS(in, '[')
    var i: Int = 0
    if JsonParser.firstArrayElement(in) then
      while {
        {
          builder += elemDecoder.unsafeDecode(in)
          i += 1
        }; JsonParser.nextArrayElement(in)
      } do ()
    builder.result()
  }

//------------------------------------------------------------

private trait DecoderLowPriority1:
  this: sj.type =>

  def seq[A](elemDecoder: sj[A]): sj[Seq[A]] = new sj[Seq[A]] {
    def unsafeDecode(in: JsonSource): Seq[A] =
      builder(in, elemDecoder, scala.collection.immutable.Seq.newBuilder[A])
  }

//   implicit def list[A: JsonDecoder]: JsonDecoder[List[A]] = new JsonDecoder[List[A]] {
//     def unsafeDecode(in: JsonSource): List[A] =
//       builder(in, elemDecoder, new scala.collection.mutable.ListBuffer[A])
//   }
