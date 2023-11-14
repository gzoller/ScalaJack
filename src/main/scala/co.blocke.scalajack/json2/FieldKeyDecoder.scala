package co.blocke.scalajack
package json2

import json.JsonParseError

trait FieldKeyDecoder[+A] {
  self =>

  final def map[B](f: A => B): FieldKeyDecoder[B] =
    new FieldKeyDecoder[B] {
      def unsafeDecodeField(in: String): B = f(self.unsafeDecodeField(in))
    }

  final def mapOrFail[B](f: A => Either[String, B]): FieldKeyDecoder[B] =
    new FieldKeyDecoder[B] {
      def unsafeDecodeField(in: String): B =
        f(self.unsafeDecodeField(in)) match {
          case Left(err) => throw JsonParseError(err)
          case Right(b) => b
        }
    }

  def unsafeDecodeField(in: String): A
}

object FieldKeyDecoder {
  def apply[A](implicit a: FieldKeyDecoder[A]): FieldKeyDecoder[A] = a

  implicit val string: FieldKeyDecoder[String] = new FieldKeyDecoder[String] {
    def unsafeDecodeField(in: String): String = in
  }

  implicit val int: FieldKeyDecoder[Int] =
    FieldKeyDecoder[String].mapOrFail { str =>
      try {
        Right(str.toInt)
      } catch {
        case n: NumberFormatException => Left(s"Invalid Int: '$str': $n")
      }
    }

  implicit val long: FieldKeyDecoder[Long] =
    FieldKeyDecoder[String].mapOrFail { str =>
      try {
        Right(str.toLong)
      } catch {
        case n: NumberFormatException => Left(s"Invalid Long: '$str': $n")
      }
    }
}