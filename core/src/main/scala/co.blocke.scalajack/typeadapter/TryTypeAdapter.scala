package co.blocke.scalajack
package typeadapter

import co.blocke.scalajack.json.Tokenizer

import scala.util.{ Failure, Success, Try }
import scala.reflect.runtime.universe.{ Type, typeOf }

trait Drink
case class OrangeJuice(pulp: Boolean) extends Drink
case class Meal(drink: Try[Drink])

object TrySandbox extends App {

  val scalaJack = ScalaJack()

  val json = """{"drink": "bill"}"""

  val meal = scalaJack.read[Meal](json)
  println(meal)
  println(scalaJack.render[Meal](meal))

}

object TryTypeAdapter extends TypeAdapterFactory {

  override def typeAdapter(tpe: Type, context: Context): Option[TypeAdapter[_]] =
    if (tpe <:< typeOf[Try[_]]) {
      val valueType = tpe.typeArgs.head
      val valueTypeAdapter = context.typeAdapter(valueType)

      Some(TryTypeAdapter(valueTypeAdapter))
    } else {
      None
    }

}

case class TryTypeAdapter[T](valueTypeAdapter: TypeAdapter[T]) extends TypeAdapter[Try[T]] {

  override def read(reader: Reader): Try[T] = {
    val originalPosition = reader.position

    val attempt = Try { valueTypeAdapter.read(reader) }

    attempt match {
      case self @ Success(_) ⇒
        self

      case Failure(cause) ⇒
        reader.position = originalPosition
        reader.skipValue()

        val unreadableJsonOffset = reader.tokenOffsetAt(originalPosition + 1)
        val unreadableJsonLength = reader.tokenOffsetAt(reader.position + 1) - unreadableJsonOffset

        val exception = new UnreadableException(reader.source, unreadableJsonOffset, unreadableJsonLength, cause)

        Failure(exception)
    }
  }

  override def write(value: Try[T], writer: Writer): Unit =
    value match {
      case Success(v) ⇒
        valueTypeAdapter.write(v, writer)

      case Failure(e: UnreadableException) ⇒
        e.write(writer)

      case Failure(e) ⇒
        throw e
    }

}
