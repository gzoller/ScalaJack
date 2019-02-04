package co.blocke.scalajack
package typeadapter

import util.Path
import model._

import scala.collection.mutable.Builder

/*
 O, the exquisite pain of mapping Option (None) to something in JSON!
 Our assumptions (which may be different from earlier versions of ScalaJack:

   * Normal: None is just missing.  Doesn't read or write anything at all.  Classic example is a class field value--it's
     just "missing" from the JSON and understood to be None.

   * Map key fields: Element is dropped from Map, like List element behavior

   * Map value fields:  Element is dropped from Map, like List element behavior

   * List elements:  Normal, i.e. read/write nothing.  None elements just disappear.  NOTE this does mean that a read/render
     cycle may not yield the same object, which is generally breaking a ScalaJack core behavior goal

   * Tuple elements: Place needs to be preserved in a Tuple, so None becomes JSON null.  Really hate this option, but JSON
     doesn't leave many choices here.
 */

object OptionTypeAdapterFactory extends TypeAdapterFactory {
  def typeAdapterOf[T](next: TypeAdapterFactory)(implicit context: Context, tt: TypeTag[T]): TypeAdapter[T] =
    if (tt.tpe <:< typeOf[Option[_]]) {
      val elementType :: Nil = tt.tpe.baseType(tt.tpe.typeSymbol).typeArgs
      OptionTypeAdapter(context.typeAdapter(elementType)).asInstanceOf[TypeAdapter[T]]
    } else
      next.typeAdapterOf[T]
}

case class OptionTypeAdapter[E](valueTypeAdapter: TypeAdapter[E], nullIsNone: Boolean = false)(implicit tt: TypeTag[E]) extends TypeAdapter[Option[E]] {

  override def defaultValue: Option[Option[E]] = Some(None)

  def valueIsStringish(): Boolean = valueTypeAdapter.isInstanceOf[Stringish]

  def read[WIRE](path: Path, reader: Transceiver[WIRE]): Option[E] =
    // We have to do some voodoo here and peek ahead for Null.  Some types, e.g. Int, aren't nullable,
    // but Option[Int] is nullable, so we can't trust the valueTypeAdapter to catch and handle null in
    // these cases.
    reader.peek() match {
      case TokenType.Null if nullIsNone =>
        reader.skip()
        None
      case TokenType.Null =>
        reader.skip()
        null
      case _ =>
        Some(valueTypeAdapter.read(path, reader))
    }

  def write[WIRE](t: Option[E], writer: Transceiver[WIRE], out: Builder[Any, WIRE], isMapKey: Boolean): Unit =
    t match {
      case null               => writer.writeNull(out)
      case Some(e)            => valueTypeAdapter.write(e, writer, out, isMapKey)
      case None if nullIsNone => writer.writeNull(out)
      case None               =>
    }

  def convertNullToNone() = this.copy(nullIsNone = true)
}
