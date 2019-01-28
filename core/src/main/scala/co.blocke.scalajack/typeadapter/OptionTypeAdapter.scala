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

object OptionTypeAdapterFactory extends TypeAdapterFactory.=:=.withOneTypeParam[Option] {
  override def create[E](next: TypeAdapterFactory)(implicit context: Context, tt: TypeTag[Option[E]], ttElement: TypeTag[E]): TypeAdapter[Option[E]] =
    OptionTypeAdapter(context.typeAdapterOf[E])
}

case class OptionTypeAdapter[E](valueTypeAdapter: TypeAdapter[E])(implicit tt: TypeTag[E]) extends TypeAdapter[Option[E]] {

  override def defaultValue: Option[Option[E]] = Some(None)

  def read[WIRE](path: Path, reader: Transceiver[WIRE]): Option[E] =
    valueTypeAdapter.read(path, reader) match {
      case null => null
      case v    => Some(v)
    }

  def write[WIRE](t: Option[E], writer: Transceiver[WIRE], out: Builder[Any, WIRE]): Unit =
    t match {
      case Some(e) => valueTypeAdapter.write(e, writer, out)
      case None    =>
    }
}

case class TupleOptionTypeAdapter[E](valueTypeAdapter: TypeAdapter[E])(implicit tt: TypeTag[E]) extends TypeAdapter[Option[E]] {

  override def defaultValue: Option[Option[E]] = Some(None)

  def read[WIRE](path: Path, reader: Transceiver[WIRE]): Option[E] =
    valueTypeAdapter.read(path, reader) match {
      case null => None // <-- Difference from main OptionTypeAdapter... treat null as None to preserve order in tuple
      case v    => Some(v)
    }

  def write[WIRE](t: Option[E], writer: Transceiver[WIRE], out: Builder[Any, WIRE]): Unit =
    t match {
      case Some(e) => valueTypeAdapter.write(e, writer, out)
      case None    =>
    }
}
