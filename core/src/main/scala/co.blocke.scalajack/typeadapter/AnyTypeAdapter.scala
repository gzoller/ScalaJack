package co.blocke.scalajack
package typeadapter

import util.BijectiveFunctionHelpers
import model._
import co.blocke.scalajack.model

import scala.reflect.runtime.universe._
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.{Success, Try}

object AnyTypeAdapterFactory extends TypeAdapterFactory {
  override def typeAdapterOf[T](
      next: TypeAdapterFactory
  )(implicit taCache: TypeAdapterCache, tt: TypeTag[T]): TypeAdapter[T] = {
    if (tt.tpe =:= typeOf[Any])
      AnyTypeAdapter(taCache.jackFlavor).asInstanceOf[TypeAdapter[T]]
    else
      next.typeAdapterOf[T]
  }
}

case class AnyTypeAdapter(jackFlavor: JackFlavor[_])(implicit tt: TypeTag[Any]) extends TypeAdapter[Any] {
  lazy val mapAnyTypeAdapter: TypeAdapter[Map[Any, Any]] =
    jackFlavor.taCache.typeAdapterOf[Map[Any, Any]]
  lazy val listAnyTypeAdapter: TypeAdapter[List[Any]] =
    jackFlavor.taCache.typeAdapterOf[List[Any]]
  lazy val optionAnyTypeAdapter: TypeAdapter[Option[Any]] =
    jackFlavor.taCache.typeAdapterOf[Option[Any]]

  def read(parser: Parser): Any =
    parser match {
      case p if p.peekForNull   => null
      case p if p.nextIsBoolean => p.expectBoolean()
      case p if p.nextIsNumber =>
        BigDecimal(p.expectNumber()) match {
          case i if i.isValidInt      => i.toIntExact
          case i if i.isValidLong     => i.toLongExact
          case d if d.isDecimalDouble => d.toDouble
          case d                      => d
        }
      case p if p.nextIsString && jackFlavor.permissivesOk =>
        jackFlavor.stringWrapTypeAdapterFactory(this).read(p)
      case p if p.nextIsString => p.expectString()
      case p if p.nextIsArray =>
        val listBuilder: ListBuffer[Any] = mutable.ListBuffer.empty[Any]
        p.expectList(jackFlavor.anyTypeAdapter, listBuilder)
      case p if p.nextIsObject =>
        val mapBuilder = mutable.Map
          .empty[Any, Any]
          .asInstanceOf[mutable.Builder[(Any, Any), mutable.Map[Any, Any]]]
        val mark = parser.mark()
        val foundMap = p.expectMap[Any, Any, mutable.Map[Any, Any]](
          jackFlavor.stringWrapTypeAdapterFactory(this),
          this,
          mapBuilder
        )
        if (foundMap.contains(jackFlavor.defaultHint)) {
          parser.revertToMark(mark)
          Try(
            BijectiveFunctionHelpers.fullNameToType
              .apply(foundMap(jackFlavor.defaultHint).toString)
          ) match {
            case Success(concreteType) =>
              jackFlavor.taCache.typeAdapter(concreteType).read(p)
            case _ => foundMap
          }
        } else
          foundMap
      case p => p.sourceAsString
    }

  // Need this little bit of gymnastics here to unpack the X type parameter so we can use it to case the TypeAdapter
  private def unpack[X, WIRE](value: X, writer: Writer[WIRE], out: mutable.Builder[WIRE, WIRE]): Unit =
    jackFlavor.taCache
      .typeAdapter(typeFromClassName(value.getClass.getName))
      .asInstanceOf[TypeAdapter[X]] match {
      case ta: CaseClassTypeAdapter[X] =>
        val builder =
          jackFlavor.getBuilder.asInstanceOf[mutable.Builder[WIRE, WIRE]]
        ta.writeWithHint(value, writer, builder)
        writer.writeRaw(builder.result(), out)
      case ta => ta.write(value, writer, out)
    }

  // WARNING: JSON output broken for Option[...] where value is None -- especially bad for Map keys!
  def write[WIRE](t: Any, writer: Writer[WIRE], out: mutable.Builder[WIRE, WIRE]): Unit =
    t match {
      case null                    => writer.writeNull(out)
      case enum: Enumeration#Value => writer.writeString(enum.toString, out)
      case _: Map[_, _] =>
        mapAnyTypeAdapter.write(t.asInstanceOf[Map[Any, Any]], writer, out)
      case _: Seq[_] =>
        listAnyTypeAdapter.write(t.asInstanceOf[List[Any]], writer, out)
      case _: Option[_] =>
        optionAnyTypeAdapter.write(t.asInstanceOf[Option[Any]], writer, out)
      case v => unpack(v, writer, out)
    }
}

// For stringified Map keys, i.e. JSON.  If value is a string, handle normally, else treat as a string wrapper
case class AnyMapKeyTypeAdapter(
    jackFlavor: JackFlavor[_],
    anyTA: TypeAdapter[Any]
)(implicit tt: TypeTag[Any])
    extends TypeAdapter[Any] {
  lazy val mapAnyTypeAdapter: TypeAdapter[Map[Any, Any]] =
    jackFlavor.taCache.typeAdapterOf[Map[Any, Any]]
  lazy val listAnyTypeAdapter: TypeAdapter[List[Any]] =
    jackFlavor.taCache.typeAdapterOf[List[Any]]

  def read(parser: Parser): Any =
    parser.expectString() match {
      case null => null
      case s    => anyTA.read(parser.subParser(s.asInstanceOf[parser.WIRE]))
    }

  private def unpack[X, WIRE](
      value: X,
      writer: Writer[WIRE],
      out: mutable.Builder[WIRE, WIRE],
      isMapKey: Boolean
  )(implicit tx: TypeTag[X]): Unit = {
    jackFlavor.taCache
      .typeAdapter(typeFromClassName(value.getClass.getName))
      .asInstanceOf[TypeAdapter[X]] match {
      case ta: Stringish => ta.write(value, writer, out)
      case ta: CaseClassTypeAdapter[X] =>
        val stringBuilder = model.StringBuilder()
        ta.writeWithHint(
          value,
          writer,
          stringBuilder.asInstanceOf[mutable.Builder[Any, WIRE]]
        )
        writer.writeString(stringBuilder.result(), out)
      case ta =>
        jackFlavor.stringWrapTypeAdapterFactory(ta).write(value, writer, out)
    }
  }

  // WARNING: JSON output broken for Option[...] where value is None -- especially bad for Map keys!
  def write[WIRE](t: Any, writer: Writer[WIRE], out: mutable.Builder[WIRE, WIRE]): Unit =
    t match {
      // Null not needed: null Map keys are inherently invalid in SJ
      case enum: Enumeration#Value =>
        writer.writeString(enum.toString, out)
      case _: Map[_, _] =>
        jackFlavor
          .stringWrapTypeAdapterFactory(mapAnyTypeAdapter)
          .write(t.asInstanceOf[Map[Any, Any]], writer, out)
      case _: Seq[_] =>
        jackFlavor
          .stringWrapTypeAdapterFactory(listAnyTypeAdapter)
          .write(t.asInstanceOf[List[Any]], writer, out)
      case opt: Option[_] if opt.isDefined =>
        write(t.asInstanceOf[Option[_]].get, writer, out)
      // $COVERAGE-OFF$Should be impossible (Nones filtered by CanBuildFromTypeAdapter).  Code left in as a safety
      case opt: Option[_] if opt.isEmpty =>
        jackFlavor.taCache
          .typeAdapter(typeOf[Option[Any]])
          .asInstanceOf[TypeAdapter[Option[Any]]]
          .write(None, writer, out)
      // $COVERAGE-ON$
      case v =>
        unpack(t, writer, out, isMapKey = true)
    }
}
