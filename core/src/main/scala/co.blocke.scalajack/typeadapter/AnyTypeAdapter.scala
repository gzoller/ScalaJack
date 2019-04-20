package co.blocke.scalajack
package typeadapter

import model._
import util.Path
import compat.StringBuilder
import model.TokenType._
import typeadapter.classes.CaseClassTypeAdapter

import scala.collection.mutable.Builder
import scala.collection.GenTraversableOnce
import scala.util.{ Try, Success }

object BigIntExtractor {
  def unapply(s: String): Option[BigInt] = Try(BigInt(s)).toOption
}
object BigDecimalExtractor {
  def unapply(s: String): Option[BigDecimal] = Try(BigDecimal(s)).toOption
}

object AnyTypeAdapterFactory extends TypeAdapterFactory {
  var jackFlavor: JackFlavor[_] = null

  override def typeAdapterOf[T](next: TypeAdapterFactory)(implicit context: Context, tt: TypeTag[T]): TypeAdapter[T] = {
    if (tt.tpe =:= typeOf[Any])
      AnyTypeAdapter(jackFlavor).asInstanceOf[TypeAdapter[T]]
    else
      next.typeAdapterOf[T]
  }
}

case class AnyTypeAdapter(jackFlavor: JackFlavor[_]) extends TypeAdapter[Any] {

  private lazy val mapAnyTypeAdapter: TypeAdapter[Map[Any, Any]] = jackFlavor.context.typeAdapterOf[Map[Any, Any]]
  private lazy val listAnyTypeAdapter: TypeAdapter[List[Any]] = jackFlavor.context.typeAdapterOf[List[Any]]
  private lazy val optionAnyTypeAdapter: TypeAdapter[Option[Any]] = jackFlavor.context.typeAdapterOf[Option[Any]]

  def read[WIRE](path: Path, reader: Reader[WIRE]): Any = _read(path, reader)

  def _read[WIRE](path: Path, reader: Reader[WIRE], isSJCapture: Boolean = false): Any = {
    reader.head.tokenType match {
      case BeginObject => // Could be Class/Trait or Map
        if (isSJCapture)
          reader.readMap(path, mapAnyTypeAdapter.asInstanceOf[CanBuildMapTypeAdapter[Any, Any, Map[Any, Any]]].builderFactory, this, this)
        else {
          val savedReader = reader.copy
          reader.scanForHint(reader.jackFlavor.defaultHint) match {
            case Some(typeName) =>
              Try(reader.jackFlavor.typeTypeAdapter.typeNameToType(path, typeName, reader)) match {
                case Success(concreteType) =>
                  // Successfully found/read the type hint, now go back read the actual thing with the right type adapter
                  savedReader.jackFlavor.context.typeAdapter(concreteType).read(path, reader)
                case _ => // Hint found, but failed to read type...Treat as map
                  // lookAheadForTypeHint found nothing so we must reset reader's internal pointer as this is not an error condition
                  reader.syncPositionTo(savedReader)
                  reader.readMap(path, mapAnyTypeAdapter.asInstanceOf[CanBuildMapTypeAdapter[Any, Any, Map[Any, Any]]].builderFactory, this, this)
              }
            case None => // no hint found... treat as a Map
              // lookAheadForTypeHint found nothing so we must reset reader's internal pointer as this is not an error condition
              reader.syncPositionTo(savedReader)
              reader.readMap(path, mapAnyTypeAdapter.asInstanceOf[CanBuildMapTypeAdapter[Any, Any, Map[Any, Any]]].builderFactory, this, this)
          }
        }
      case BeginArray =>
        reader.readArray(path, listAnyTypeAdapter.asInstanceOf[CanBuildFromTypeAdapter[Any, List[Any]]].builderFactory, this)
      case Number =>
        reader.readDecimal(path) match {
          case i if i.isValidInt      => i.toIntExact
          case i if i.isValidLong     => i.toLongExact
          case d if d.isDecimalDouble => d.toDouble
          case d                      => d
        }
      case String =>
        reader.readString(path) match { // course attempt to assign a meaningful type to Any value
          case "true"                 => true
          case "false"                => false
          case BigIntExtractor(s)     => s
          case BigDecimalExtractor(s) => s
          case s if s.startsWith("{") && s.endsWith("}") => // WARNING:  JSON-specific!
            this.read(path, reader.jackFlavor.parse(s.asInstanceOf[WIRE]))
          case s if s.startsWith("[") && s.endsWith("]") => // WARNING:  JSON-specific!
            listAnyTypeAdapter.read(path, reader.jackFlavor.parse(s.asInstanceOf[WIRE]))
          case s => s
        }
      case Boolean =>
        reader.readBoolean(path)
      case Null =>
        reader.next
        null
    }
  }

  // Need this little bit of gymnastics here to unpack the X type parameter so we can use it to case the TypeAdapter
  private def unpack[X, WIRE](value: X, writer: Writer[WIRE], out: Builder[WIRE, WIRE], isMapKey: Boolean) = {
    try {
      // Tease out Lists/Maps
      val inferredClassName = value.getClass.getName match {
        case "scala.collection.immutable.$colon$colon" => "scala.collection.immutable.List"
        case s                                         => s
      }
      val rawValueTA = writer.jackFlavor.context.typeAdapter(typeFromClassName(inferredClassName)).asInstanceOf[TypeAdapter[X]]
      rawValueTA match {
        case _: CaseClassTypeAdapter[X] if isMapKey && writer.jackFlavor.stringifyMapKeys =>
          val stringBuilder = new StringBuilder()
          rawValueTA.asInstanceOf[CaseClassTypeAdapter[X]].writeWithHint(value, writer, stringBuilder.asInstanceOf[Builder[Any, WIRE]], false)
          writer.writeString(stringBuilder.result, out)
        case _: CaseClassTypeAdapter[X] =>
          rawValueTA.asInstanceOf[CaseClassTypeAdapter[X]].writeWithHint(value, writer, out, isMapKey)
        case _: Stringish =>
          rawValueTA.write(value, writer, out, isMapKey)
        case _ if isMapKey && writer.jackFlavor.stringifyMapKeys =>
          writer.jackFlavor.stringWrapTypeAdapterFactory(rawValueTA).write(value, writer, out, isMapKey)
        case _ =>
          rawValueTA.write(value, writer, out, isMapKey)
      }
    } catch {
      case _: java.util.NoSuchElementException =>
        // $COVERAGE-OFF$Should be impossible (is anything not matchable to Any?).  Left here as a safety
        throw new IllegalStateException("Unable to discern type adapter for value " + value)
      // $COVERAGE-ON$
    }
  }

  // WARNING: JSON output broken for Option[...] where value is None -- especially bad for Map keys!
  def write[WIRE](t: Any, writer: Writer[WIRE], out: Builder[WIRE, WIRE], isMapKey: Boolean): Unit = {
    if (isMapKey && writer.jackFlavor.stringifyMapKeys)
      t match {
        // Null not needed: null Map keys are inherently invalid in SJ
        // case null => writer.writeNull(out)
        case enum: Enumeration#Value =>
          writer.writeString(enum.toString, out)
        case _: Map[_, _] =>
          writer.jackFlavor.stringWrapTypeAdapterFactory(mapAnyTypeAdapter).write(t.asInstanceOf[Map[Any, Any]], writer, out, isMapKey)
        case _: GenTraversableOnce[_] =>
          writer.jackFlavor.stringWrapTypeAdapterFactory(listAnyTypeAdapter).write(t.asInstanceOf[List[Any]], writer, out, isMapKey)
        case opt: Option[_] if opt.isDefined =>
          unpack(t.asInstanceOf[Option[_]].get, writer, out, isMapKey)
        // $COVERAGE-OFF$Should be impossible (Nones filtered by CanBuildFromTypeAdapter).  Code left in as a safety
        case opt: Option[_] if opt.isEmpty =>
          unpack(None, writer, out, isMapKey)
        // $COVERAGE-ON$
        case v =>
          unpack(v, writer, out, isMapKey)
      }
    else
      t match {
        case null                     => writer.writeNull(out)
        case enum: Enumeration#Value  => writer.writeString(enum.toString, out)
        case _: Map[_, _]             => mapAnyTypeAdapter.write(t.asInstanceOf[Map[Any, Any]], writer, out, isMapKey)
        case _: GenTraversableOnce[_] => listAnyTypeAdapter.write(t.asInstanceOf[List[Any]], writer, out, isMapKey)
        case _: Option[_]             => optionAnyTypeAdapter.write(t.asInstanceOf[Option[Any]], writer, out, isMapKey)
        case v                        => unpack(v, writer, out, isMapKey)
      }
  }
}