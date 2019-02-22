package co.blocke.scalajack
package typeadapter

import model._
import util.Path
import model.TokenType._
import typeadapter.classes.CaseClassTypeAdapter

import scala.collection.mutable.{ Builder, StringBuilder }
import scala.collection.GenTraversableOnce
import scala.util.Try

object BigIntExtractor {
  def unapply(s: String): Option[BigInt] = Try(BigInt(s)).toOption
}
object BigDecimalExtractor {
  def unapply(s: String): Option[BigDecimal] = Try(BigDecimal(s)).toOption
}

object AnyTypeAdapterFactory extends TypeAdapter.=:=[Any] {

  var jackFlavor: JackFlavor[_] = null

  private lazy val classMapTypeAdapter: TypeAdapter[Map[String, Any]] = jackFlavor.context.typeAdapterOf[Map[String, Any]]
  private lazy val mapAnyTypeAdapter: TypeAdapter[Map[Any, Any]] = jackFlavor.context.typeAdapterOf[Map[Any, Any]]
  private lazy val listAnyTypeAdapter: TypeAdapter[List[Any]] = jackFlavor.context.typeAdapterOf[List[Any]]
  private lazy val optionAnyTypeAdapter: TypeAdapter[Option[Any]] = jackFlavor.context.typeAdapterOf[Option[Any]]

  //  @inline def isNumberChar(char: Char): Boolean =
  //    ('0' <= char && char <= '9') || (char == '-') || (char == '.') || (char == 'e') || (char == 'E') || (char == '-') || (char == '+')

  def read[WIRE](path: Path, reader: Transceiver[WIRE]): Any = {
    reader.peek() match {
      case BeginObject => // Could be Class/Trait or Map
        reader.lookAheadForTypeHint(reader.jackFlavor.defaultHint, (s: String) => this.jackFlavor.typeTypeAdapter.read(path, reader)) match {
          case Some(concreteType) => // type hint found... this is a Class/Trait
            reader.jackFlavor.context.typeAdapter(concreteType).read(path, reader)

          case None => // no hint found... treat as a Map
            reader.rollbackToSave() // lookAheadForTypeHint found nothing so we must reset reader's internal pointer as this is not an error condition
            val raw = reader.readMap(path, Map.canBuildFrom[Any, Any], this, this)
            // We need to check the keys for raw in case they're an embedded list or object.  If not--just return raw
            raw.map {
              case (k, v) =>
                k match {
                  case s: String if s.startsWith("{") && s.endsWith("}") => // WARNING:  JSON-specific!
                    Try(classMapTypeAdapter.read(path, reader.cloneWithSource(s.asInstanceOf[WIRE]))).map(worked =>
                      if (worked.contains(reader.jackFlavor.defaultHint)) {
                        val ta = reader.jackFlavor.context.typeAdapter(typeFromClassName(worked(reader.jackFlavor.defaultHint).asInstanceOf[String]))
                        (ta.read(path, reader.cloneWithSource(s.asInstanceOf[WIRE])), v)
                      } else
                        (worked, v)
                    ).getOrElse((k, v))
                  case s: String if s.startsWith("[") && s.endsWith("]") => // WARNING:  JSON-specific!
                    (listAnyTypeAdapter.read(path, reader.cloneWithSource(s.asInstanceOf[WIRE])), v)
                  case _ => (k, v)
                }
            }
        }
      case BeginArray =>
        reader.readArray(path, Vector.canBuildFrom[Any], this).toList
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
            this.read(path, reader.cloneWithSource(s.asInstanceOf[WIRE]))
          case s if s.startsWith("[") && s.endsWith("]") => // WARNING:  JSON-specific!
            listAnyTypeAdapter.read(path, reader.cloneWithSource(s.asInstanceOf[WIRE]))
          case s => s
        }
      case True | False =>
        reader.readBoolean(path)
      case Null =>
        reader.skip()
        null
    }
  }

  // Need this little bit of gymnastics here to unpack the X type parameter so we can use it to case the TypeAdapter
  private def unpack[X, WIRE](value: X, writer: Transceiver[WIRE], out: Builder[Any, WIRE], isMapKey: Boolean) = {
    try {
      val rawValueTA = writer.jackFlavor.context.typeAdapter(typeFromClassName(value.getClass.getName)).asInstanceOf[TypeAdapter[X]]
      rawValueTA match {
        case cc: CaseClassTypeAdapter[X] if isMapKey && writer.jackFlavor.stringifyMapKeys =>
          val stringBuilder = new StringBuilder()
          rawValueTA.asInstanceOf[CaseClassTypeAdapter[X]].writeWithHint(value, writer, stringBuilder.asInstanceOf[Builder[Any, WIRE]], false)
          writer.writeString(stringBuilder.result, out)
        case cc: CaseClassTypeAdapter[X] =>
          rawValueTA.asInstanceOf[CaseClassTypeAdapter[X]].writeWithHint(value, writer, out, isMapKey)
        case _: Stringish =>
          rawValueTA.write(value, writer, out, isMapKey)
        case _ if isMapKey && writer.jackFlavor.stringifyMapKeys =>
          (new StringWrapTypeAdapter(rawValueTA)).write(value, writer, out, isMapKey)
        case _ =>
          rawValueTA.write(value, writer, out, isMapKey)
      }
    } catch {
      case _: java.util.NoSuchElementException => throw new Exception("Unable to discern type adapter for value " + value)
    }
  }

  // TODO: Can't handle containers at all.... Must add string wrappers where needed for Map keys
  def write[WIRE](t: Any, writer: Transceiver[WIRE], out: Builder[Any, WIRE], isMapKey: Boolean): Unit =
    if (isMapKey && writer.jackFlavor.stringifyMapKeys)
      t match {
        case null                        => writer.writeNull(out)
        case enum: Enumeration#Value     => writer.writeString(enum.toString, out)
        case map: Map[_, _]              => (new StringWrapTypeAdapter(mapAnyTypeAdapter)).write(t.asInstanceOf[Map[Any, Any]], writer, out, isMapKey)
        case list: GenTraversableOnce[_] => (new StringWrapTypeAdapter(listAnyTypeAdapter)).write(t.asInstanceOf[List[Any]], writer, out, isMapKey)
        case opt: Option[_]              => (new StringWrapTypeAdapter(optionAnyTypeAdapter)).write(t.asInstanceOf[Option[Any]], writer, out, isMapKey)
        case v                           => unpack(v, writer, out, isMapKey)
      }
    else
      t match {
        case null                        => writer.writeNull(out)
        case enum: Enumeration#Value     => writer.writeString(enum.toString, out)
        case map: Map[_, _]              => mapAnyTypeAdapter.write(t.asInstanceOf[Map[Any, Any]], writer, out, isMapKey)
        case list: GenTraversableOnce[_] => listAnyTypeAdapter.write(t.asInstanceOf[List[Any]], writer, out, isMapKey)
        case opt: Option[_]              => optionAnyTypeAdapter.write(t.asInstanceOf[Option[Any]], writer, out, isMapKey)
        case v                           => unpack(v, writer, out, isMapKey)
      }
}