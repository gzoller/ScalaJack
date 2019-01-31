package co.blocke.scalajack
package typeadapter

import model._
import util.Path
import model.TokenType._

import scala.collection.mutable.Builder
import scala.util.Try

object AnyTypeAdapterFactory extends TypeAdapter.=:=[Any] {

  var jackFlavor: JackFlavor[_, _] = null

  private lazy val typeTypeAdapter: TypeAdapter[Type] = jackFlavor.context.typeAdapterOf[Type]
  private lazy val numberTypeAdapter: TypeAdapter[Number] = jackFlavor.context.typeAdapterOf[Number]
  private lazy val mapAnyTypeAdapter: TypeAdapter[Map[String, Any]] = jackFlavor.context.typeAdapterOf[Map[String, Any]]
  private lazy val listAnyTypeAdapter: TypeAdapter[List[Any]] = jackFlavor.context.typeAdapterOf[List[Any]]

  //  @inline def isNumberChar(char: Char): Boolean =
  //    ('0' <= char && char <= '9') || (char == '-') || (char == '.') || (char == 'e') || (char == 'E') || (char == '-') || (char == '+')

  def read[WIRE](path: Path, reader: Transceiver[WIRE]): Any = {
    reader.peek() match {
      case BeginObject => // Could be Class/Trait or Map
        reader.savePos()
        reader.lookAheadForField(reader.jackFlavor.defaultHint) match {
          case Some(_) => // type hint found... this is a Class/Trait
            val concreteType = typeTypeAdapter.read(path, reader)
            reader.rollbackToSave()
            reader.jackFlavor.context.typeAdapter(concreteType).read(path, reader)

          case None => // no hint found... treat as a Map
            reader.rollbackToSave()
            val raw = reader.readMap(path, Map.canBuildFrom[Any, Any], this, this)
            // We need to check the keys for raw in case they're an embedded list or object.  If not--just return raw
            raw.map {
              case (k, v) =>
                k match {
                  case s: String if s.startsWith("{") && s.endsWith("}") =>
                    Try(mapAnyTypeAdapter.read(path, reader.cloneWithSource(s.asInstanceOf[WIRE]))).map(worked =>
                      if (worked.contains(reader.jackFlavor.defaultHint)) {
                        val ta = reader.jackFlavor.context.typeAdapter(typeFromClassName(worked(reader.jackFlavor.defaultHint).asInstanceOf[String]))
                        (ta.read(path, reader.cloneWithSource(s.asInstanceOf[WIRE])), v)
                      } else
                        (worked, v)
                    ).getOrElse((k, v))
                  case s: String if s.startsWith("[") && s.endsWith("]") =>
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
        reader.readString(path)
      case True | False =>
        reader.readBoolean(path)
      case Null =>
        reader.skip()
        null
    }
  }

  // Need this little bit of gymnastics here to unpack the X type parameter so we can use it to case the TypeAdapter
  private def unpack[X, WIRE](value: X, writer: Transceiver[WIRE], out: Builder[Any, WIRE]) = {
    val valueTA = writer.jackFlavor.context.typeAdapter(typeFromClassName(value.getClass.getName)).asInstanceOf[TypeAdapter[X]]
    if (valueTA.isInstanceOf[classes.CaseClassTypeAdapter[X]])
      valueTA.asInstanceOf[classes.CaseClassTypeAdapter[X]].writeWithHint(value, writer, out)
    else
      valueTA.write(value, writer, out)
  }

  def write[WIRE](t: Any, writer: Transceiver[WIRE], out: Builder[Any, WIRE]): Unit =
    t match {
      case null                    => writer.writeNull(out)
      case s: String               => writer.writeString(s, out)
      case b: Boolean              => writer.writeBoolean(b, out)
      case bi: BigInt              => writer.writeBigInt(bi, out)
      case bd: BigDecimal          => writer.writeDecimal(bd, out)
      case n: Number               => numberTypeAdapter.write(n, writer, out)
      case enum: Enumeration#Value => writer.writeString(enum.toString, out)
      case list: List[_]           => writer.writeArray(list, this, out)
      case mmap: Map[_, _]         => writer.writeMap(mmap.asInstanceOf[Map[Any, Any]], this, this, out)
      case v                       => unpack(v, writer, out)
    }
}