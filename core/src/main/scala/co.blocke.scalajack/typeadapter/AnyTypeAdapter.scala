package co.blocke.scalajack
package typeadapter

import model._
import util.Path
import model.TokenType._

import scala.collection.mutable.Builder

object AnyTypeAdapterFactory extends TypeAdapter.=:=[Any] {

  var hintLabel: String = null // Very un-functional, but must be set externally because Context isn't created until later.
  var context: Context = null

  private lazy val typeTypeAdapter = context.typeAdapterOf[Type]

  @inline def isNumberChar(char: Char): Boolean =
    ('0' <= char && char <= '9') || (char == '-') || (char == '.') || (char == 'e') || (char == 'E') || (char == '-') || (char == '+')

  def read[WIRE](path: Path, reader: Transceiver[WIRE], isMapKey: Boolean = false): Any = {
    reader.peek() match {
      case BeginObject => // Could be Trait or Map
        reader.savePos()
        reader.lookAheadForField(hintLabel) match {
          case Some(_) => // type hint found... this is a trait
            val concreteType = typeTypeAdapter.read(path, reader, false)
            reader.rollbackToSave()
            context.typeAdapter(concreteType).read(path, reader, isMapKey)

          case None => // no hint found... treat as a Map
            reader.rollbackToSave()
            reader.readMap(path, Map.canBuildFrom[Any, Any], this, this, isMapKey)
        }
      case BeginArray =>
        reader.readArray(path, Vector.canBuildFrom[Any], this, isMapKey).toList
      case Number =>
        reader.readDecimal(path, isMapKey) match {
          case i if i.isValidInt    => i.toIntExact
          case i if i.isValidLong   => i.toLongExact
          case d if d.isExactDouble => d.toDouble
          case d                    => d
        }
      case String if isMapKey =>
        val text = reader.readString(path)
        if (text == "")
          text
        else {
          text.toCharArray.head match {
            case c if c == '[' || c == '{' || isNumberChar(c) || text == "true" || text == "false" =>
              val subReader = reader.cloneWithSource(text.asInstanceOf[WIRE])
              this.read(path, subReader, false)
            case _ =>
              text
          }
        }
      case String =>
        reader.readString(path)
      case True | False =>
        reader.readBoolean(path, isMapKey)
      case Null =>
        reader.skip()
        null
    }
  }

  def write[WIRE](t: Any, writer: Transceiver[WIRE], out: Builder[Any, WIRE]): Unit = {}
}