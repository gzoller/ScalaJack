package co.blocke.scalajack
package typeadapter

import model._
import util.Path
import model.TokenType._

object AnyTypeAdapterFactory extends TypeAdapter.=:=[Any] {

  var hintLabel: String = null // Very un-functional, but must be set externally because Context isn't created until later.
  var context: Context = null

  private lazy val typeTypeAdapter = context.typeAdapterOf[Type]

  @inline def isNumberChar(char: Char): Boolean =
    ('0' <= char && char <= '9') || (char == '-') || (char == '.') || (char == 'e') || (char == 'E') || (char == '-') || (char == '+')

  def read(path: Path, reader: Reader, isMapKey: Boolean = false): Any = {
    reader.peek() match {
      case BeginObject => // Could be Trait or Map
        reader.savePos()
        reader.lookAheadForField(hintLabel) match {
          case Some(found) =>
            val concreteType = typeTypeAdapter.read(path, reader, false)
            reader.rollbackToSave()
            context.typeAdapter(concreteType).read(path, reader, isMapKey)

          case None =>
            reader.rollbackToSave()
            reader.readMap(path, Map.canBuildFrom[Any, Any], this, this, isMapKey)
        }
      case BeginArray =>
        reader.readArray(path, Vector.canBuildFrom[Any], this, isMapKey).toList
      case Number =>
        reader.readDecimal(isMapKey) match {
          case i if i.isValidInt    => i.toIntExact
          case i if i.isValidLong   => i.toLongExact
          case d if d.isExactDouble => d.toDouble
          case d                    => d
        }
      case String if isMapKey =>
        val text = reader.readString()
        if (text == "")
          text
        else {
          text.toCharArray.head match {
            case c if c == '[' || c == '{' || isNumberChar(c) || text == "true" || text == "false" =>
              val subReader = reader.cloneWithSource(text.asInstanceOf[reader.WIRE])
              this.read(path, subReader, false)
            case _ =>
              text
          }
        }
      case String =>
        reader.readString()
      case True | False =>
        reader.readBoolean(isMapKey)
      case Null =>
        reader.skip()
        null
    }
  }
}