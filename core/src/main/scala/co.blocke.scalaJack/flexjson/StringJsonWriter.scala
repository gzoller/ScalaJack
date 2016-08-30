package co.blocke.scalajack.flexjson

object StructureType extends Enumeration {
  type StructureType = Value
  val Object, Array = Value
}

object ValueType extends Enumeration {
  type ValueType = Value
  val String, Number, Boolean, Identifier, Object, Array, Unreadable, Nothing = Value
}

import StructureType.StructureType
import ValueType.ValueType

class Structure {

  var structureType: StructureType = _
  var numberOfElementsSoFar: Int = 0
  var numberOfFieldsSoFar: Int = 0
  var fieldNameWritten: Boolean = false
  var builderLengthBeforeFieldName: Int = 0

  def reset(): Unit = {
    structureType = null
    numberOfElementsSoFar = 0
    numberOfFieldsSoFar = 0
    fieldNameWritten = false
    builderLengthBeforeFieldName = 0
  }

}

class StringJsonWriter extends Writer {

  val builder = new StringBuilder

  var optionalPendingName: Option[String] = None

  val structureStack = new Array[Structure](16)
  var structureDepth = 0
  var maxDepth = 0

  def jsonString: String = builder.toString

  @inline def createStructureIfAbsent(depth: Int): Structure = {
    structureStack(depth) match {
      case null ⇒
        val structure = new Structure
        structureStack(depth) = structure
        structure

      case structure ⇒
        structure
    }
  }

  @inline def structure: Structure = structureStack(structureDepth)

  @inline def beginValue(valueType: ValueType): Unit = {
    val currentStructure = this.structure

    currentStructure.structureType match {
      case StructureType.Object ⇒
        if (currentStructure.fieldNameWritten) {
          // Writing the value
          if (valueType == ValueType.Nothing) {
            // Undo the name
            builder.length = currentStructure.builderLengthBeforeFieldName
          }
        } else {
          if (structure.numberOfFieldsSoFar > 0) {
            writeValueSeparator()
            structure.numberOfFieldsSoFar += 1
          }

          // Writing the key
          currentStructure.builderLengthBeforeFieldName = builder.length // Just in case the value is Nothing
        }
    }

    structureDepth += 1

    val nextStackFrame = this.structure



    val stackFrame = createStructureIfAbsent(structureDepth)
//    stackFrame.structureType = valueType

    writeValueSeparatorIfSubsequent()
    writeNameIfPending()
  }

  @inline def endValue(valueType: ValueType): Unit = {
    valueType match {
      case ValueType.Object | ValueType.Array ⇒
        structureDepth -= 1

      case _ ⇒
    }
  }

  override def beginObject(): Unit = {
    beginValue(ValueType.Object)
    builder.append("{")
  }

  override def endObject(): Unit = {
    builder.append("}")
    endValue(ValueType.Object)
  }

  override def beginArray(): Unit = {
    beginValue(ValueType.Array)
    builder.append("[")
  }

  override def endArray(): Unit = {
    builder.append("]")
    endValue(ValueType.Array)
  }

  override def writeRaw(source: Array[Char], offset: Int, length: Int): Unit =
    builder.appendAll(source, offset, length)

  override def writeName(name: String): Unit =
    writeString(name)

  def writeValueSeparatorIfSubsequent(): Unit = {
    ???
  }

  def writeNameIfPending(): Unit =
    optionalPendingName match {
      case Some(pendingName) ⇒
        writeValueSeparatorIfSubsequent()
        writeName(pendingName)
        writeNameSeparator()

      case None ⇒
        // Do nothing
    }

  override def writeNothing(): Unit = {
    beginValue(ValueType.Nothing)
    endValue(ValueType.Nothing)
  }

  override def writeString(string: String): Unit = {
    beginValue(ValueType.String)
    builder.append('"').append(string).append('"') // TODO escape values
    endValue(ValueType.String)
  }

  override def writeInt(value: Int): Unit = {
    beginValue(ValueType.Number)
    builder.append(value)
    endValue(ValueType.Number)
  }

  override def writeNameSeparator(): Unit =
    builder.append(":")

  override def writeValueSeparator(): Unit = {
    writeNameIfPending()
    builder.append(",")
  }

  override def writeFalse(): Unit = {
    beginValue(ValueType.Identifier)
    builder.append("false")
    endValue(ValueType.Identifier)
  }

  override def writeTrue(): Unit = {
    beginValue(ValueType.Identifier)
    builder.append("true")
    endValue(ValueType.Identifier)
  }

  override def writeNull(): Unit = {
    beginValue(ValueType.Identifier)
    builder.append("null")
    endValue(ValueType.Identifier)
  }

  override def writeFloat(value: Float): Unit = {
    beginValue(ValueType.Number)
    builder.append(value)
    endValue(ValueType.Number)
  }

  override def writeDouble(value: Double): Unit = {
    beginValue(ValueType.Number)
    builder.append(value)
    endValue(ValueType.Number)
  }

  override def writeLong(value: Long): Unit = {
    beginValue(ValueType.Number)
    builder.append(value)
    endValue(ValueType.Number)
  }

  override def writeChar(value: Char): Unit = {
    beginValue(ValueType.String)
    builder.append(value)
    endValue(ValueType.String)
  }

  override def writeByte(value: Byte): Unit = {
    beginValue(ValueType.Number)
    builder.append(value)
    endValue(ValueType.Number)
  }

  override def writeShort(value: Short): Unit = {
    beginValue(ValueType.Number)
    builder.append(value)
    endValue(ValueType.Number)
  }

  override def writeBoolean(value: Boolean): Unit = {
    beginValue(ValueType.Boolean)
    builder.append(value)
    endValue(ValueType.Boolean)
  }
}
