package co.blocke.scalajack.flexjson

object StructureType extends Enumeration {
  type StructureType = Value
  val Object, Array = Value
}

object ValueType extends Enumeration {
  type ValueType = Value
  val String, Number, Boolean, Identifier, Object, Array, Unreadable, Raw, Nothing = Value
}

object MemberPart extends Enumeration {
  type MemberPart = Value
  val MemberName, MemberValue = Value
}

import co.blocke.scalajack.flexjson.StructureType.StructureType
import co.blocke.scalajack.flexjson.ValueType.ValueType
import co.blocke.scalajack.flexjson.MemberPart.MemberPart

class StringJsonWriter extends Writer {

  sealed trait Structure {

    val structureType: StructureType

    def beginNestedValue(nestedValueType: ValueType): Unit

    def endNestedValue(nestedValueType: ValueType): Unit

    def endStructure(expectedStructureType: StructureType): Unit = {
      val actualStructureType = structureType
      if (expectedStructureType != actualStructureType) {
        throw new RuntimeException(s"Attempting to end an $actualStructureType structure when a $expectedStructureType is currently open")
      }
    }

  }

  class ObjectStructure extends Structure {

    var numberOfMembersWrittenSoFar: Int = 0
    var nextMemberPartToBeWritten: MemberPart = MemberPart.MemberName
    var memberPartCurrentlyBeingWritten: MemberPart = null
    var builderLengthBeforeMemberNameWritten: Int = 0

    override val structureType = StructureType.Object

    override def beginNestedValue(nestedValueType: ValueType): Unit = {
      nextMemberPartToBeWritten match {
        case MemberPart.MemberName ⇒
          builderLengthBeforeMemberNameWritten = builder.length // Just in case the value is Nothing
          if (numberOfMembersWrittenSoFar > 0) {
            writeValueSeparator()
          }

          memberPartCurrentlyBeingWritten = MemberPart.MemberName
          nextMemberPartToBeWritten = MemberPart.MemberValue

        case MemberPart.MemberValue ⇒
          writeNameSeparator()
          if (nestedValueType == ValueType.Nothing) {
            builder.length = builderLengthBeforeMemberNameWritten
          }

          memberPartCurrentlyBeingWritten = MemberPart.MemberValue
          nextMemberPartToBeWritten = MemberPart.MemberName
      }
    }

    override def endNestedValue(nestedValueType: ValueType): Unit = {
      if (memberPartCurrentlyBeingWritten == MemberPart.MemberValue && nestedValueType != ValueType.Nothing) {
        numberOfMembersWrittenSoFar += 1
      }

      memberPartCurrentlyBeingWritten = null
    }

  }

  class ArrayStructure extends Structure {

    var numberOfElementsWrittenSoFar: Int = 0
    var builderLengthBeforeElementWritten: Int = 0

    override val structureType = StructureType.Array

    override def beginNestedValue(nestedValueType: ValueType): Unit = {
      builderLengthBeforeElementWritten = builder.length
      if (numberOfElementsWrittenSoFar > 0) {
        writeValueSeparator()
      }
      if (nestedValueType == ValueType.Nothing) {
        builder.length = builderLengthBeforeElementWritten
      }
    }

    override def endNestedValue(nestedValueType: ValueType): Unit = {
      if (nestedValueType != ValueType.Nothing) {
        numberOfElementsWrittenSoFar += 1
      }
    }

  }

  import scala.collection.mutable

  val builder = new StringBuilder
  val structures = new mutable.Stack[Structure]

  def jsonString: String = builder.toString

  @inline def beginValue(valueType: ValueType): Unit = {
    if (structures.nonEmpty) {
      structures.top.beginNestedValue(valueType)
    }
  }

  @inline def endValue(valueType: ValueType): Unit = {
    if (structures.nonEmpty) {
      structures.top.endNestedValue(valueType)
    }
  }

  override def beginObject(): Unit = {
    beginValue(ValueType.Object)
    structures.push(new ObjectStructure)
    builder.append("{")
  }

  override def endObject(): Unit = {
    builder.append("}")
    structures.pop().endStructure(expectedStructureType = StructureType.Object)
    endValue(ValueType.Object)
  }

  override def beginArray(): Unit = {
    beginValue(ValueType.Array)
    structures.push(new ArrayStructure)
    builder.append("[")
  }

  override def endArray(): Unit = {
    builder.append("]")
    structures.pop().endStructure(expectedStructureType = StructureType.Array)
    endValue(ValueType.Array)
  }

  override def writeRawValue(source: Array[Char], offset: Int, length: Int): Unit = {
    beginValue(ValueType.Raw)
    builder.appendAll(source, offset, length)
    endValue(ValueType.Raw)
  }

  override def writeNothing(): Unit = {
    beginValue(ValueType.Nothing)
    endValue(ValueType.Nothing)
  }

  override def writeString(string: String): Unit = {
    beginValue(ValueType.String)
    builder.append('"')

    for (i ← 0 until string.length) {
      string.charAt(i) match {
        case '\t' ⇒
          builder.append("\\t")

        case '\r' ⇒
          builder.append("\\r")

        case '\n' ⇒
          builder.append("\\n")

        case '"' ⇒
          builder.append("\\\"")

        case ch ⇒
          builder.append(ch)
      }
    }

    builder.append('"') // TODO escape values
    endValue(ValueType.String)
  }

  override def writeInt(value: Int): Unit = {
    beginValue(ValueType.Number)
    builder.append(value)
    endValue(ValueType.Number)
  }

  def writeNameSeparator(): Unit =
    builder.append(":")

  def writeValueSeparator(): Unit = {
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
    builder.append('"').append(value).append('"')
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
