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
    var builderLengthBeforeMemberNameWritten: Int = 0

    override val structureType = StructureType.Object

    override def beginNestedValue(valueType: ValueType): Unit = {
      nextMemberPartToBeWritten match {
        case MemberPart.MemberName ⇒
          builderLengthBeforeMemberNameWritten = builder.length // Just in case the value is Nothing
          if (numberOfMembersWrittenSoFar > 0) {
            writeValueSeparator()
          }

        case MemberPart.MemberValue ⇒
          writeNameSeparator()
          if (valueType == ValueType.Nothing) {
            builder.length = builderLengthBeforeMemberNameWritten
          }
      }
    }

    override def endNestedValue(valueType: ValueType): Unit = {
      numberOfMembersWrittenSoFar += 1
      nextMemberPartToBeWritten = nextMemberPartToBeWritten match {
        case MemberPart.MemberName ⇒ MemberPart.MemberValue
        case MemberPart.MemberValue ⇒ MemberPart.MemberName
      }
    }

  }

  class ArrayStructure extends Structure {

    var numberOfElementsWrittenSoFar: Int = 0
    var builderLengthBeforeElementWritten: Int = 0

    override val structureType = StructureType.Array

    override def beginNestedValue(valueType: ValueType): Unit = {
      builderLengthBeforeElementWritten = builder.length
      if (numberOfElementsWrittenSoFar > 0) {
        writeValueSeparator()
      }
      if (valueType == ValueType.Nothing) {
        builder.length = builderLengthBeforeElementWritten
      }
    }

    override def endNestedValue(valueType: ValueType): Unit = {
      numberOfElementsWrittenSoFar += 1
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

    valueType match {
      case ValueType.Object ⇒
        structures.push(new ObjectStructure)

      case ValueType.Array ⇒
        structures.push(new ArrayStructure)

      case _ ⇒
    }
  }

  @inline def endValue(valueType: ValueType): Unit = {
    valueType match {
      case ValueType.Object ⇒
        structures.pop().endStructure(expectedStructureType = StructureType.Object)

      case ValueType.Array ⇒
        structures.pop().endStructure(expectedStructureType = StructureType.Array)

      case _ ⇒
    }

    if (structures.nonEmpty) {
      structures.top.endNestedValue(valueType)
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
    builder.append('"').append(string).append('"') // TODO escape values
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
