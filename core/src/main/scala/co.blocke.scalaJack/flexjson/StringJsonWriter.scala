package co.blocke.scalajack.flexjson

object StructureType extends Enumeration {
  type StructureType = Value
  val Object, Array = Value
}

object ValueType extends Enumeration {
  type ValueType = Value
  val String, Number, Boolean, Identifier, Object, Array, Unreadable, Nothing = Value
}

object MemberPart extends Enumeration {
  type MemberPart = Value
  val MemberName, MemberValue = Value
}

import co.blocke.scalajack.flexjson.StructureType.StructureType
import co.blocke.scalajack.flexjson.ValueType.ValueType
import co.blocke.scalajack.flexjson.MemberPart.MemberPart

class Structure(var structureType: StructureType) {

  // structureType == StructureType.Object
  var numberOfMembersWrittenSoFar: Int = 0
  var nextMemberPartToBeWritten: MemberPart = MemberPart.MemberName
  var builderLengthBeforeMemberNameWritten: Int = 0

  // structureType == StructureType.Array
  var numberOfElementsWrittenSoFar: Int = 0
  var builderLengthBeforeElementWritten: Int = 0

}

class StringJsonWriter extends Writer {

  import scala.collection.mutable

  val builder = new StringBuilder
  val structures = new mutable.Stack[Structure]

  def jsonString: String = builder.toString

  @inline def structure: Structure = structures.last

  @inline def beginValue(valueType: ValueType): Unit = {
    if (structures.nonEmpty) {
      val structure = structures.top

      structure.structureType match {
        case StructureType.Object ⇒

          structure.nextMemberPartToBeWritten match {
            case MemberPart.MemberName ⇒
              structure.builderLengthBeforeMemberNameWritten = builder.length // Just in case the value is Nothing
              if (structure.numberOfMembersWrittenSoFar > 0) {
                writeValueSeparator()
              }

            case MemberPart.MemberValue ⇒
              writeNameSeparator()
              if (valueType == ValueType.Nothing) {
                builder.length = structure.builderLengthBeforeMemberNameWritten
              }
          }

        case StructureType.Array ⇒
          structure.builderLengthBeforeElementWritten = builder.length
          if (structure.numberOfElementsWrittenSoFar > 0) {
            writeValueSeparator()
          }
          if (valueType == ValueType.Nothing) {
            builder.length = structure.builderLengthBeforeElementWritten
          }

      }
    }

    valueType match {
      case ValueType.Object ⇒
        val structure = new Structure(StructureType.Object)
        structures.push(structure)

      case ValueType.Array ⇒
        val structure = new Structure(StructureType.Array)
        structures.push(structure)

      case _ ⇒
    }
  }

  @inline def endValue(valueType: ValueType): Unit = {
    valueType match {
      case ValueType.Object ⇒
        val structure = structures.pop()
        assert(structure.structureType == StructureType.Object)

      case ValueType.Array ⇒
        val structure = structures.pop()
        assert(structure.structureType == StructureType.Array)

      case _ ⇒
    }

    if (structures.nonEmpty) {
      val structure = structures.top

      structure.structureType match {
        case StructureType.Object ⇒
          structure.numberOfMembersWrittenSoFar += 1
          structure.nextMemberPartToBeWritten = structure.nextMemberPartToBeWritten match {
            case MemberPart.MemberName ⇒ MemberPart.MemberValue
            case MemberPart.MemberValue ⇒ MemberPart.MemberName
          }

        case StructureType.Array ⇒
          structure.numberOfElementsWrittenSoFar += 1
      }
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
