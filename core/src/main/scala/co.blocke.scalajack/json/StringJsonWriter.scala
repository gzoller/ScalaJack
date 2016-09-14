package co.blocke.scalajack.json

object StructureType extends Enumeration {
  type StructureType = Value
  val Object, Array = Value
}

object ValueType extends Enumeration {
  type ValueType = Value
  val String, Number, LiteralName, Object, Array, Unreadable, Raw, Nothing = Value
}

object MemberPart extends Enumeration {
  type MemberPart = Value
  val MemberName, MemberValue = Value
}

import co.blocke.scalajack.RenderException
import co.blocke.scalajack.json.StructureType.StructureType
import co.blocke.scalajack.json.ValueType.ValueType
import co.blocke.scalajack.json.MemberPart.MemberPart

class StringJsonWriter(canonical: Boolean = true) extends Writer {

  sealed trait Structure {

    val structureType: StructureType

    def parent: Structure

    def beginChildValue(nestedValueType: ValueType): Unit

    def endChildValue(nestedValueType: ValueType): Unit

    def end(expectedStructureType: StructureType): Unit = {
      val actualStructureType = structureType
      if (expectedStructureType != actualStructureType) {
        throw new RuntimeException(s"Attempting to end an $actualStructureType structure when a $expectedStructureType is currently open")
      }
    }

  }

  object RootStructure extends Structure {

    override val structureType: StructureType = null

    override def parent: Structure = ???

    override def beginChildValue(nestedValueType: ValueType): Unit = {}

    override def endChildValue(nestedValueType: ValueType): Unit = {}

  }

  class ObjectStructure(override val parent: Structure) extends Structure {

    var numberOfMembersWrittenSoFar: Int = 0
    var nextMemberPartToBeWritten: MemberPart = MemberPart.MemberName
    var memberPartCurrentlyBeingWritten: MemberPart = null
    var builderLengthBeforeMemberNameWritten: Int = 0

    override val structureType = StructureType.Object

    override def beginChildValue(childValueType: ValueType): Unit = {
      nextMemberPartToBeWritten match {
        case MemberPart.MemberName ⇒

          if (canonical && childValueType != ValueType.String) {
            throw new RenderException(s"Member names must be of type ${TokenType.String}, not $childValueType")
          }

          builderLengthBeforeMemberNameWritten = builder.length // Just in case the value is Nothing
          if (numberOfMembersWrittenSoFar > 0) {
            writeValueSeparator()
          }

          memberPartCurrentlyBeingWritten = MemberPart.MemberName
          nextMemberPartToBeWritten = MemberPart.MemberValue

        case MemberPart.MemberValue ⇒
          writeNameSeparator()
          if (childValueType == ValueType.Nothing) {
            builder.length = builderLengthBeforeMemberNameWritten
          }

          memberPartCurrentlyBeingWritten = MemberPart.MemberValue
          nextMemberPartToBeWritten = MemberPart.MemberName
      }
    }

    override def endChildValue(childValueType: ValueType): Unit = {
      if (memberPartCurrentlyBeingWritten == MemberPart.MemberValue && childValueType != ValueType.Nothing) {
        numberOfMembersWrittenSoFar += 1
      }

      memberPartCurrentlyBeingWritten = null
    }

  }

  class ArrayStructure(override val parent: Structure) extends Structure {

    var numberOfElementsWrittenSoFar: Int = 0
    var builderLengthBeforeElementWritten: Int = 0

    override val structureType = StructureType.Array

    override def beginChildValue(childValueType: ValueType): Unit = {
      builderLengthBeforeElementWritten = builder.length
      if (numberOfElementsWrittenSoFar > 0) {
        writeValueSeparator()
      }
      if (childValueType == ValueType.Nothing) {
        builder.length = builderLengthBeforeElementWritten
      }
    }

    override def endChildValue(childValueType: ValueType): Unit = {
      if (childValueType != ValueType.Nothing) {
        numberOfElementsWrittenSoFar += 1
      }
    }

  }

  val builder = new StringBuilder
  var structure: Structure = RootStructure

  def jsonString: String = builder.toString

  override def beginObject(): Unit = {
    structure.beginChildValue(ValueType.Object)
    structure = new ObjectStructure(parent = structure)
    builder.append("{")
  }

  override def endObject(): Unit = {
    builder.append("}")
    structure.end(expectedStructureType = StructureType.Object)
    structure = structure.parent
    structure.endChildValue(ValueType.Object)
  }

  override def beginArray(): Unit = {
    structure.beginChildValue(ValueType.Array)
    structure = new ArrayStructure(parent = structure)
    builder.append("[")
  }

  override def endArray(): Unit = {
    builder.append("]")
    structure.end(expectedStructureType = StructureType.Array)
    structure = structure.parent
    structure.endChildValue(ValueType.Array)
  }

  override def writeRawValue(source: Array[Char], offset: Int, length: Int): Unit = {
    structure.beginChildValue(ValueType.Raw)
    builder.appendAll(source, offset, length)
    structure.endChildValue(ValueType.Raw)
  }

  override def writeNothing(): Unit = {
    structure.beginChildValue(ValueType.Nothing)
    structure.endChildValue(ValueType.Nothing)
  }

  override def writeString(string: String): Unit = {
    structure.beginChildValue(ValueType.String)
    builder append '"'

    var i = 0
    val length = string.length

    var startOfUnescapedCharacters = 0

    @inline def appendAnyUnescapedCharacters(): Unit = {
      if (i > startOfUnescapedCharacters) {
        builder append string.substring(startOfUnescapedCharacters, i)
      }
    }

    while (i < length) {
      string.charAt(i) match {
        case '"' ⇒
          appendAnyUnescapedCharacters()
          builder append """\""""
          startOfUnescapedCharacters = i + 1

        case '\\' ⇒
          appendAnyUnescapedCharacters()
          builder append """\\"""
          startOfUnescapedCharacters = i + 1

        case '/' ⇒
          appendAnyUnescapedCharacters()
          builder append """\/"""
          startOfUnescapedCharacters = i + 1

        case '\b' ⇒
          appendAnyUnescapedCharacters()
          builder append """\b"""
          startOfUnescapedCharacters = i + 1

        case '\f' ⇒
          appendAnyUnescapedCharacters()
          builder append """\f"""
          startOfUnescapedCharacters = i + 1

        case '\n' ⇒
          appendAnyUnescapedCharacters()
          builder append """\n"""
          startOfUnescapedCharacters = i + 1

        case '\r' ⇒
          appendAnyUnescapedCharacters()
          builder append """\r"""
          startOfUnescapedCharacters = i + 1

        case '\t' ⇒
          appendAnyUnescapedCharacters()
          builder append """\t"""
          startOfUnescapedCharacters = i + 1

        case ch if ch >= 128 ⇒
          appendAnyUnescapedCharacters()
          builder append """\""" append "u" append "%04x".format(ch.toInt)
          startOfUnescapedCharacters = i + 1

        case _ ⇒
      }

      i += 1
    }

    appendAnyUnescapedCharacters()

    builder append '"'
    structure.endChildValue(ValueType.String)
  }

  override def writeInt(value: Int): Unit = {
    structure.beginChildValue(ValueType.Number)
    builder.append(value)
    structure.endChildValue(ValueType.Number)
  }

  def writeNameSeparator(): Unit =
    builder.append(":")

  def writeValueSeparator(): Unit = {
    builder.append(",")
  }

  override def writeFalse(): Unit = {
    structure.beginChildValue(ValueType.LiteralName)
    builder.append("false")
    structure.endChildValue(ValueType.LiteralName)
  }

  override def writeTrue(): Unit = {
    structure.beginChildValue(ValueType.LiteralName)
    builder.append("true")
    structure.endChildValue(ValueType.LiteralName)
  }

  override def writeNull(): Unit = {
    structure.beginChildValue(ValueType.LiteralName)
    builder.append("null")
    structure.endChildValue(ValueType.LiteralName)
  }

  override def writeFloat(value: Float): Unit = {
    structure.beginChildValue(ValueType.Number)
    builder.append(value)
    structure.endChildValue(ValueType.Number)
  }

  override def writeDouble(value: Double): Unit = {
    structure.beginChildValue(ValueType.Number)
    builder.append(value)
    structure.endChildValue(ValueType.Number)
  }

  override def writeLong(value: Long): Unit = {
    structure.beginChildValue(ValueType.Number)
    builder.append(value)
    structure.endChildValue(ValueType.Number)
  }

  override def writeChar(value: Char): Unit = {
    structure.beginChildValue(ValueType.String)
    builder.append('"').append(value).append('"')
    structure.endChildValue(ValueType.String)
  }

  override def writeByte(value: Byte): Unit = {
    structure.beginChildValue(ValueType.Number)
    builder.append(value)
    structure.endChildValue(ValueType.Number)
  }

  override def writeShort(value: Short): Unit = {
    structure.beginChildValue(ValueType.Number)
    builder.append(value)
    structure.endChildValue(ValueType.Number)
  }

  override def writeBoolean(value: Boolean): Unit = {
    structure.beginChildValue(ValueType.LiteralName)
    builder.append(value)
    structure.endChildValue(ValueType.LiteralName)
  }

}
