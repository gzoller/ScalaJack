package co.blocke.scalajack

import co.blocke.scalajack.json.Writer
import org.bson.{BsonArray, BsonDocument, BsonInt32, BsonString, BsonValue}

class BsonWriter extends Writer {

  object MemberPart extends Enumeration {
    type MemberPart = Value
    val MemberName, MemberValue = Value
  }
  import MemberPart._

  sealed trait Structure {

    val parent: Structure

    def beginChildValue(childValue: BsonValue): Unit

    def endChildValue(childValue: BsonValue): Unit

    def end(): Unit

  }

  var value: BsonValue = _

  object RootStructure extends Structure {

    override val parent: Structure = null

    override def beginChildValue(childValue: BsonValue): Unit = {}

    override def endChildValue(childValue: BsonValue): Unit = {
      value = childValue
    }

    override def end(): Unit = ???

  }

  class ObjectStructure(override val parent: Structure, val document: BsonDocument) extends Structure {

    var nextMemberPartToBeWritten: MemberPart = MemberPart.MemberName
    var memberPartCurrentlyBeingWritten: MemberPart = _

    var memberName: BsonValue = _

    override def beginChildValue(childValue: BsonValue): Unit = {
      nextMemberPartToBeWritten match {
        case MemberName =>
          memberName = null
          memberPartCurrentlyBeingWritten = MemberName
          nextMemberPartToBeWritten = MemberValue

        case MemberValue =>
          memberPartCurrentlyBeingWritten = MemberValue
          nextMemberPartToBeWritten = MemberName
      }
    }

    override def endChildValue(childValue: BsonValue): Unit = {
      memberPartCurrentlyBeingWritten match {
        case MemberName =>
          memberName = childValue

        case MemberValue =>
          document.append(memberName.asString.getValue, childValue)
      }
    }

    override def end(): Unit = {}

  }

  class ArrayStructure(override val parent: Structure, val array: BsonArray) extends Structure {

    override def beginChildValue(childValue: BsonValue): Unit = ???

    override def endChildValue(childValue: BsonValue): Unit = ???

    override def end(): Unit = {}

  }

  var structure: Structure = RootStructure

  override def beginObject(): Unit = {
    val document = new BsonDocument

    structure.beginChildValue(document)
    structure = new ObjectStructure(structure, document)
  }

  override def endObject(): Unit = {
    structure.end() // TODO expected structure type
    val document = structure.asInstanceOf[ObjectStructure].document
    structure = structure.parent
    structure.endChildValue(document)
  }

  override def beginArray(): Unit = ???

  override def endArray(): Unit = ???

  override def writeRawValue(source: Array[Char], offset: Int, length: Int): Unit = ???

  override def writeNothing(): Unit = ???

  override def writeString(string: String): Unit = {
    val value = new BsonString(string)
    structure.beginChildValue(value)
    structure.endChildValue(value)
  }

  override def writeByte(value: Byte): Unit = ???

  override def writeShort(value: Short): Unit = ???

  override def writeInt(value: Int): Unit = {
    val bsonValue = new BsonInt32(value)
    structure.beginChildValue(bsonValue)
    structure.endChildValue(bsonValue)
  }

  override def writeFloat(value: Float): Unit = ???

  override def writeDouble(value: Double): Unit = ???

  override def writeLong(value: Long): Unit = ???

  override def writeBoolean(value: Boolean): Unit = ???

  override def writeFalse(): Unit = ???

  override def writeTrue(): Unit = ???

  override def writeNull(): Unit = ???

  override def writeChar(value: Char): Unit = ???

}
