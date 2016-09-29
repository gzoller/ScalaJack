package co.blocke.scalajack

import org.bson.{ BsonArray, BsonBoolean, BsonDocument, BsonDouble, BsonInt32, BsonInt64, BsonString, BsonValue }
import org.mongodb.scala.bson.BsonNull

class BsonWriter extends Writer {

  object MemberPart extends Enumeration {
    type MemberPart = Value
    val MemberName, MemberValue = Value
  }
  import MemberPart._

  sealed trait Structure {

    val parent: Structure

    def beginChildValue(): Unit

    def endChildValue(childValue: BsonValue): Unit

    def onChildValue(childValue: BsonValue): Unit = {
      beginChildValue()
      endChildValue(childValue)
    }

    def end(): Unit

  }

  object RootStructure extends Structure {

    override val parent: Structure = null
    var value: BsonValue = _

    override def beginChildValue(): Unit = {}

    override def endChildValue(childValue: BsonValue): Unit = {
      value = childValue
    }

    override def end(): Unit = {}

  }

  class ObjectStructure(override val parent: Structure, val document: BsonDocument) extends Structure {

    var nextMemberPartToBeWritten: MemberPart = MemberPart.MemberName
    var memberPartCurrentlyBeingWritten: MemberPart = _

    var memberName: BsonValue = _

    override def beginChildValue(): Unit = {
      nextMemberPartToBeWritten match {
        case MemberName ⇒
          memberName = null
          memberPartCurrentlyBeingWritten = MemberName
          nextMemberPartToBeWritten = MemberValue

        case MemberValue ⇒
          memberPartCurrentlyBeingWritten = MemberValue
          nextMemberPartToBeWritten = MemberName
      }
    }

    override def endChildValue(childValue: BsonValue): Unit = {
      memberPartCurrentlyBeingWritten match {
        case MemberName ⇒
          memberName = childValue

        case MemberValue ⇒
          if (childValue != null) {
            val memberNameAsString = memberName.asString.getValue
            document.append(memberNameAsString, childValue)
          }

          memberPartCurrentlyBeingWritten = null
      }
    }

    override def end(): Unit = {}

  }

  class ArrayStructure(override val parent: Structure, val array: BsonArray) extends Structure {

    override def beginChildValue(): Unit = {

    }

    override def endChildValue(childValue: BsonValue): Unit = {
      if (childValue != null) {
        array.add(childValue)
      }
    }

    override def end(): Unit = {}

  }

  var structure: Structure = RootStructure

  override def beginObject(): Unit = {
    val document = new BsonDocument

    structure.beginChildValue()
    structure = new ObjectStructure(structure, document)
  }

  override def endObject(): Unit = {
    structure.end() // TODO expected structure type
    val document = structure.asInstanceOf[ObjectStructure].document
    structure = structure.parent
    structure.endChildValue(document)
  }

  override def beginArray(): Unit = {
    val array = new BsonArray

    structure.beginChildValue()
    structure = new ArrayStructure(structure, array)
  }

  override def endArray(): Unit = {
    structure.end()
    val array = structure.asInstanceOf[ArrayStructure].array
    structure = structure.parent
    structure.endChildValue(array)
  }

  override def writeRawValue(source: Array[Char], offset: Int, length: Int): Unit = ???

  override def writeNothing(): Unit = {
    structure.onChildValue(null)
  }

  override def writeString(string: String): Unit =
    structure.onChildValue(new BsonString(string))

  override def writeByte(value: Byte): Unit =
    structure.onChildValue(new BsonInt32(value.toInt))

  override def writeShort(value: Short): Unit =
    structure.onChildValue(new BsonInt32(value.toInt))

  override def writeInt(value: Int): Unit =
    structure.onChildValue(new BsonInt32(value))

  override def writeFloat(value: Float): Unit =
    structure.onChildValue(new BsonDouble(value))

  override def writeDouble(value: Double): Unit =
    structure.onChildValue(new BsonDouble(value))

  override def writeLong(value: Long): Unit =
    structure.onChildValue(new BsonInt64(value))

  override def writeBoolean(value: Boolean): Unit =
    structure.onChildValue(new BsonBoolean(value))

  override def writeFalse(): Unit =
    structure.onChildValue(new BsonBoolean(false))

  override def writeTrue(): Unit =
    structure.onChildValue(new BsonBoolean(true))

  override def writeNull(): Unit =
    structure.onChildValue(BsonNull())

  override def writeChar(value: Char): Unit =
    structure.onChildValue(new BsonString(value.toString))

}
