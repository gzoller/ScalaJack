package co.blocke.scalajack
package mongo

import org.bson.{ BsonArray, BsonBinary, BsonBoolean, BsonDateTime, BsonDocument, BsonDouble, BsonInt32, BsonInt64, BsonMaxKey, BsonMinKey, BsonObjectId, BsonString, BsonTimestamp, BsonUndefined, BsonValue }
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

    def end(): BsonValue

  }

  object RootStructure extends Structure {

    override val parent: Structure = null
    var value: BsonValue = _

    override def beginChildValue(): Unit = {}

    override def endChildValue(childValue: BsonValue): Unit = {
      value = childValue
    }

    override def end(): BsonValue = value

  }

  class ObjectStructure(override val parent: Structure) extends Structure {

    val document = new BsonDocument

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

    override def end(): BsonValue = {
      val keys = document.keySet

      val value =
        if (keys contains "$date") {
          new BsonDateTime(document.getNumber("$date").longValue)
        } else if (keys contains "$minKey") {
          new BsonMinKey
        } else if (keys contains "$maxKey") {
          new BsonMaxKey
        } else if (keys contains "$oid") {
          new BsonObjectId(new org.bson.types.ObjectId(document.getString("$oid").getValue))
        } else {
          document
        }

      value
    }

  }

  class ArrayStructure(override val parent: Structure) extends Structure {

    val array = new BsonArray

    override def beginChildValue(): Unit = {

    }

    override def endChildValue(childValue: BsonValue): Unit = {
      if (childValue != null) {
        array.add(childValue)
      }
    }

    override def end(): BsonArray = array

  }

  var structure: Structure = RootStructure

  override def beginObject(): Unit = {
    structure.beginChildValue()
    structure = new ObjectStructure(structure)
  }

  override def endObject(): Unit = {
    val value = structure.end() // TODO expected structure type

    structure = structure.parent
    structure.endChildValue(value)
  }

  override def beginArray(): Unit = {
    structure.beginChildValue()
    structure = new ArrayStructure(structure)
  }

  override def endArray(): Unit = {
    val value = structure.end() // TODO expected structure type

    structure = structure.parent
    structure.endChildValue(value)
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
