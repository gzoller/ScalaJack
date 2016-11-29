package co.blocke.scalajack.typeadapter

import co.blocke.scalajack.{ MemberName, Reader, TokenType, TypeAdapter, Writer }

import scala.collection.mutable
import scala.reflect.runtime.universe
import scala.reflect.runtime.universe.TypeTag

object ClassLikeTypeAdapter {

  sealed trait Member[Owner] {

    def name: MemberName

  }

  trait TypeMember[Owner] extends Member[Owner]

  trait FieldMember[Owner] extends Member[Owner] {

    type Value

    def valueTypeTag: TypeTag[Value]

    def index: Int

    def defaultValue: Option[Value]

    def valueIn(owner: Owner): Value

    def readValue(reader: Reader): Value

    def writeValue(value: Value, writer: Writer): Unit

    def annotationOf[A](implicit tt: TypeTag[A]): Option[universe.Annotation]

    def isStringValue: Boolean

  }

}

trait ClassLikeTypeAdapter[C] extends TypeAdapter[C] {

  type TypeMember = ClassLikeTypeAdapter.TypeMember[C]
  type FieldMember = ClassLikeTypeAdapter.FieldMember[C]

  def members = typeMembers ++ fieldMembers

  def typeMembers: List[TypeMember]

  def typeMember(memberName: MemberName): Option[TypeMember]

  def fieldMembers: List[FieldMember]

  def fieldMember(memberName: MemberName): Option[FieldMember]

  def readMemberName(reader: Reader): MemberName

  def writeMemberName(memberName: MemberName, writer: Writer): Unit

  def instantiate(fieldMemberValues: Array[Any]): C

  // $COVERAGE-OFF$Not used for JSON (Mongo)
  override def read(reader: Reader): C =
    reader.peek match {
      case TokenType.BeginObject =>
        reader.beginObject()

        val membersByName = fieldMembers.map(member => member.name -> member).toMap

        val numberOfMembers = fieldMembers.size
        val memberValues = new Array[Any](numberOfMembers)
        val found = new mutable.BitSet(numberOfMembers)

        while (reader.hasMoreMembers) {
          val memberName = readMemberName(reader)
          membersByName.get(memberName) match {
            case Some(member) =>
              val memberValue = member.readValue(reader)
              memberValues(member.index) = memberValue
              found(member.index) = true

            case None =>
              reader.skipValue()
          }
        }

        reader.endObject()

        for (member <- fieldMembers if !found(member.index)) {
          memberValues(member.index) = member.defaultValue.getOrElse(throw new RuntimeException(s"No default value for ${member.name}"))
        }

        instantiate(memberValues)

      case TokenType.Null =>
        reader.readNull().asInstanceOf[C]
    }

  override def write(instanceOfClass: C, writer: Writer): Unit =
    if (instanceOfClass == null) {
      writer.writeNull()
    } else {
      writer.beginObject()

      for (member <- fieldMembers) {
        writeMemberName(member.name, writer)

        val value = member.valueIn(instanceOfClass)
        member.writeValue(value, writer)
      }

      writer.endObject()
    }
  // $COVERAGE-ON$

}
