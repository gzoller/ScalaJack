package co.blocke.scalajack.typeadapter

import co.blocke.scalajack.{Reader, TokenType, TypeAdapter, Writer, _}

import scala.annotation.Annotation
import scala.collection.mutable

import scala.reflect.runtime.universe.TypeTag

object ClassLikeTypeAdapter {

  trait Member[Owner] {

    type Value

    def index: Int

    def name: MemberName

    def defaultValue: Option[Value]

    def valueIn(owner: Owner): Value

    def readValue(reader: Reader): Value

    def writeValue(value: Value, writer: Writer): Unit

    def annotation[A <: Annotation](implicit tt: TypeTag[A]): Option[A]

  }

}

trait ClassLikeTypeAdapter[C >: Null] extends TypeAdapter[C] {

  type Member = ClassLikeTypeAdapter.Member[C]

  def members: List[Member]

  def member(memberName: MemberName): Option[Member]

  def readMemberName(reader: Reader): MemberName

  def writeMemberName(memberName: MemberName, writer: Writer): Unit

  def instantiate(memberValues: Array[Any]): C

  override def read(reader: Reader): C =
    reader.peek match {
      case TokenType.BeginObject =>
        reader.beginObject()

        val membersByName = members.map(member => member.name -> member).toMap

        val numberOfMembers = members.size
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

        for (member <- members if !found(member.index)) {
          memberValues(member.index) = member.defaultValue.getOrElse(throw new RuntimeException(s"No default value for ${member.name}"))
        }

        instantiate(memberValues)

      case TokenType.Null =>
        reader.readNull()
    }

  override def write(instanceOfClass: C, writer: Writer): Unit =
    if (instanceOfClass == null) {
      writer.writeNull()
    } else {
      writer.beginObject()

      for (member <- members) {
        writeMemberName(member.name, writer)

        val value = member.valueIn(instanceOfClass)
        member.writeValue(value, writer)
      }

      writer.endObject()
    }

}
