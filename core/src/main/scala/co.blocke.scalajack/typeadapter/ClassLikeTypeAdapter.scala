package co.blocke.scalajack.typeadapter

import co.blocke.scalajack.{ Reader, TokenType, TypeAdapter, Writer, _ }

import scala.collection.mutable

object ClassLikeTypeAdapter {

  trait Member {

    type OwnerType
    type MemberValueType

    val index: Int

    val name: MemberName

    val valueTypeAdapter: TypeAdapter[MemberValueType]

    def defaultValue: Option[MemberValueType]

    def valueIn(owner: OwnerType): MemberValueType

    def writeValue(memberValue: MemberValueType, writer: Writer): Unit = {
      valueTypeAdapter.write(memberValue, writer)
    }

  }

  //  case class ConcreteMember2(index: Int, name: MemberName, valueTypeAdapter: TypeAdapter[Any]) extends Member2 {
  //
  //    override type OwnerType = Any
  //    override type MemberValueType = Any
  //
  //    override def defaultValue: Option[MemberValueType] = ???
  //
  //    override def valueIn(owner: OwnerType): MemberValueType = ???
  //
  //  }

  trait MemberOld[C, V] {

    def index: Int

    def name: MemberName

    def valueIn(instanceOfClass: C): V

    def valueTypeAdapter: TypeAdapter[V]

    def defaultValue: Option[V]

  }

}

trait ClassLikeTypeAdapter[C >: Null] extends TypeAdapter[C] {

  def memberNameTypeAdapter: TypeAdapter[MemberName]

  def members: List[ClassLikeTypeAdapter.Member]

  def instantiate(memberValues: Array[Any]): C

  override def read(reader: Reader): C =
    reader.peek match {
      case TokenType.Null ⇒
        reader.readNull()

      case TokenType.BeginObject ⇒
        reader.beginObject()

        val membersByName = members.map(member ⇒ member.name → member).toMap

        val numberOfMembers = members.size
        val memberValues = new Array[Any](numberOfMembers)
        val found = new mutable.BitSet(numberOfMembers)

        while (reader.hasMoreMembers) {
          val memberName = memberNameTypeAdapter.read(reader)
          membersByName.get(memberName) match {
            case Some(member) ⇒
              val memberValue = member.valueTypeAdapter.read(reader)
              memberValues(member.index) = memberValue
              found(member.index) = true

            case None ⇒
              reader.skipValue()
          }
        }

        reader.endObject()

        for (member ← members if !found(member.index)) {
          memberValues(member.index) = member.defaultValue.getOrElse(throw new RuntimeException(s"No default value for ${member.name}"))
        }

        instantiate(memberValues)
    }

  override def write(instanceOfClass: C, writer: Writer): Unit =
    if (instanceOfClass == null) {
      writer.writeNull()
    } else {
      writer.beginObject()

      val membersByName = members.map(member ⇒ member.name → member).toMap

      for (member ← membersByName.values) {
        val value = member.valueIn(instanceOfClass.asInstanceOf[member.OwnerType])
        memberNameTypeAdapter.write(member.name, writer)
        member.valueTypeAdapter.asInstanceOf[TypeAdapter[Any]].write(value, writer)
      }

      writer.endObject()
    }

}
