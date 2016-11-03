package co.blocke.scalajack
package mongo
package typeadapter

import co.blocke.scalajack.typeadapter.ClassLikeTypeAdapter

class CompositeKeyTypeAdapter(
    memberNameTypeAdapter: TypeAdapter[MemberName],
    override val members:  List[ClassLikeTypeAdapter.Member[Array[Any]]]
) extends ClassLikeTypeAdapter[Array[Any]] {

  val membersByName = members.map(member => member.name -> member).toMap

  override def member(memberName: MemberName): Option[Member] =
    membersByName.get(memberName)

  override def readMemberName(reader: Reader): MemberName =
    memberNameTypeAdapter.read(reader)

  override def writeMemberName(memberName: MemberName, writer: Writer): Unit =
    memberNameTypeAdapter.write(memberName, writer)

  override def instantiate(memberValues: Array[Any]): Array[Any] =
    memberValues

}
