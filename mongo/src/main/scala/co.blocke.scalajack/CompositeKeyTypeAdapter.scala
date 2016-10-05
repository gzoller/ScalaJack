package co.blocke.scalajack

import co.blocke.scalajack.typeadapter.ClassLikeTypeAdapter
import co.blocke.scalajack.typeadapter.ClassLikeTypeAdapter.Member

import scala.collection.immutable.ListMap

class CompositeKeyTypeAdapter extends ClassLikeTypeAdapter[Array[Any]] {

  override def members: List[Member] = ???

  override def memberNameTypeAdapter: TypeAdapter[MemberName] = ???

  override def instantiate(memberValues: Array[Any]): Array[Any] = memberValues

}
