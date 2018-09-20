package co.blocke.scalajack
package typeadapter

import scala.reflect.runtime.universe.Annotation

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

    def valueIn(owner: TypeTagged[Owner]): TypeTagged[Value]

    def deserializeValueFromNothing[J](path: Path)(implicit ops: JsonOps[J]): DeserializationResult[Value]

    def deserializeValue[J](path: Path, json: J)(implicit ops: JsonOps[J]): DeserializationResult[Value]

    def serializeValue[J](tagged: TypeTagged[Value])(implicit ops: JsonOps[J]): SerializationResult[J]

    def annotationOf[A](implicit tt: TypeTag[A]): Option[Annotation]

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
  def instantiate(fieldMemberValues: Array[Any]): C
}
