package co.blocke.scalajack
package mongo
package typeadapter

import co.blocke.scalajack.typeadapter.ClassLikeTypeAdapter

import scala.reflect.runtime.universe
import scala.reflect.runtime.currentMirror
import scala.reflect.runtime.universe.{ TypeTag, typeOf }

object MongoCaseClassTypeAdapter extends TypeAdapterFactory {

  val IdMemberName: MemberName = "_id"

  override def typeAdapterOf[T](next: TypeAdapterFactory)(implicit context: Context, tt: TypeTag[T]): TypeAdapter[T] =
    next.typeAdapterOf[T] match {
      case realClassTypeAdapter: ClassLikeTypeAdapter[T] =>
        val memberNameTypeAdapter = context.typeAdapterOf[MemberName]

        type RealClass = T

        val membersOfRealClass = realClassTypeAdapter.fieldMembers
        val numberOfRealMembers = membersOfRealClass.length

        val (keyMembersOfRealClass, nonKeyMembersOfRealClass) = membersOfRealClass.partition(_.annotationOf[DBKey].isDefined)
        keyMembersOfRealClass match {
          case Nil =>
            // Easy!
            realClassTypeAdapter

          case keyMemberOfRealClass :: Nil =>
            type SyntheticClass = Array[Any]
            type MemberOfSyntheticClass = ClassLikeTypeAdapter.FieldMember[SyntheticClass]

            val idMemberOfSyntheticClass = new MemberOfSyntheticClass {

              override type Value = keyMemberOfRealClass.Value

              override def index = 0

              override def name = IdMemberName

              override def defaultValue =
                keyMemberOfRealClass.defaultValue

              override def valueIn(instanceOfSyntheticClass: SyntheticClass): Value =
                instanceOfSyntheticClass(index).asInstanceOf[Value]

              override def readValue(reader: Reader): Value =
                keyMemberOfRealClass.readValue(reader)

              override def writeValue(value: Value, writer: Writer): Unit =
                keyMemberOfRealClass.writeValue(value, writer)

              override def annotationOf[A](implicit tt: TypeTag[A]): Option[universe.Annotation] =
                keyMemberOfRealClass.annotationOf[A]

              override def isStringValue: Boolean =
                keyMemberOfRealClass.isStringValue

              override def valueTypeTag: TypeTag[Value] =
                keyMemberOfRealClass.valueTypeTag

            }

            val nonIdMembersOfSyntheticClass = for ((memberOfRealClass, i) <- nonKeyMembersOfRealClass.zipWithIndex) yield new MemberOfSyntheticClass {

              override type Value = memberOfRealClass.Value

              override def index = 1 + i

              override def name =
                memberOfRealClass.name

              override def defaultValue =
                memberOfRealClass.defaultValue

              override def valueIn(instanceOfSyntheticClass: SyntheticClass): Value =
                instanceOfSyntheticClass(index).asInstanceOf[Value]

              override def readValue(reader: Reader): Value =
                memberOfRealClass.readValue(reader)

              override def writeValue(value: Value, writer: Writer): Unit =
                memberOfRealClass.writeValue(value, writer)

              override def annotationOf[A](implicit tt: TypeTag[A]): Option[universe.Annotation] =
                memberOfRealClass.annotationOf[A]

              override def isStringValue: Boolean =
                memberOfRealClass.isStringValue

              override def valueTypeTag: TypeTag[Value] =
                memberOfRealClass.valueTypeTag

            }

            val membersOfSyntheticClass = idMemberOfSyntheticClass :: nonIdMembersOfSyntheticClass
            val membersOfSyntheticClassByName = membersOfSyntheticClass.map(member => member.name -> member).toMap

            val syntheticClassTypeAdapter = new ClassLikeTypeAdapter[SyntheticClass] {

              override def typeMembers: List[TypeMember] = Nil

              override def typeMember(memberName: MemberName): Option[TypeMember] = None

              override def fieldMembers: List[FieldMember] =
                membersOfSyntheticClass

              override def fieldMember(memberName: MemberName): Option[FieldMember] =
                membersOfSyntheticClassByName.get(memberName)

              override def readMemberName(reader: Reader): MemberName =
                memberNameTypeAdapter.read(reader)

              override def writeMemberName(memberName: MemberName, writer: Writer): Unit =
                memberNameTypeAdapter.write(memberName, writer)

              override def instantiate(memberValuesOfSyntheticClass: Array[Any]): SyntheticClass =
                memberValuesOfSyntheticClass

            }

            new TypeAdapter[RealClass] {

              override def read(reader: Reader): RealClass =
                syntheticClassTypeAdapter.read(reader) match {
                  case null =>
                    null.asInstanceOf[RealClass]

                  case instanceOfSyntheticClass =>
                    val memberValuesOfSyntheticClass = instanceOfSyntheticClass

                    val memberValuesOfRealClass = new Array[Any](numberOfRealMembers)

                    memberValuesOfRealClass(keyMemberOfRealClass.index) = memberValuesOfSyntheticClass(0)

                    for ((memberOfRealClass, i) <- nonKeyMembersOfRealClass.zipWithIndex) {
                      memberValuesOfRealClass(memberOfRealClass.index) = memberValuesOfSyntheticClass(1 + i)
                    }

                    realClassTypeAdapter.instantiate(memberValuesOfRealClass)
                }

              override def write(instanceOfRealClass: RealClass, writer: Writer): Unit =
                if (instanceOfRealClass == null) {
                  writer.writeNull()
                } else {
                  val memberValuesOfSyntheticClass = new Array[Any](1 + nonKeyMembersOfRealClass.length)
                  memberValuesOfSyntheticClass(0) = keyMemberOfRealClass.valueIn(instanceOfRealClass)

                  for ((memberOfRealClass, i) <- nonKeyMembersOfRealClass.zipWithIndex) {
                    memberValuesOfSyntheticClass(1 + i) = memberOfRealClass.valueIn(instanceOfRealClass)
                  }

                  val instanceOfSyntheticClass: SyntheticClass = memberValuesOfSyntheticClass

                  syntheticClassTypeAdapter.write(instanceOfSyntheticClass, writer)
                }

            }

          case allKeyMembers =>

            // Now it gets tricky...
            // We are effectively inventing two fictional classes. One class represents the "_id" field. The other represents
            // an alternative form of the original class-like type adapter.

            type SyntheticId = Array[Any]
            type SyntheticClass = Array[Any]

            type MemberOfSyntheticId = ClassLikeTypeAdapter.FieldMember[SyntheticId]
            type MemberOfSyntheticClass = ClassLikeTypeAdapter.FieldMember[SyntheticClass]

            val idTypeAdapter = {
              val membersOfSyntheticId: List[MemberOfSyntheticId] = for ((memberOfRealClass, i) <- allKeyMembers.zipWithIndex) yield new MemberOfSyntheticId {

                override type Value = memberOfRealClass.Value

                override def index = i

                override def name = memberOfRealClass.name

                override def defaultValue = memberOfRealClass.defaultValue

                override def valueIn(syntheticId: SyntheticId): Value =
                  syntheticId(index).asInstanceOf[Value]

                override def readValue(reader: Reader): Value =
                  memberOfRealClass.readValue(reader)

                override def writeValue(value: Value, writer: Writer): Unit =
                  memberOfRealClass.writeValue(value, writer)

                override def annotationOf[A](implicit tt: TypeTag[A]): Option[universe.Annotation] =
                  memberOfRealClass.annotationOf[A]

                override def isStringValue: Boolean =
                  memberOfRealClass.isStringValue

                override def valueTypeTag: TypeTag[Value] =
                  memberOfRealClass.valueTypeTag

              }

              val membersOfSyntheticIdByName = membersOfSyntheticId.map(member => member.name -> member).toMap

              new ClassLikeTypeAdapter[SyntheticId] {

                override def typeMembers: List[TypeMember] = Nil

                override def typeMember(memberName: MemberName): Option[TypeMember] = None

                override def fieldMembers: List[FieldMember] =
                  membersOfSyntheticId

                override def fieldMember(memberName: MemberName): Option[FieldMember] =
                  membersOfSyntheticIdByName.get(memberName)

                override def readMemberName(reader: Reader): MemberName =
                  memberNameTypeAdapter.read(reader)

                override def writeMemberName(memberName: MemberName, writer: Writer): Unit =
                  memberNameTypeAdapter.write(memberName, writer)

                override def instantiate(memberValues: Array[Any]): SyntheticId =
                  memberValues

              }
            }

            val idMemberOfSyntheticClass = new MemberOfSyntheticClass {

              override type Value = SyntheticId

              override def index = 0

              override def name = IdMemberName

              override def defaultValue = None

              override def valueIn(instanceOfSyntheticClass: SyntheticClass): Value =
                instanceOfSyntheticClass(index).asInstanceOf[Value]

              override def readValue(reader: Reader): Value =
                idTypeAdapter.read(reader)

              override def writeValue(value: Value, writer: Writer): Unit =
                idTypeAdapter.write(value, writer)

              override def annotationOf[A](implicit tt: TypeTag[A]): Option[universe.Annotation] = None

              override def isStringValue: Boolean = false

              override def valueTypeTag: TypeTag[SyntheticId] =
                TypeTags.of(currentMirror, typeOf[Nothing])

            }

            val nonIdMembersOfSyntheticClass = for ((memberOfRealClass, i) <- nonKeyMembersOfRealClass.zipWithIndex) yield new MemberOfSyntheticClass {

              override type Value = memberOfRealClass.Value

              override def index = 1 + i

              override def name =
                memberOfRealClass.name

              override def defaultValue =
                memberOfRealClass.defaultValue

              override def valueIn(instanceOfSyntheticClass: SyntheticClass): Value =
                instanceOfSyntheticClass(index).asInstanceOf[Value]

              override def readValue(reader: Reader): Value =
                memberOfRealClass.readValue(reader)

              override def writeValue(value: Value, writer: Writer): Unit =
                memberOfRealClass.writeValue(value, writer)

              override def annotationOf[A](implicit tt: TypeTag[A]): Option[universe.Annotation] =
                memberOfRealClass.annotationOf[A]

              override def isStringValue: Boolean =
                memberOfRealClass.isStringValue

              override def valueTypeTag: TypeTag[Value] =
                memberOfRealClass.valueTypeTag

            }

            val membersOfSyntheticClass = idMemberOfSyntheticClass :: nonIdMembersOfSyntheticClass
            val membersOfSyntheticClassByName = membersOfSyntheticClass.map(member => member.name -> member).toMap

            val syntheticClassTypeAdapter = new ClassLikeTypeAdapter[SyntheticClass] {

              override def typeMembers: List[TypeMember] = Nil

              override def typeMember(memberName: MemberName): Option[TypeMember] = None

              override def fieldMembers: List[FieldMember] =
                membersOfSyntheticClass

              override def fieldMember(memberName: MemberName): Option[FieldMember] =
                membersOfSyntheticClassByName.get(memberName)

              override def readMemberName(reader: Reader): MemberName =
                memberNameTypeAdapter.read(reader)

              override def writeMemberName(memberName: MemberName, writer: Writer): Unit =
                memberNameTypeAdapter.write(memberName, writer)

              override def instantiate(memberValuesOfSyntheticClass: Array[Any]): SyntheticClass =
                memberValuesOfSyntheticClass

            }

            new TypeAdapter[RealClass] {

              override def read(reader: Reader): RealClass =
                syntheticClassTypeAdapter.read(reader) match {
                  case null =>
                    null.asInstanceOf[RealClass]

                  case instanceOfSyntheticClass =>
                    val memberValuesOfSyntheticClass = instanceOfSyntheticClass // Surprise! They're one and the same!

                    val syntheticId = memberValuesOfSyntheticClass(0).asInstanceOf[SyntheticId]

                    val memberValuesOfRealClass = new Array[Any](numberOfRealMembers)

                    for ((memberOfRealClass, i) <- keyMembersOfRealClass.zipWithIndex) {
                      memberValuesOfRealClass(memberOfRealClass.index) = syntheticId(i)
                    }

                    for ((memberOfRealClass, i) <- nonKeyMembersOfRealClass.zipWithIndex) {
                      memberValuesOfRealClass(memberOfRealClass.index) = memberValuesOfSyntheticClass(1 + i)
                    }

                    realClassTypeAdapter.instantiate(memberValuesOfRealClass)
                }

              override def write(instanceOfRealClass: RealClass, writer: Writer): Unit =
                if (instanceOfRealClass == null) {
                  writer.writeNull()
                } else {
                  val syntheticId = new Array[Any](keyMembersOfRealClass.length)

                  for ((memberOfRealClass, i) <- keyMembersOfRealClass.zipWithIndex) {
                    syntheticId(i) = memberOfRealClass.valueIn(instanceOfRealClass)
                  }

                  val memberValuesOfSyntheticClass = new Array[Any](1 + nonKeyMembersOfRealClass.length)
                  memberValuesOfSyntheticClass(0) = syntheticId

                  for ((memberOfRealClass, i) <- nonKeyMembersOfRealClass.zipWithIndex) {
                    memberValuesOfSyntheticClass(1 + i) = memberOfRealClass.valueIn(instanceOfRealClass)
                  }

                  val instanceOfSyntheticClass: SyntheticClass = memberValuesOfSyntheticClass

                  syntheticClassTypeAdapter.write(instanceOfSyntheticClass, writer)
                }

            }
        }

      case other =>
        other
    }

}
