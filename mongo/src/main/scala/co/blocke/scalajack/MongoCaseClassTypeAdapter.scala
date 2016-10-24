package co.blocke.scalajack

import co.blocke.scalajack.typeadapter.ClassLikeTypeAdapter
import co.blocke.scalajack.typeadapter.ClassLikeTypeAdapter.Member

import scala.annotation.Annotation
import scala.reflect.runtime.universe.{Type, TypeTag}

object MongoCaseClassTypeAdapter extends TypeAdapterFactory {

  override def typeAdapter(tpe: Type, context: Context, next: TypeAdapterFactory): TypeAdapter[_] = {
    next.typeAdapter(tpe, context) match {
      case realClassTypeAdapter: ClassLikeTypeAdapter[_] =>
        val memberNameTypeAdapter = context.typeAdapterOf[MemberName]

        type RealClass = AnyRef

        val membersOfRealClass = realClassTypeAdapter.members.map(_.asInstanceOf[Member[RealClass]])
        val numberOfRealMembers = membersOfRealClass.length

        val (keyMembersOfRealClass, nonKeyMembersOfRealClass) = membersOfRealClass.partition(_.annotation[DBKey].isDefined)
        keyMembersOfRealClass match {
          case Nil =>
            // Easy!
            realClassTypeAdapter

          case onlyKeyMember :: Nil =>
            ???

          case allKeyMembers =>

            // Now it gets tricky...
            // We are effectively inventing two fictional classes. One class represents the "_id" field. The other represents
            // an alternative form of the original class-like type adapter.

            type SyntheticClass = Array[Any]
            type SyntheticId = Array[Any]

            val idTypeAdapter = new CompositeKeyTypeAdapter(memberNameTypeAdapter, for ((memberOfRealClass, i) <- allKeyMembers.zipWithIndex) yield
              new ClassLikeTypeAdapter.Member[SyntheticId] {

                override type Value = memberOfRealClass.Value

                override def index = i

                override def name = memberOfRealClass.name

                override def defaultValue = memberOfRealClass.defaultValue

                override def valueIn(syntheticId: SyntheticId): Value = syntheticId(i).asInstanceOf[Value]

                override def readValue(reader: Reader): Value = memberOfRealClass.readValue(reader)

                override def writeValue(value: Value, writer: Writer): Unit = memberOfRealClass.writeValue(value, writer)

                override def annotation[A <: Annotation](implicit tt: TypeTag[A]): Option[A] = memberOfRealClass.annotation[A]

              })

            val idMemberOfSyntheticClass = new ClassLikeTypeAdapter.Member[SyntheticClass] {

              override type Value = SyntheticId

              override def index = 0

              override def name = "_id"

              override def defaultValue = None

              override def valueIn(instanceOfSyntheticClass: SyntheticClass): Value =
                instanceOfSyntheticClass(0).asInstanceOf[Value]

              override def readValue(reader: Reader): Value =
                idTypeAdapter.read(reader)

              override def writeValue(value: Value, writer: Writer): Unit =
                idTypeAdapter.write(value, writer)

              override def annotation[A <: Annotation](implicit tt: TypeTag[A]): Option[A] = None

            }

            val nonIdMembersOfSyntheticClass = for ((memberOfRealClass, i) <- nonKeyMembersOfRealClass.zipWithIndex) yield
              new ClassLikeTypeAdapter.Member[SyntheticClass] {

                override type Value = memberOfRealClass.Value

                override def index = i + 1

                override def name =
                  memberOfRealClass.name

                override def defaultValue =
                  memberOfRealClass.defaultValue

                override def valueIn(instanceOfSyntheticClass: SyntheticClass): Value =
                  instanceOfSyntheticClass(i + 1).asInstanceOf[Value]

                override def readValue(reader: Reader): Value =
                  memberOfRealClass.readValue(reader)

                override def writeValue(value: Value, writer: Writer): Unit =
                  memberOfRealClass.writeValue(value, writer)

                override def annotation[A <: Annotation](implicit tt: TypeTag[A]): Option[A] =
                  memberOfRealClass.annotation[A]

              }

            val membersOfSyntheticClass = idMemberOfSyntheticClass :: nonIdMembersOfSyntheticClass
            val membersOfSyntheticClassByName = membersOfSyntheticClass.map(member => member.name -> member).toMap

            val syntheticClassTypeAdapter = new ClassLikeTypeAdapter[SyntheticClass] {

              override def members: List[Member] = membersOfSyntheticClass

              override def member(memberName: MemberName): Option[Member] =
                membersOfSyntheticClassByName.get(memberName)

              override def readMemberName(reader: Reader): MemberName =
                memberNameTypeAdapter.read(reader)

              override def writeMemberName(memberName: MemberName, writer: Writer): Unit =
                memberNameTypeAdapter.write(memberName, writer)

              override def instantiate(memberValuesOfSyntheticClass: Array[Any]): SyntheticClass =
                memberValuesOfSyntheticClass

            }

            val t: TypeAdapter[RealClass] = new TypeAdapter[RealClass] {

              override def read(reader: Reader): RealClass =
                syntheticClassTypeAdapter.read(reader) match {
                  case null =>
                    null

                  case instanceOfSyntheticClass =>
                    val memberValuesOfSyntheticClass = instanceOfSyntheticClass

                    val syntheticId = memberValuesOfSyntheticClass(0).asInstanceOf[SyntheticId]

                    val memberValuesOfRealClass = new Array[Any](numberOfRealMembers)

                    for ((memberOfRealClass, i) <- keyMembersOfRealClass.zipWithIndex) {
                      memberValuesOfRealClass(memberOfRealClass.index) = syntheticId(i)
                    }

                    for ((memberOfRealClass, i) <- nonKeyMembersOfRealClass.zipWithIndex) {
                      memberValuesOfRealClass(memberOfRealClass.index) = memberValuesOfSyntheticClass(1 + i)
                    }

                    realClassTypeAdapter.instantiate(memberValuesOfRealClass).asInstanceOf[RealClass]
                }

              override def write(instanceOfRealClass: RealClass, writer: Writer): Unit = {
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

            ??? // FIXME return the actual type adapter
        }

      case other =>
        other
    }
  }

}
