package co.blocke.scalajack
package msgpack

import scala.reflect.runtime.currentMirror
import scala.reflect.runtime.universe.{ ClassSymbol, Type, appliedType, typeOf }
import scala.collection.mutable.{ Map => MMap }
import org.msgpack.MessagePack

case class MsgPackPolymorphicTypeAdapterFactory(hintFieldName: String) extends TypeAdapterFactory.FromClassSymbol {

  override def typeAdapter(tpe: Type, classSymbol: ClassSymbol, context: Context): Option[TypeAdapter[_]] =
    if (classSymbol.isTrait) {
      Some(MsgPackPolymorphicTypeAdapter(hintFieldName, context.typeAdapterOf[Type], context.typeAdapterOf[MemberName], context, tpe))
    } else {
      None
    }

}

class MsgPackPolymorphicWriter(
    typeFieldName:         String,
    tpe:                   Type,
    typeTypeAdapter:       TypeAdapter[Type],
    memberNameTypeAdapter: TypeAdapter[MemberName]
) extends MsgPackWriterLike {

  var depth = 0
  val msgpack = new MessagePack()
  val out = new java.io.ByteArrayOutputStream()

  override def beginObject(size: Int): Unit = {
    depth += 1
    super.beginObject(size + 1)

    if (depth == 1) {
      memberNameTypeAdapter.write(typeFieldName, this)
      typeTypeAdapter.write(tpe, this)
    }
  }

  override def endObject(): Unit = {
    depth -= 1
    super.endObject()
  }

}

object MsgPackPolymorphicTypeAdapter {
  private val resolved: MMap[(Type, List[Type]), List[Type]] = MMap.empty[(Type, List[Type]), List[Type]]
}

case class MsgPackPolymorphicTypeAdapter[T](
    typeMemberName:        MemberName,
    typeTypeAdapter:       TypeAdapter[Type],
    memberNameTypeAdapter: TypeAdapter[MemberName],
    context:               Context,
    polymorphicType:       Type
) extends TypeAdapter[T] {

  import scala.collection.mutable

  val populatedConcreteTypeCache = new mutable.WeakHashMap[Type, Type]

  def populateConcreteType(concreteType: Type): Type =
    populatedConcreteTypeCache.getOrElseUpdate(concreteType, Reflection.populateChildTypeArgs(polymorphicType, concreteType))

  override def read(reader: Reader): T = {
    if (reader.peek == TokenType.Null) {
      reader.readNull().asInstanceOf[T]
    } else {
      val savedPosition = reader.position

      reader.beginObject()

      var optionalConcreteType: Option[Type] = None

      while (optionalConcreteType.isEmpty && reader.hasMoreMembers) {
        val memberName = memberNameTypeAdapter.read(reader)

        if (memberName == typeMemberName) {
          val concreteType = typeTypeAdapter.read(reader)
          optionalConcreteType = Some(concreteType)
        } else {
          reader.skipValue()
        }
      }

      val concreteType = optionalConcreteType.getOrElse(throw new java.lang.IllegalStateException(s"""Could not find type field named "$typeMemberName"\n""" + reader.showError()))
      val populatedConcreteType = populateConcreteType(concreteType)
      val concreteTypeAdapter = context.typeAdapter(populatedConcreteType)

      reader.position = savedPosition

      concreteTypeAdapter.read(reader).asInstanceOf[T]
    }
  }

  override def write(value: T, writer: Writer): Unit = {
    // TODO figure out a better way to infer the type (perhaps infer the type arguments?)
    if (value == null) {
      writer.writeNull()
    } else {
      val concreteType = currentMirror.classSymbol(value.getClass).toType
      val populatedConcreteType = populateConcreteType(concreteType)
      val valueTypeAdapter = context.typeAdapter(populatedConcreteType).asInstanceOf[TypeAdapter[T]]

      val polymorphicWriter = new MsgPackPolymorphicWriter(typeMemberName, populatedConcreteType, typeTypeAdapter, memberNameTypeAdapter)
      valueTypeAdapter.write(value, polymorphicWriter)
      writer.asInstanceOf[MsgPackWriterLike].dumpBytes(polymorphicWriter.out.toByteArray)
    }
  }

}
