package co.blocke.scalajack.flexjson.typeadapter

import co.blocke.scalajack.flexjson.FlexJsonFlavor.MemberName
import co.blocke.scalajack.flexjson.{ Context, ForwardingWriter, Reader, TypeAdapter, TypeAdapterFactory, Writer }

import scala.reflect.runtime.currentMirror
import scala.reflect.runtime.universe.{ ClassSymbol, Type }

case class PolymorphicTypeAdapterFactory(hintFieldName: String) extends TypeAdapterFactory.FromClassSymbol {

  override def typeAdapter(tpe: Type, classSymbol: ClassSymbol, context: Context): Option[TypeAdapter[_]] =
    if (classSymbol.isTrait) {
      Some(PolymorphicTypeAdapter(hintFieldName, context.typeAdapterOf[Type], context.typeAdapterOf[MemberName], context))
    } else {
      None
    }

}

class PolymorphicWriter(
    override val delegate: Writer,
    typeFieldName:         String,
    tpe:                   Type,
    typeTypeAdapter:       TypeAdapter[Type],
    memberNameTypeAdapter: TypeAdapter[MemberName]
) extends ForwardingWriter {

  var depth = 0

  override def beginObject(): Unit = {
    depth += 1
    super.beginObject()

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

case class PolymorphicTypeAdapter[T](
    typeMemberName:        MemberName,
    typeTypeAdapter:       TypeAdapter[Type],
    memberNameTypeAdapter: TypeAdapter[MemberName],
    context:               Context
) extends TypeAdapter[T] {

  override def read(reader: Reader): T = {
    val originalPosition = reader.position

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

    val concreteType = optionalConcreteType.getOrElse(throw new Exception(s"""Could not find type field named "$typeMemberName" """))

    val concreteTypeAdapter = context.typeAdapter(concreteType)

    reader.position = originalPosition

    concreteTypeAdapter.read(reader).asInstanceOf[T]
  }

  override def write(value: T, writer: Writer): Unit = {
    // TODO figure out a better way to infer the type (perhaps infer the type arguments?)
    val valueType = currentMirror.classSymbol(value.getClass).info
    val valueTypeAdapter = context.typeAdapter(valueType).asInstanceOf[TypeAdapter[T]]

    val polymorphicWriter = new PolymorphicWriter(writer, typeMemberName, valueType, typeTypeAdapter, memberNameTypeAdapter)
    valueTypeAdapter.write(value, polymorphicWriter)
  }

}
