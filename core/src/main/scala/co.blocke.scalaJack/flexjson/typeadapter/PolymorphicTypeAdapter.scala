package co.blocke.scalajack.flexjson.typeadapter

import co.blocke.scalajack.flexjson.{Context, ForwardingWriter, Reader, TypeAdapter, TypeAdapterFactory, Writer}

import scala.reflect.runtime.universe.{ClassSymbol, Type}

object PolymorphicTypeAdapter extends TypeAdapterFactory.FromClassSymbol {

  override def typeAdapter(tpe: Type, classSymbol: ClassSymbol, context: Context): Option[TypeAdapter[_]] =
    if (classSymbol.isTrait) {
      Some(PolymorphicTypeAdapter("_hint", context.typeAdapterOf[Type], context))
    } else {
      None
    }

}

class PolymorphicWriter(override val delegate: Writer,
                        typeFieldName: String,
                        tpe: Type,
                        typeTypeAdapter: TypeAdapter[Type]) extends ForwardingWriter {

  var depth = 0

  override def beginObject(): Unit = {
    depth += 1
    super.beginObject()

    if (depth == 1) {
      writeName(typeFieldName)
      typeTypeAdapter.write(tpe, this)
    }
  }

  override def endObject(): Unit = {
    depth -= 1
    super.endObject()
  }

}

case class PolymorphicTypeAdapter[T](typeFieldName: String,
                                     typeTypeAdapter: TypeAdapter[Type],
                                     context: Context) extends TypeAdapter[T] {

  override def read(reader: Reader): T = {
    val originalPosition = reader.position

    reader.beginObject()

    var optionalConcreteType: Option[Type] = None

    while (optionalConcreteType.isEmpty && reader.hasMoreFields) {
      val fieldName = reader.readIdentifier()

      if (fieldName == typeFieldName) {
        val concreteType = typeTypeAdapter.read(reader)
        optionalConcreteType = Some(concreteType)
      } else {
        reader.skipValue()
      }
    }

    val concreteType = optionalConcreteType.getOrElse(throw new Exception(s"""Could not find type field named "$typeFieldName" """))

    val concreteTypeAdapter = context.typeAdapter(concreteType)

    reader.position = originalPosition

    concreteTypeAdapter.read(reader).asInstanceOf[T]
  }

  override def write(value: T, writer: Writer): Unit = {
    ???
  }

}
