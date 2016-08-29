package co.blocke.scalajack.flexjson.typeadapter

import co.blocke.scalajack.flexjson.{Context, Reader, TypeAdapter, TypeAdapterFactory, Writer}

import scala.reflect.runtime.universe.Type

object PolymorphicTypeAdapter extends TypeAdapterFactory {

  override def typeAdapter(tpe: Type, context: Context): Option[TypeAdapter[_]] =
    if (tpe.typeSymbol.isClass) {
      val classSymbol = tpe.typeSymbol.asClass
      if (classSymbol.isTrait) {
        Some(PolymorphicTypeAdapter("_hint", context.typeAdapterOf[Type], context))
      } else {
        None
      }
    } else {
      None
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

  override def write(value: T, writer: Writer): Unit = ???

}
